#include <lcc/context.hh>
#include <lcc/diags.hh>
#include <lcc/file.hh>
#include <lcc/utils/macros.hh>

#ifndef _WIN32
#    include <fcntl.h>
#    include <sys/mman.h>
#    include <sys/stat.h>
#    include <sys/wait.h>
#    include <unistd.h>
#endif

#include <array>
#include <cstdio>
#include <cstring>
#include <filesystem>
#include <memory>
#include <random>
#include <ranges>
#include <string>
#include <string_view>
#include <thread>
#include <vector>

auto lcc::File::TempPath(std::string_view extension) -> fs::path {
    std::mt19937 rd(std::random_device{}());

    /// Get the temporary directory.
    auto tmp_dir = std::filesystem::temp_directory_path();

    /// Use the pid on Linux, and another random number on Windows.
#ifndef _WIN32
    auto pid = std::to_string(u32(::getpid()));
#else
    auto pid = std::to_string(rd());
#endif

    /// Get the current time and tid.
    auto now = chr::system_clock::now().time_since_epoch().count();
    auto tid = std::to_string(u32(std::hash<std::thread::id>{}(std::this_thread::get_id())));

    /// And some random letters too.
    /// Do NOT use `char` for this because it’s signed on some systems (including mine),
    /// which completely breaks the modulo operation below... Thanks a lot, C.
    std::array<u8, 8> rand{};
    rgs::generate(rand, [&] { return rd() % 26 + 'a'; });

    /// Create a unique file name.
    auto tmp_name = fmt::format(
        "{}.{}.{}.{}",
        pid,
        tid,
        now,
        std::string_view{(char*) rand.data(), rand.size()}
    );

    /// Append it to the temporary directory.
    auto f = tmp_dir / tmp_name;
    if (not extension.empty()) {
        if (not extension.starts_with('.')) f += '.';
        f += extension;
    }
    return f;
}

bool lcc::File::Write(const void* data, usz size, const fs::path& file) {
    auto f = std::fopen(file.string().c_str(), "wb");
    if (not f) return false;
    defer { std::fclose(f); };
    for (;;) {
        auto written = std::fwrite(data, 1, size, f);
        if (written == size) break;
        if (written < 1) return false;
        data = (char*) data + written;
        size -= written;
    }
    return true;
}

void lcc::File::WriteOrTerminate(const void* data, usz size, const fs::path& file) {
    if (not File::Write(data, size, file))
        Diag::Fatal("Failed to write to file '{}': {}", file.string(), std::strerror(errno));
}

lcc::File::File(Context& ctx, fs::path name, std::vector<char>&& contents)
    : ctx(ctx), file_path(std::move(name)), contents(std::move(contents)) {}

auto lcc::File::Read(const fs::path& path) -> std::vector<char> {
    return LoadFileData(path);
}

auto lcc::File::LoadFileData(const fs::path& path) -> std::vector<char> {
#ifdef __linux__
    /// Open the file.
    int fd = open(path.c_str(), O_RDONLY);
    if (fd == -1) Diag::Fatal("Could not open file \"{}\": {}", path.string(), strerror(errno));
    defer { close(fd); };

    /// Determine the file size.
    struct stat st {};
    if (fstat(fd, &st) == -1) Diag::Fatal("Could not stat file \"{}\": {}", path.string(), strerror(errno));

    /// If the file is empty, return an empty string.
    if (st.st_size == 0) return {};

    /// Map the file into memory.
    void* ptr = mmap(nullptr, static_cast<usz>(st.st_size), PROT_READ, MAP_PRIVATE, fd, 0);
    if (ptr == MAP_FAILED) Diag::Fatal("Could not map file \"{}\": {}", path.string(), strerror(errno));

    /// Copy the file into a vector.
    std::vector<char> ret(static_cast<usz>(st.st_size));
    memcpy(ret.data(), ptr, static_cast<usz>(st.st_size));

    /// Unmap the file.
    munmap(ptr, static_cast<usz>(st.st_size));

#else
    /// Read the file manually.
    std::unique_ptr<FILE, decltype(&std::fclose)> f{std::fopen(path.string().c_str(), "rb"), std::fclose};
    if (not f) Diag::Fatal("Could not open file \"{}\": {}", path.string(), strerror(errno));

    /// Get the file size.
    std::fseek(f.get(), 0, SEEK_END);
    auto sz = std::size_t(std::ftell(f.get()));
    std::fseek(f.get(), 0, SEEK_SET);

    /// Read the file.
    std::vector<char> ret;
    ret.resize(sz);
    std::size_t n_read = 0;
    while (n_read < sz) {
        errno = 0;
        auto n = std::fread(ret.data() + n_read, 1, sz - n_read, f.get());
        if (errno) Diag::Fatal("Error reading file \"{}\": {}", path.string(), strerror(errno));
        if (n == 0) break;
        n_read += n;
    }
#endif

    /// Construct the file data.
    return ret;
}
