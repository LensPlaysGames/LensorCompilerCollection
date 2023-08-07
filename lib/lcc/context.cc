#include <lcc/context.hh>

auto lcc::Context::get_or_load_file(fs::path path) -> File& {
    auto f = rgs::find_if(owned_files, [&](const auto& e) { return e->path() == path; });
    if (f != owned_files.end()) return *f->get();

    /// Load the file.
    auto contents = File::LoadFileData(path);
    return make_file(std::move(path), std::move(contents));
}

auto lcc::Context::make_file(fs::path name, std::vector<char>&& contents) -> File& {
    /// Create the file.
    auto fptr = new File(*this, std::move(name), std::move(contents));
    fptr->id = u32(owned_files.size());
    owned_files.emplace_back(fptr);
    return *fptr;
}
