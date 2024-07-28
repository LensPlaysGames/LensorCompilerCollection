#include <lcc/context.hh>
#include <lcc/ir/type.hh>

#include <limits>
#include <mutex>
#include <utility>

lcc::Context::Context(
    const Target* target,
    const Format* format,
    const lcc::Context::Options& options
) : _options(options),
    _target(target),
    _format(format) {
    static std::once_flag once;
    std::call_once(once, InitialiseLCCData);

    /// Initialise type caches.
    integer_types[1] = Type::I1Ty;
}

lcc::Context::~Context() {
    for (auto* type : array_types) delete type;
    for (auto* type : function_types) delete type;
    for (auto* type : struct_types) delete type;
    for (auto [_, type] : integer_types)
        if (type != Type::I1Ty)
            delete type;
}

auto lcc::Context::get_or_load_file(fs::path path) -> File& {
    auto f = rgs::find_if(owned_files, [&](const auto& e) {
        return e->path() == path;
    });
    if (f != owned_files.end()) return **f;

    /// Load the file.
    auto contents = File::LoadFileData(path);
    return make_file(std::move(path), std::move(contents));
}

auto lcc::Context::make_file(fs::path name, std::vector<char>&& contents) -> File& {
    /// Create the file.
    auto* fptr = new File(
        *this,
        std::move(name),
        std::move(contents)
    );
    fptr->_id = u32(owned_files.size());
    LCC_ASSERT(fptr->_id <= std::numeric_limits<u16>::max());
    owned_files.emplace_back(fptr);
    return *fptr;
}
