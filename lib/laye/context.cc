#include <laye/ast.hh>
#include <lcc/context.hh>

namespace lcc::laye {

auto LayeContext::get_or_load_module(fs::path path) -> Module*
{
    auto canonical_path = fs::canonical(path);
    std::string canonical_name = canonical_path.string();

    auto module = lookup_module(canonical_name);
    if (module) return module;

    auto& file = context()->get_or_load_file(canonical_path);

    module = parse_laye_file(file);
    add_module(canonical_name, module);

    return module;
}

auto LayeContext::get_or_load_module(File& file) -> Module*
{
    auto canonical_path = fs::canonical(file.path());
    std::string canonical_name = canonical_path.string();

    auto module = lookup_module(canonical_name);
    if (module) return module;

    module = parse_laye_file(file);
    add_module(canonical_name, module);

    return module;
}

void Module::add_header(ModuleHeader* header) {
    _headers.push_back(header);
    if (auto import_header = cast<ImportHeader>(header)) {
        _imports.push_back(import_header);
    }
}

auto Module::lookup_import(const std::string& name, bool is_exported) const -> std::optional<ImportHeader*> {
    auto it = std::find_if(_imports.begin(), _imports.end(), [name, is_exported](auto m) {
        return (is_exported ? m->exported() : true) and m->alias() == name;
    });
    if (it == _imports.end())
        return std::nullopt;
    return *it;
}

auto Module::scope() -> Scope* {
    LCC_ASSERT(not scopes.empty());
    LCC_ASSERT(scopes[0]->parent() == nullptr);
    return scopes[0];
}

};
