#include <glint/ast.hh>
#include <glint/parser.hh>
#include <glint/sema.hh>

#include <lcc/context.hh>
#include <lcc/format.hh>
#include <lcc/location.hh>
#include <lcc/target.hh>
#include <lcc/utils.hh>

#include <pybind11/complex.h>
#include <pybind11/pybind11.h>
#include <pybind11/stl.h>

#include <memory>
#include <optional>
#include <string>
#include <unordered_set>
#include <vector>

/// Default target.
const lcc::Target* default_target =
#if defined(LCC_PLATFORM_WINDOWS)
    lcc::Target::x86_64_windows;
#elif defined(__APPLE__) or defined(__linux__)
    lcc::Target::x86_64_linux;
#else
#    error "Unsupported target"
#endif

/// Default format
const lcc::Format* default_format = lcc::Format::gnu_as_att_assembly;

struct PythonLocation {
    // absolute in file
    int byte_offset{-1};
    int length{-1};
    // calculated line and character position
    int line{-1};
    int character{-1};
};

bool invalid(const PythonLocation& p) {
    return p.byte_offset < 0;
}

PythonLocation py_location(lcc::Context* ctx, lcc::Location loc) {
    auto e_locinfo = loc.seek_line_column(ctx);
    return {
        .byte_offset = int(loc.pos),
        .length = int(loc.len),
        .line = int(e_locinfo.line),
        .character = int(e_locinfo.col)
    };
}

struct PythonDeclInfo {
    PythonLocation location{};
    PythonLocation decl_location{};
    PythonLocation type_location{};
    std::string type_representation{};
};

bool invalid(const PythonDeclInfo& p) {
    return invalid(p.decl_location);
}

struct DeclInfo {
    PythonDeclInfo py_info{};
    lcc::glint::Expr* decl{nullptr};
};

bool invalid(const DeclInfo& d) {
    return d.decl == nullptr;
}

constexpr auto default_context() {
    return lcc::Context{
        default_target,
        default_format,
        lcc::Context::Options{
            lcc::Context::DoNotUseColour,
            lcc::Context::DoNotPrintAST,
            lcc::Context::DoNotStopatLex,
            lcc::Context::DoNotStopatSyntax,
            lcc::Context::DoNotStopatSema,
            lcc::Context::DoNotPrintMIR,
            lcc::Context::DoNotStopatMIR
        }
    };
}

constexpr auto get_analysed_module(lcc::Context& context, std::string source) -> std::optional<std::unique_ptr<lcc::glint::Module>> {
    auto& f = context.create_file(
        "glintd.tmp.g",
        std::vector<char>{source.begin(), source.end()}
    );

    auto m = lcc::glint::Parser::Parse(
        &context,
        f
    );
    // If we don't have a module at all, we have parsed no information.
    if (not m) return {};

    // Replace named types with what they were before, etc
    lcc::glint::Sema::Analyse(&context, *m);

    // NOTE: Ownership released.
    return m;
}

struct PythonToken {
    std::string source{};
    PythonLocation location{};
    // TODO: Bind TokenKind Enum to Python and include that information here.
};

auto tokenize(std::string source) -> std::vector<PythonToken> {
    auto context = default_context();

    auto& f = context.create_file(
        "glintd.tmp.g",
        std::vector<char>{source.begin(), source.end()}
    );

    std::vector<PythonToken> out{};
    for (auto t : lcc::glint::Parser::GetTokens(&context, f)) {
        auto source = lcc::glint::ToSource(t);
        if (source) {
            out.emplace_back(
                *source,
                py_location(&context, t.location)
            );
        }
    }
    return out;
}

// Walks children of expression until name matches.
DeclInfo findDeclImpl(lcc::glint::Module& m, lcc::Context* ctx, std::string_view name, lcc::glint::Expr* e) {
    if (not e) return {};

    if (e->name() == name) {
        // spans entire declaration and type
        PythonLocation location = py_location(ctx, e->location());
        // spans just the declaration
        const PythonLocation decl_location = py_location(ctx, e->location());
        // spans just the type
        PythonLocation type_location{};
        std::string type_repr{};

        if (e->type()) {
            location = py_location(ctx, {e->location(), e->type()->location()});
            type_location = py_location(ctx, e->type()->location());

            auto maybe_type_repr = m.ToSource(*e->type());
            if (maybe_type_repr) type_repr = *maybe_type_repr;
        }
        return {
            {//
             location,
             decl_location,
             type_location,
             type_repr
            },
            e
        };
    }

    // Walk children recursively until we find a name that matches.
    for (auto* c : e->children()) {
        LCC_ASSERT(c != e, "Infinite Recursion");
        if (auto l = findDeclImpl(m, ctx, name, c); not invalid(l))
            return l;
    }

    return {};
}

// Parse source, and return location of first declaration matching name
// parameter.
PythonDeclInfo findDecl(std::string source, std::string name) {
    if (source.empty()) return {};
    if (name.empty()) return {};

    // If the given name isn't a valid Glint identifier, there is no
    // declaration.
    if (not lcc::glint::Lexer::IsIdentStart((lcc::u32) name.at(0)))
        return {};

    auto context = default_context();
    auto maybe_m = get_analysed_module(context, source);
    if (not maybe_m) return {};
    auto& m = *maybe_m;
    LCC_ASSERT(m);

    return findDeclImpl(*m, &context, name, m->top_level_function()->body()).py_info;
}

std::vector<std::string> getValidTypeConstituents(lcc::glint::Type* t) {
    LCC_ASSERT(t);
    switch (t->kind()) {
        case lcc::glint::Type::Kind::Builtin:
        case lcc::glint::Type::Kind::FFIType:
        case lcc::glint::Type::Kind::Pointer:
        case lcc::glint::Type::Kind::Reference:
        case lcc::glint::Type::Kind::Array:
        case lcc::glint::Type::Kind::Function:
        case lcc::glint::Type::Kind::Integer:
        case lcc::glint::Type::Kind::Named:
            return {};

        case lcc::glint::Type::Kind::ArrayView:
            return getValidTypeConstituents(lcc::as<lcc::glint::ArrayViewType>(t)->struct_type());

        case lcc::glint::Type::Kind::DynamicArray:
            return getValidTypeConstituents(lcc::as<lcc::glint::DynamicArrayType>(t)->struct_type());

        case lcc::glint::Type::Kind::Sum:
            return getValidTypeConstituents(lcc::as<lcc::glint::SumType>(t)->struct_type());

        case lcc::glint::Type::Kind::Union: {
            auto u = lcc::as<lcc::glint::UnionType>(t);
            std::vector<std::string> out{};
            out.reserve(u->members().size());
            for (const auto& m : u->members())
                out.emplace_back(m.name);

            return out;
        }

        case lcc::glint::Type::Kind::Enum: {
            auto e = lcc::as<lcc::glint::EnumType>(t);
            std::vector<std::string> out{};
            out.reserve(e->enumerators().size());
            for (auto d : e->enumerators())
                out.emplace_back(d->name());

            return out;
        }

        case lcc::glint::Type::Kind::Struct: {
            auto s = lcc::as<lcc::glint::StructType>(t);
            std::vector<std::string> out{};
            out.reserve(s->members().size());
            for (const auto& m : s->members())
                out.emplace_back(m.name);

            return out;
        }
    }
    LCC_UNREACHABLE();
}

enum class ScopeFilter {
    ExcludeGenerated = 0,
    IncludeAll
};
constexpr auto extract_scope_symbols(
    lcc::glint::Scope* scope,
    ScopeFilter filter = ScopeFilter::ExcludeGenerated
) -> std::vector<std::string> {
    LCC_ASSERT(scope);
    std::unordered_set<std::string> out{};
    for (auto d : scope->all_symbols()) {
        auto n = d->name();

        // Skip mangled names, and temporary names inserted by compiler (like for
        // ranged for loops).
        if (filter == ScopeFilter::ExcludeGenerated and n.starts_with("_XGlint")) continue;

        out.emplace(d->name());
    }
    return {out.begin(), out.end()};
}

std::vector<std::string> getValidSymbols(std::string source) {
    auto context = default_context();
    auto maybe_m = get_analysed_module(context, source);
    if (not maybe_m) return {};
    auto& m = *maybe_m;
    LCC_ASSERT(m);

    std::unordered_set<std::string> all_valid_symbols{};
    for (auto s : m->scopes) {
        auto syms = extract_scope_symbols(s);
        for (auto sym : syms)
            all_valid_symbols.emplace(sym);
    }

    return {all_valid_symbols.cbegin(), all_valid_symbols.cend()};
}

auto getScopes(std::string source) -> std::vector<std::vector<std::string>> {
    auto context = default_context();
    auto maybe_m = get_analysed_module(context, source);
    if (not maybe_m) return {};
    auto& m = *maybe_m;
    LCC_ASSERT(m);

    std::vector<std::vector<std::string>> out{};
    for (auto s : m->scopes)
        out.emplace_back(extract_scope_symbols(s));

    return out;
}

auto getScopeAtPoint(std::string source, PythonLocation location) -> std::vector<std::vector<std::string>> {
    if (invalid(location)) return {};

    auto context = default_context();
    auto maybe_m = get_analysed_module(context, source);
    if (not maybe_m) return {};
    auto& m = *maybe_m;
    LCC_ASSERT(m);

    auto l = lcc::Location{
        (lcc::u32) location.byte_offset,
        (lcc::u16) location.length,
        (lcc::u16) context.files().at(0)->file_id()
    };
    auto s = m->enclosing_scope(l);
    std::vector<std::vector<std::string>> out{};
    while (s) {
        out.emplace_back(extract_scope_symbols(s));
        s = s->parent();
    }
    return out;
}

namespace py = pybind11;

PYBIND11_MODULE(glinttools, m) {
    m.doc() = "Glint bindings for language server written in Python (because pygls is kewl).";

    py::class_<PythonLocation>(m, "Location")
        .def(py::init<int, int, int, int>())
        .def_readwrite("byte_offset", &PythonLocation::byte_offset)
        .def_readwrite("length", &PythonLocation::length)
        .def_readwrite("line", &PythonLocation::line)
        .def_readwrite("character", &PythonLocation::character);

    py::class_<PythonToken>(m, "Token")
        .def(py::init<std::string, PythonLocation>())
        .def_readwrite("source", &PythonToken::source)
        .def_readwrite("location", &PythonToken::location);

    py::class_<PythonDeclInfo>(m, "DeclInfo")
        .def(py::init<PythonLocation, PythonLocation, PythonLocation, std::string>())
        .def_readwrite("location", &PythonDeclInfo::location)
        .def_readwrite("decl_location", &PythonDeclInfo::decl_location)
        .def_readwrite("type_location", &PythonDeclInfo::type_location)
        .def_readwrite("type_representation", &PythonDeclInfo::type_representation);

    m.def(
        "findDecl",
        &findDecl,
        "Return the location of the declaration of the given symbol within the given source (if any)."
    );

    m.def(
        "getValidSymbols",
        &getValidSymbols,
        "Return all valid symbols within a given source (all symbols from all scopes)."
    );

    m.def(
        "getScopes",
        &getScopes,
        "Return a list of all scopes in a program."
    );

    m.def(
        "getScopeAtPoint",
        &getScopeAtPoint,
        "Given a valid source location, return a list of scopes containing the closest enclosing scope, and all parent scopes."
    );

    m.def(
        "tokenize",
        &tokenize,
        "Get a list of token objects."
    );
}
