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
    return p.byte_offset < 0 or p.length < 0 or p.line < 0 or p.character < 0;
}

PythonLocation py_location(lcc::Context* ctx, lcc::Location loc) {
    if (loc.seekable(ctx)) {
        auto e_locinfo = loc.seek_line_column(ctx);
        return {
            .byte_offset = int(loc.pos),
            .length = int(loc.len),
            .line = int(e_locinfo.line),
            .character = int(e_locinfo.col)
        };
    }
    return {};
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

struct PythonDiagnostic {
    enum class Severity {
        None,
        Note,
        Warning,
        Error,
        Fix,
        Fatal,
        ICE,
    } severity{Severity::None};
    PythonLocation where{};
    std::string message{};
};

bool invalid(const PythonDiagnostic& d) {
    return invalid(d.where);
}

struct PythonType {
    lcc::glint::Type::Kind kind{};
    std::string representation{};
    int size{};
    int align{};
    PythonLocation location{};
    bool valid{false};

    PythonType() : valid(false) {}

    PythonType(lcc::Context* context, lcc::glint::Type* type)
        : kind(type->kind()),
          representation(type->string()),
          size((int) type->size_in_bytes(context)),
          align((int) type->align(context)),
          location(py_location(context, type->location())),
          valid(true) {}
};

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

    auto out = findDeclImpl(*m, &context, name, m->top_level_function()->body()).py_info;
    LCC_ASSERT(m);
    return out;
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
        case lcc::glint::Type::Kind::Typeof:
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

struct PythonNode {
    lcc::glint::Expr::Kind kind;
    std::vector<PythonNode> children{};
    PythonType type{};
    PythonLocation location{};
    bool valid{false};

    PythonNode() : valid(false) {}

    PythonNode(lcc::Context* ctx, lcc::glint::Expr* e)
        : kind(e->kind()),
          type(ctx, e->type()),
          location(py_location(ctx, e->location())) {
        // Should never be null, but always good to check.
        if (not e) return;

        for (auto c : e->children())
            children.emplace_back(PythonNode{ctx, c});

        valid = true;
    }
};

PythonNode getTree(std::string source) {
    auto context = default_context();
    auto maybe_m = get_analysed_module(context, source);
    if (not maybe_m) return {};
    auto& m = *maybe_m;
    LCC_ASSERT(m);

    fmt::print(stderr, "You see this.\n");

    auto e = m->top_level_function()->body();

    fmt::print(stderr, "But, can you see this?\n");

    // If we just return, we risk unique_ptr `m` going out of scope...
    auto out = PythonNode(&context, e);
    LCC_ASSERT(m);
    return out;
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

auto getDiagnostics(std::string source) -> std::vector<PythonDiagnostic> {
    auto context = default_context();
    auto maybe_m = get_analysed_module(context, source);

    std::vector<PythonDiagnostic> out{};
    out.reserve(context.diagnostics().size());
    for (auto d : context.diagnostics()) {
        auto kind = PythonDiagnostic::Severity::None;
        switch (d.kind) {
            case lcc::Diag::Kind::None: break;
            case lcc::Diag::Kind::Note:
                kind = PythonDiagnostic::Severity::Note;
                break;
            case lcc::Diag::Kind::Warning:
                kind = PythonDiagnostic::Severity::Warning;
                break;
            case lcc::Diag::Kind::Error:
                kind = PythonDiagnostic::Severity::Error;
                break;
            case lcc::Diag::Kind::FError:
                kind = PythonDiagnostic::Severity::Fatal;
                break;
            case lcc::Diag::Kind::ICError:
                kind = PythonDiagnostic::Severity::ICE;
                break;
        }
        out.emplace_back(
            kind,
            py_location(&context, d.where),
            d.message
        );
    }

    return out;
}

// FIXME BUGGY Recursive AST search implementation
lcc::glint::Expr* getNodeAtPointImpl(
    lcc::glint::Module& mod,
    lcc::glint::Expr* expr,
    lcc::Location loc,
    lcc::glint::Expr* last_expr = nullptr
) {
    fmt::print(stderr, "getNodeAtPointImpl expr=");
    auto s = mod.ToSource(*expr);
    if (s) fmt::print(stderr, "{}\n", *s);
    else fmt::print(stderr, "FAIL\n");

    // If our search implementation did not change the selected node, return
    // the selected node.
    if (expr == last_expr) return expr;

    // Attempt to find closest child that precedes given location.
    lcc::glint::Expr* at_location = expr;
    for (auto c : expr->children()) {
        fmt::print(stderr, "  child pos={}\n", c->location().pos);
        if (not c->location().is_valid()) continue;
        // A valid position /past/ the one we are looking for.
        if (c->location().pos > loc.pos) break;
        // A valid position not yet past the one we are looking for. This means
        // 'c' is currently our closest child that precedes the given location, so
        // we update it.
        at_location = c;
    }

    // If the location we found is invalid, we haven't found any node at
    // point.
    // Also check that the found location spans far enough to make it to our
    // given location.
    if (
        (not at_location->location().is_valid())
        or at_location->location().pos > loc.pos
        or at_location->location().pos + at_location->location().len <= loc.pos
    ) return nullptr;

    // If the child we found has children that may be closer, search those...
    return getNodeAtPointImpl(mod, at_location, loc, expr);
}

lcc::glint::Expr* getNodeAtPoint(lcc::glint::Module& mod, lcc::Location loc) {
    fmt::print(stderr, "getNodeAtPoint loc.pos={}\n", loc.pos);
    return getNodeAtPointImpl(mod, mod.top_level_function()->body(), loc);
}

PythonType getTypeAtPoint(std::string source, PythonLocation location) {
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

    auto found = getNodeAtPoint(*m, l);
    if (not found) return {};

    return PythonType{&context, found->type()};
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

    py::enum_<PythonDiagnostic::Severity>(m, "DiagnosticSeverity")
        .value("None", PythonDiagnostic::Severity::None)
        .value("Note", PythonDiagnostic::Severity::Note)
        .value("Warning", PythonDiagnostic::Severity::Warning)
        .value("Error", PythonDiagnostic::Severity::Error)
        .value("Fix", PythonDiagnostic::Severity::Fix)
        .value("Fatal", PythonDiagnostic::Severity::Fatal)
        .value("ICE", PythonDiagnostic::Severity::ICE);

    py::class_<PythonDiagnostic>(m, "Diagnostic")
        .def(py::init<PythonDiagnostic::Severity, PythonLocation, std::string>())
        .def_readwrite("severity", &PythonDiagnostic::severity)
        .def_readwrite("location", &PythonDiagnostic::where)
        .def_readwrite("message", &PythonDiagnostic::message);

    py::enum_<lcc::glint::Type::Kind>(m, "TypeKind")
        .value("Builtin", lcc::glint::Type::Kind::Builtin)
        .value("FFIType", lcc::glint::Type::Kind::FFIType)
        .value("Named", lcc::glint::Type::Kind::Named)
        .value("Pointer", lcc::glint::Type::Kind::Pointer)
        .value("Reference", lcc::glint::Type::Kind::Reference)
        .value("DynamicArray", lcc::glint::Type::Kind::DynamicArray)
        .value("Array", lcc::glint::Type::Kind::Array)
        .value("ArrayView", lcc::glint::Type::Kind::ArrayView)
        .value("Function", lcc::glint::Type::Kind::Function)
        .value("Sum", lcc::glint::Type::Kind::Sum)
        .value("Union", lcc::glint::Type::Kind::Union)
        .value("Enum", lcc::glint::Type::Kind::Enum)
        .value("Struct", lcc::glint::Type::Kind::Struct)
        .value("Integer", lcc::glint::Type::Kind::Integer);

    py::class_<PythonType>(m, "Type")
        .def_readwrite("kind", &PythonType::kind)
        .def_readwrite("representation", &PythonType::representation)
        .def_readwrite("size", &PythonType::size)
        .def_readwrite("align", &PythonType::align)
        .def_readwrite("location", &PythonType::location);

    py::enum_<lcc::glint::Expr::Kind>(m, "ExprKind")
        .value("While", lcc::glint::Expr::Kind::While)
        .value("For", lcc::glint::Expr::Kind::For)
        .value("Return", lcc::glint::Expr::Kind::Return)
        .value("TypeDecl", lcc::glint::Expr::Kind::TypeDecl)
        .value("TypeAliasDecl", lcc::glint::Expr::Kind::TypeAliasDecl)
        .value("EnumeratorDecl", lcc::glint::Expr::Kind::EnumeratorDecl)
        .value("VarDecl", lcc::glint::Expr::Kind::VarDecl)
        .value("FuncDecl", lcc::glint::Expr::Kind::FuncDecl)
        .value("IntegerLiteral", lcc::glint::Expr::Kind::IntegerLiteral)
        .value("StringLiteral", lcc::glint::Expr::Kind::StringLiteral)
        .value("CompoundLiteral", lcc::glint::Expr::Kind::CompoundLiteral)
        .value("OverloadSet", lcc::glint::Expr::Kind::OverloadSet)
        .value("EvaluatedConstant", lcc::glint::Expr::Kind::EvaluatedConstant)
        .value("If", lcc::glint::Expr::Kind::If)
        .value("Block", lcc::glint::Expr::Kind::Block)
        .value("Call", lcc::glint::Expr::Kind::Call)
        .value("IntrinsicCall", lcc::glint::Expr::Kind::IntrinsicCall)
        .value("Cast", lcc::glint::Expr::Kind::Cast)
        .value("Unary", lcc::glint::Expr::Kind::Unary)
        .value("Binary", lcc::glint::Expr::Kind::Binary)
        .value("NameRef", lcc::glint::Expr::Kind::NameRef)
        .value("Type", lcc::glint::Expr::Kind::Type)
        .value("MemberAccess", lcc::glint::Expr::Kind::MemberAccess)
        .value("Module", lcc::glint::Expr::Kind::Module)
        .value("Match", lcc::glint::Expr::Kind::Match)
        .value("Sizeof", lcc::glint::Expr::Kind::Sizeof)
        .value("Alignof", lcc::glint::Expr::Kind::Alignof)
        .value("Template", lcc::glint::Expr::Kind::Template);

    py::class_<PythonNode>(m, "Node")
        .def_readwrite("kind", &PythonNode::kind)
        .def_readwrite("children", &PythonNode::children)
        .def_readwrite("type", &PythonNode::type)
        .def_readwrite("location", &PythonNode::location)
        .def_readwrite("valid", &PythonNode::valid);

    py::class_<PythonDeclInfo>(m, "DeclInfo")
        .def(py::init<PythonLocation, PythonLocation, PythonLocation, std::string>())
        .def_readwrite("location", &PythonDeclInfo::location)
        .def_readwrite("decl_location", &PythonDeclInfo::decl_location)
        .def_readwrite("type_location", &PythonDeclInfo::type_location)
        .def_readwrite("type_representation", &PythonDeclInfo::type_representation);

    m.def(
        "getTree",
        &getTree,
        "Return the typed AST."
    );

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

    m.def(
        "getDiagnostics",
        &getDiagnostics,
        "Get a list of diagnostics pertaining to the given source."
    );

    m.def(
        "getTypeAtPoint",
        &getTypeAtPoint,
        "Get the type of the closest preceding AST node to the given location."
    );
}
