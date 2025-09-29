#include <glint/ast.hh>
#include <glint/parser.hh>

#include <lcc/context.hh>
#include <lcc/format.hh>
#include <lcc/target.hh>

#include <pybind11/pybind11.h>

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

PythonDeclInfo findDeclImpl(lcc::glint::Module& m, lcc::Context* ctx, std::string_view name, lcc::glint::Expr* e) {
    if (e->name() == name) {
        return {
            py_location(ctx, {e->location(), e->type()->location()}),
            py_location(ctx, e->location()),
            py_location(ctx, e->type()->location()),
            *m.ToSource(*e->type())
        };
    }
    for (auto* c : e->children()) {
        if (auto l = findDeclImpl(m, ctx, name, c); not invalid(l))
            return l;
    }

    return {};
}

PythonDeclInfo findDecl(std::string source, std::string name) {
    // TODO: Parse source. Return location of declaration matching symbol.
    lcc::Context context{
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

    auto& f = context.create_file(
        "tmp",
        std::vector<char>{source.begin(), source.end()}
    );

    auto m = lcc::glint::Parser::Parse(
        &context,
        f
    );

    return findDeclImpl(*m, &context, name, m->top_level_function()->body());
}

std::vector<std::string> getCompletions() {
    // TODO: Parse source up until the incomplete member access (ideally,
    // exactly to the dot).
    return {};
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

    py::class_<PythonDeclInfo>(m, "DeclInfo")
        .def(py::init<int, int, int, int>())
        .def_readwrite("location", &PythonDeclInfo::location)
        .def_readwrite("decl_location", &PythonDeclInfo::decl_location)
        .def_readwrite("type_location", &PythonDeclInfo::type_location)
        .def_readwrite("type_representation", &PythonDeclInfo::type_representation);

    m.def(
        "findDecl",
        &findDecl,
        "Return the location of the declaration of the given symbol within the given source (if any)."
    );

    // Expose the 'add' function to Python
    m.def(
        "getCompletions",
        &getCompletions,
        "Return a list of valid completions at point. Currently just handles member access at exactly the dot."
    );
}
