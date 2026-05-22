#include <language_c/irgen.hh>

#include <lcc/context.hh>
#include <lcc/core.hh>
#include <lcc/ir/core.hh>
#include <lcc/ir/module.hh>
#include <lcc/ir/type.hh>

#include <language_c/ast.hh>

namespace lcc::language_c {

lcc::Type* IRGen::convert(const Type* t) {
    switch (t->kind()) {
        case TypeKind::Int:
        case TypeKind::Pointer:
        case TypeKind::Function:
        case TypeKind::Array: {
            Diag::ICE("TODO: {} type irgen conversion", *t);
        }
        case TypeKind::Invalid:
        case TypeKind::Count: break;
    }
    Diag::ICE("unreachable");
}

void IRGen::create_function(Declaration* d) {
    generated_ir[d] = new (*ir_module) lcc::Function(
        ir_module,
        std::string(d->name()),
        as<lcc::FunctionType>(convert(d->type())),
        Linkage::Exported,
        CallConv::C,
        {}
    );
}

auto IRGen::Generate(Context* context, TranslationUnit& tu) -> lcc::Module* {
    IRGen irgen{context};

    return irgen.ir_module;
}

} // namespace lcc::language_c
