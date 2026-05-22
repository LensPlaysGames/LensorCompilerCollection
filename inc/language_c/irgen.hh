#ifndef LANGUAGE_C_IRGEN_HH
#define LANGUAGE_C_IRGEN_HH

#include <lcc/context.hh>
#include <lcc/core.hh>
#include <lcc/ir/core.hh>
#include <lcc/ir/module.hh>
#include <lcc/ir/type.hh>

#include <language_c/ast.hh>

namespace lcc::language_c {

class IRGen {
    lcc::Context* context{};
    lcc::Module* ir_module{};
    std::unordered_map<Node*, lcc::Value*> generated_ir{};

    void create_function(Declaration* d);

    lcc::Type* convert(const Type* t);

public:
    IRGen(lcc::Context* context_)
        : context(context_),
          ir_module(new lcc::Module(context)) {}

    static auto Generate(Context*, TranslationUnit&) -> lcc::Module*;
};

} // namespace lcc::language_c

#endif /* LANGUAGE_C_IRGEN_HH */
