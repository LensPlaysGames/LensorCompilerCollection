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
    std::unordered_map<const Node*, lcc::Value*> generated_ir{};

    struct InsertContext {
        lcc::Function* function{};
        lcc::Block* block{};
    } insert_context{};

    void create_function(const Declaration*);

    void insert(lcc::Inst*);

    lcc::Type* convert(const Type*);

    void generate_expression(const Node*);
    void generate_function(const Declaration*);

    void update_block(lcc::Block* new_block) {
        if (not insert_context.function)
            Diag::ICE("Invalid insert context: nullptr encountered");
        insert_context.function->append_block(new_block);
        insert_context.block = new_block;
    }

public:
    IRGen(lcc::Context* context_)
        : context(context_),
          ir_module(new lcc::Module(context)) {}

    static auto Generate(Context*, TranslationUnit&) -> lcc::Module*;
};

} // namespace lcc::language_c

#endif /* LANGUAGE_C_IRGEN_HH */
