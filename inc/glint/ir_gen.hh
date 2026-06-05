#ifndef GLINT_IR_GEN_HH
#define GLINT_IR_GEN_HH

#include <glint/ast.hh>
#include <lcc/context.hh>
#include <lcc/ir/core.hh>
#include <lcc/ir/module.hh>
#include <lcc/utils/rtti.hh>

#include <memory>
#include <unordered_map>
#include <vector>

namespace lcc::glint {

class IRGen {
    Context* ctx;
    glint::Module& glint_module;
    lcc::Module* ir_module;

    lcc::Function* function{nullptr};
    lcc::Block* block{nullptr};

    usz total_block = 0;
    usz total_while = 0;
    usz total_for = 0;
    usz total_if = 0;
    usz total_string = 0;

    void update_block(lcc::Block* new_block) {
        function->append_block(new_block);
        block = new_block;
        total_block += 1;
    }

    std::unordered_map<glint::Expr*, lcc::Value*> generated_ir;
    struct LoopBlocks {
        lcc::Block* entry{};
        lcc::Block* exit{};
    };
    std::unordered_map<glint::Expr*, LoopBlocks> loop_info;
    std::vector<lcc::GlobalVariable*> string_literals;

    IRGen(Context* c, glint::Module& m)
        : ctx(c)
        , glint_module(m) {
        ir_module = new lcc::Module(ctx);
    }

    void insert(lcc::Inst* inst);

    void generate_expression(glint::Expr*);

    void create_function(glint::FuncDecl* f);
    void generate_function(glint::FuncDecl*);

public:
    auto mod() -> lcc::Module* { return ir_module; }

    static auto Generate(Context*, glint::Module&) -> lcc::Module*;
};

} // namespace lcc::glint

#endif /* GLINT_IR_GEN_HH */
