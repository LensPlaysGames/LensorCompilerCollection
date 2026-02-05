#ifndef GLINT_IR_GEN_HH
#define GLINT_IR_GEN_HH

#include <glint/ast.hh>
#include <lcc/context.hh>
#include <lcc/ir/core.hh>
#include <lcc/ir/module.hh>
#include <lcc/utils/rtti.hh>

#include <memory>
#include <unordered_map>

namespace lcc::glint {

class IRGen {
    Context* ctx;
    glint::Module& int_module;
    lcc::Module* module;

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
    std::vector<lcc::GlobalVariable*> string_literals;

    IRGen(Context* c, glint::Module& m) : ctx(c), int_module(m) {
        module = new lcc::Module(ctx);
    }

    void insert(lcc::Inst* inst);

    void generate_expression(glint::Expr*);

    void create_function(glint::FuncDecl* f);
    void generate_function(glint::FuncDecl*);

    // If given decl is defined in the Glint module (linkage not imported),
    // AND the type of the decl is of dynamic array type,
    // insert initialization code, storing into given pointer.
    // Basically, generate the instruction that is the lvalue of the
    // declaration (i.e. GlobalVariable, AllocaInst, etc), then call this
    // function.
    void insert_dynarray_initialization(VarDecl*, Value* struct_pointer);

public:
    /// NOTE: I would name this module(), but C++ doesn't have properties.
    auto mod() -> lcc::Module* {
        return module;
    }

    static auto Generate(Context*, glint::Module&) -> lcc::Module*;
};

} // namespace lcc::glint

#endif /* GLINT_IR_GEN_HH */
