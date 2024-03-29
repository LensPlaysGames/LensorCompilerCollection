#ifndef LAYE_IR_GEN_HH
#define LAYE_IR_GEN_HH

#include <laye/ast.hh>
#include <lcc/context.hh>
#include <lcc/ir/ir.hh>
#include <lcc/utils/rtti.hh>
#include <lcc/ir/module.hh>
#include <memory>
#include <unordered_map>

namespace lcc::laye {

class IRGen {
    LayeContext* _laye_context;
    laye::Module* _laye_module;

    lcc::Context* _ctx;
    lcc::Module* _mod;

    std::unordered_map<SemaNode*, lcc::Value*> _ir_values;
    std::unordered_map<const SemaNode*, lcc::Type*> _ir_types;
    //std::vector<lcc::GlobalVariable*> string_literals;
    StringMap<lcc::GlobalVariable*> string_literals;

    lcc::Function* curr_func{nullptr};
    lcc::Block* curr_block{nullptr};

    usz total_block = 0;
    usz total_for = 0;
    usz total_if = 0;

    usz total_string = 0;

    void UpdateBlock(lcc::Block* new_block) {
        curr_func->append_block(new_block);
        curr_block = new_block;
        total_block += 1;
    }

    IRGen(LayeContext* laye_context, laye::Module* laye_module)
        : _laye_context(laye_context), _laye_module(laye_module) {
        _ctx = laye_context->context();
        _mod = new lcc::Module(laye_context->context(), laye_module->file()->path().string());
    }

    void Insert(lcc::Inst* inst) {
        LCC_ASSERT(inst, "Invalid argument");
        LCC_ASSERT(_ctx, "Invalid context");
        LCC_ASSERT(curr_func && curr_block, "Invalid insert point");
        curr_block->insert(inst);
    }

    lcc::Type* Convert(const lcc::laye::Type* in);

    void GenerateModule(Module* module);
    void CreateStructDeclType(StructDecl* decl);
    void CreateIRFunctionValue(FunctionDecl* decl);
    void GenerateIRFunctionBody(FunctionDecl* decl);
    void GenerateStatement(Statement* statement);
    lcc::Value* GenerateExpression(Expr* expr);
    void EnsureBoolIsI1(lcc::laye::Expr* expr, lcc::Value*& value);

public:
    auto laye_context() const { return _laye_context; }
    auto context() const { return _laye_context->context(); }

    auto mod() const { return _mod; }
    auto laye_mod() const { return _laye_module; }

    static auto Generate(LayeContext* laye_context, laye::Module* laye_module) -> lcc::Module*;
};

};

#endif // LAYE_IR_GEN_HH
