#include <language_c/irgen.hh>

#include <lcc/context.hh>
#include <lcc/core.hh>
#include <lcc/diags.hh>
#include <lcc/ir/core.hh>
#include <lcc/ir/module.hh>
#include <lcc/ir/type.hh>
#include <lcc/target.hh>

#include <language_c/ast.hh>
#include <language_c/parser.hh>

namespace lcc::language_c {

lcc::Type* IRGen::convert(const Type* t) {
    switch (t->kind()) {
        case TypeKind::Int: {
            return lcc::IntegerType::Get(
                context,
                context->target()->ffi.size_of_int
            );
        }

        case TypeKind::Void:
            return lcc::Type::VoidTy;

        case TypeKind::Pointer:
            return lcc::Type::PtrTy;

        case TypeKind::Function: {
            const auto* f = (FunctionType*) t;

            std::vector<lcc::Type*> parameter_types{};
            for (auto p : f->parameters())
                parameter_types.emplace_back(convert(p.type));

            return lcc::FunctionType::Get(
                context,
                convert(f->return_type()),
                parameter_types
            );
        }

        case TypeKind::Array: {
            Diag::ICE("TODO: {} type irgen conversion", *t);
        }

        case TypeKind::Invalid:
        case TypeKind::Count: break;
    }
    Diag::ICE("unreachable");
}

void IRGen::insert(lcc::Inst* inst) {
    if (not inst or not context)
        Diag::ICE("nullptr argument");
    if (not insert_context.function or not insert_context.block)
        Diag::ICE("Invalid insert context: nullptr encountered");
    insert_context.block->insert(inst);
}

void IRGen::create_function(const Declaration* d) {
    generated_ir[d] = new (*ir_module) lcc::Function(
        ir_module,
        std::string(d->name()),
        as<lcc::FunctionType>(convert(d->type())),
        d->initialising_expression() ? Linkage::Exported : Linkage::Imported,
        CallConv::C,
        {}
    );
}

void IRGen::generate_expression(const Node* n) {
    // Only ever generate once.
    if (generated_ir.contains(n)) return;

    switch (n->kind()) {
        case NodeKind::Group: {
            auto g = (Group*) n;
            for (auto c : g->constituents())
                generate_expression(c);
            return;
        }
        case NodeKind::Block: {
            auto b = (Block*) n;
            for (auto c : b->constituents())
                generate_expression(c);
            return;
        }

        case NodeKind::NameReference: {
            Diag::ICE("Handle name-reference irgen...");
        }

        case NodeKind::Declaration: {
            Diag::ICE("Handle declaration irgen...");
        }

        case NodeKind::IntegerLiteral: {
            const auto* i = (IntegerLiteral*) n;
            generated_ir[n] = new (*ir_module) lcc::IntegerConstant(
                IntegerType::Get(context, context->target()->ffi.size_of_int),
                i->value()
            );
            return;
        }

        case NodeKind::Return: {
            const auto* r = (Return*) n;
            if (r->expression()) {
                generate_expression(r->expression());
                auto inst = new (*ir_module) lcc::ReturnInst(
                    generated_ir[r->expression()]
                );
                generated_ir[n] = inst;
                insert(inst);
                return;
            }
            auto inst = new (*ir_module) lcc::ReturnInst(nullptr);
            generated_ir[n] = inst;
            insert(inst);
            return;
        }

        case NodeKind::BinaryOperation: {
            const auto* b = (BinaryOperation*) n;

            generate_expression(b->lhs());
            generate_expression(b->rhs());
#define BINARY_ARGS generated_ir[b->lhs()], generated_ir[b->rhs()], b->location()

            Inst* inst{};
            switch (b->binary_operator()) {
                case TokenKind::OpPlus:
                    inst = new (*ir_module) lcc::AddInst(BINARY_ARGS);
                    break;
                case TokenKind::OpMinus:
                    inst = new (*ir_module) lcc::SubInst(BINARY_ARGS);
                    break;
                case TokenKind::OpAsterisk:
                    inst = new (*ir_module) lcc::MulInst(BINARY_ARGS);
                    break;
                case TokenKind::OpSlash:
                    inst = new (*ir_module) lcc::SDivInst(BINARY_ARGS);
                    break;
                case TokenKind::OpPercent:
                    inst = new (*ir_module) lcc::SRemInst(BINARY_ARGS);
                    break;

                case TokenKind::Invalid:
                case TokenKind::Identifier:
                case TokenKind::Integer:
                case TokenKind::Fractional:
                case TokenKind::KwVoid:
                case TokenKind::KwInt:
                case TokenKind::KwReturn:
                case TokenKind::OpComma:
                case TokenKind::LeftParenthesis:
                case TokenKind::RightParenthesis:
                case TokenKind::LeftSquareBracket:
                case TokenKind::RightSquareBracket:
                case TokenKind::LeftCurlyBrace:
                case TokenKind::RightCurlyBrace:
                case TokenKind::Semicolon:
                case TokenKind::Eof:
                case TokenKind::Count:
                    Diag::ICE("unreachable");
            }
#undef BINARY_ARGS
            LCC_ASSERT(inst, "Binary operation should have created instruction");
            generated_ir[n] = inst;
            insert(inst);
            return;
        }

        case NodeKind::Invalid:
        case NodeKind::Count: break;
    }
    Diag::ICE("unreachable");
}

void IRGen::generate_function(const Declaration* d) {
    insert_context.function = as<lcc::Function>(generated_ir[d]);
    if (d->initialising_expression()) {
        update_block(
            new (*ir_module) lcc::Block(
                fmt::format("{}.body", d->name())
            )
        );
        generate_expression(d->initialising_expression());
    }
}

auto IRGen::Generate(Context* context, TranslationUnit& tu) -> lcc::Module* {
    IRGen irgen{context};

    for (auto function : tu.functions)
        irgen.create_function(function);

    for (auto function : tu.functions)
        irgen.generate_function(function);

    return irgen.ir_module;
}

} // namespace lcc::language_c
