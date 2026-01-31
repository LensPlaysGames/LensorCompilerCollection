#include <lcc/file.hh>
#include <lcc/utils.hh>

#include <glint/ast.hh>
#include <glint/module_description.hh>
#include <glint/sema.hh>

#include <algorithm>
#include <cstring>
#include <iterator>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

lcc::glint::Module::Module(
    File* file,
    std::string module_name,
    Module::ModuleStatus is_logical_module
) : _name{std::move(module_name)},
    _is_module{is_logical_module},
    _file{file} //
{
    FuncType* ty{};

    /// Create the type of the top-level function.
    if (is_logical_module) {
        ty = new (*this) FuncType({}, BuiltinType::Void(*this), {}, {});
    } else {
        auto* cint_ty = FFIType::CInt(*this);

        auto* cchar_ty = FFIType::CChar(*this);
        auto* char_ptr = new (*this) PointerType{new (*this) PointerType{cchar_ty}};

        ty = new (*this) FuncType{
            {
                {"__argc__", cint_ty, {}},
                {"__argv__", char_ptr, {}},
                {"__envp__", char_ptr, {}},
            },

            /// We currently set main() to return `int` since that’s natural for
            /// most programs.
            Type::Int,
            {},
            {},
        };
    }

    // Don't mangle name of top level function.
    ty->set_attr(FuncAttr::NoMangle);

    Location loc{};
    if (file) {
        loc = Location{
            0,
            (decltype(Location::len)) file->size(),
            (decltype(Location::file_id)) file->file_id() //
        };
    }
    _top_level_function
        = new (*this) FuncDecl{
            is_logical_module ? InitFunctionName(name()) : "main",
            ty,
            new (*this) BlockExpr{{}, {}},
            nullptr,
            this,
            Linkage::Exported,
            loc,
            CallConv::C,
        };
}

lcc::glint::Module::~Module() {
    for (auto* node : nodes) delete node;
    for (auto* type : types) delete type;
    auto global_scope_ = global_scope();
    for (auto* scope : scopes) delete scope;
    for (auto& import_ref : _imports) {
        if (not import_ref.module)
            continue;
        // If global scope is shared, don't double free it.
        if (import_ref.module->global_scope() == global_scope_) {
            import_ref.module->scopes.erase(
                import_ref.module->scopes.cbegin()
            );
        }
        delete import_ref.module;
    }
}

void lcc::glint::Module::add_top_level_expr(Expr* node) {
    as<BlockExpr>(top_level_function()->body())->add(node);
}

auto lcc::glint::Module::intern(std::string_view str) -> usz {
    auto it = rgs::find(strings, str);
    if (it != strings.end())
        return usz(std::distance(strings.begin(), it));

    strings.emplace_back(str);
    return strings.size() - 1;
}

namespace lcc::glint {
void calculate_indices(
    std::unordered_map<Type*, ModuleDescription::TypeIndex>& out,
    usz& index,
    Type* ty
) {
    // Don't visit a type twice.
    if (out.contains(ty)) return;

    // Add dummy entry to out so that we don't visit the type twice if the
    // type is self-referencing. This needs to be updated after all the
    // children indices are calculated, and we actually know the index where
    // this type will be serialised.
    out.emplace(ty, -1);

    const auto recurse = [&](Type* t) {
        calculate_indices(out, index, t);
    };

    // Visit type's children, first.
    switch (ty->kind()) {
        // Types with no children.
        case glint::Type::Kind::Builtin:
        case glint::Type::Kind::FFIType:
        case glint::Type::Kind::Named:
        case glint::Type::Kind::Integer:
            break;

        // Types that always have a single child.
        case glint::Type::Kind::Pointer:
        case glint::Type::Kind::Reference:
        case glint::Type::Kind::DynamicArray:
        case glint::Type::Kind::Array:
        case glint::Type::Kind::ArrayView:
        case glint::Type::Kind::Enum:
            recurse(ty->elem());
            break;

        case glint::Type::Kind::Function: {
            auto f_ty = as<glint::FuncType>(ty);

            recurse(f_ty->return_type());

            for (auto p : f_ty->params())
                recurse(p.type);

        } break;

        case glint::Type::Kind::Sum: {
            auto s_ty = as<glint::SumType>(ty);
            for (auto m : s_ty->members())
                recurse(m.type);
        } break;

        case glint::Type::Kind::Union: {
            auto u_ty = as<glint::UnionType>(ty);
            for (auto m : u_ty->members())
                recurse(m.type);
        } break;

        case glint::Type::Kind::Struct: {
            auto s_ty = as<glint::StructType>(ty);
            for (auto m : s_ty->members())
                recurse(m.type);
        } break;

        case glint::Type::Kind::TemplatedStruct: {
            auto s_ty = as<glint::TemplatedStructType>(ty);
            for (auto m : s_ty->members())
                recurse(m.type);
        } break;

        case glint::Type::Kind::Typeof:
            Diag::ICE("Encountered TypeofType during serialisation. Bad sema, bad!");

        case glint::Type::Kind::Type:
            Diag::ICE("Encountered TypeType during serialisation. Bad sema, bad!");
    };

    // Finally, visit the type itself.
    out.at(ty) = ModuleDescription::TypeIndex(index);
    ++index;
}

void calculate_indices(std::unordered_map<Expr*, ModuleDescription::TypeIndex>& out, usz& index, Expr* expr) {
    // Don't visit an expr twice.
    if (out.contains(expr)) return;

    // Add dummy entry to out so that we don't visit the expr twice if the
    // expr is self-referencing. This needs to be updated after all the
    // children indices are calculated, and we actually know the index where
    // this type will be serialised.
    out.emplace(expr, -1);

    const auto recurse = [&](Expr* e) {
        calculate_indices(out, index, e);
    };

    // Visit expression's children, first.
    switch (expr->kind()) {
        // Expressions that never have children
        case Expr::Kind::EvaluatedConstant:
        case Expr::Kind::IntegerLiteral:
        case Expr::Kind::FractionalLiteral:
        case Expr::Kind::StringLiteral:
        case Expr::Kind::NameRef:
        case Expr::Kind::Type:
            break;

        // Expressions that may have a single child.
        case Expr::Kind::Return: {
            auto r = as<ReturnExpr>(expr);
            if (r->value())
                recurse(r->value());
        } break;

        // Expressions that have a single child.
        case Expr::Kind::Template: {
            auto t = as<TemplateExpr>(expr);
            recurse(t->body());
        } break;

        case Expr::Kind::MemberAccess: {
            auto m = as<MemberAccessExpr>(expr);
            recurse(m->object());
        } break;

        case Expr::Kind::Cast: {
            auto c = as<CastExpr>(expr);
            recurse(c->operand());
        } break;

        case Expr::Kind::Sizeof: {
            auto s = as<SizeofExpr>(expr);
            recurse(s->expr());
        } break;

        case Expr::Kind::Alignof: {
            auto a = as<AlignofExpr>(expr);
            recurse(a->expr());
        } break;

        case Expr::Kind::Unary: {
            auto u = as<UnaryExpr>(expr);
            recurse(u->operand());
        } break;

        // Expressions that have multiple children.
        case Expr::Kind::Binary: {
            auto b = as<BinaryExpr>(expr);
            recurse(b->lhs());
            recurse(b->rhs());
        } break;

        case Expr::Kind::If: {
            auto i = as<IfExpr>(expr);
            recurse(i->condition());
            recurse(i->then());
            if (i->otherwise())
                recurse(i->otherwise());
        } break;

        case Expr::Kind::While: {
            auto w = as<WhileExpr>(expr);
            recurse(w->condition());
            recurse(w->body());
        } break;

        case Expr::Kind::For: {
            auto f = as<ForExpr>(expr);
            recurse(f->init());
            recurse(f->condition());
            recurse(f->increment());
            recurse(f->body());
        } break;

        // Expressions that have a variable amount of children.
        case Expr::Kind::Apply: {
            auto a = as<ApplyExpr>(expr);
            recurse(a->function());
            for (auto e : a->argument_lists())
                recurse(e);
        } break;

        case Expr::Kind::Call: {
            auto c = as<CallExpr>(expr);
            recurse(c->callee());
            for (auto e : c->args())
                recurse(e);
        } break;

        case Expr::Kind::IntrinsicCall: {
            expr->print(true);
            LCC_TODO("Implement serialisation of intrinsic call");
        } break;

        case Expr::Kind::CompoundLiteral: {
            auto c = as<CompoundLiteral>(expr);
            for (auto e : c->children())
                recurse(e);
        } break;

        case Expr::Kind::Block: {
            auto b = as<BlockExpr>(expr);
            for (auto e : b->children())
                recurse(e);
        } break;

        case Expr::Kind::Group: {
            auto g = as<GroupExpr>(expr);
            for (auto e : g->children())
                recurse(e);
        } break;

        case Expr::Kind::Match: {
            auto m = as<MatchExpr>(expr);
            recurse(m->object());

            for (auto e : m->bodies())
                recurse(e);

        } break;

        case Expr::Kind::Module:
        case Expr::Kind::EnumeratorDecl:
        case Expr::Kind::FuncDecl:
        case Expr::Kind::TemplatedFuncDecl:
        case Expr::Kind::OverloadSet:
        case Expr::Kind::TypeAliasDecl:
        case Expr::Kind::TypeDecl:
        case Expr::Kind::VarDecl:
            expr->print(true);
            LCC_TODO("Implement serialisation of preceding expression AST\n");
    };

    // Finally, visit the expression itself.
    out.at(expr) = ModuleDescription::ExprIndex(index);
    ++index;
}
} // namespace lcc::glint

auto lcc::glint::Module::serialise() -> std::vector<lcc::u8> {
    // NOTE: See glint/module_description.hh for binary format details.

    // TODO: List of modules imported by this module (so that a dependency
    // tree may be formed simply from the metadatas alone).
    // TODO: InfoTable with different, optional entries:
    //     Target Entry (target triple)
    //     Command Line Entry (how module was compiled)
    //     Time Entry (when module was compiled)
    //     Location Information Entry (maybe give location corresponding to type index)
    //     Source File (path to module source)

    ModuleDescription::Header hdr{};
    std::vector<u8> declarations{};
    std::vector<u8> types_data{};
    std::vector<u8> exprs_data{};
    std::vector<u8> serialised_name{};

    // Associate every single Type* we will end up serialising with an
    // index, ahead of time. This means that, even for circular references, we
    // know "where" it will end up (at least, enough about where it will end
    // up to write a type index).
    std::unordered_map<Type*, ModuleDescription::TypeIndex> type_indices{};
    {
        usz index{0};
        for (auto* decl : exports()) {
            calculate_indices(type_indices, index, decl->type());

            if (
                ModuleDescription::DeclarationHeader::get_kind(decl)
                == ModuleDescription::DeclarationHeader::Kind::TEMPLATE
            ) {
                auto* v = as<VarDecl>(decl);
                LCC_ASSERT(v->init(), "Named Template has NULL body");
                for (auto template_parameter : as<TemplateExpr>(v->init())->params())
                    calculate_indices(type_indices, index, template_parameter.type);
            }
        }
    }
    // Associate every single Expr* THAT WE WILL END UP SERIALISING with an
    // index, ahead of time. This means that, even for circular references, we
    // know "where" it will end up (at least, enough about where it will end
    // up to write an expression index).
    std::unordered_map<Expr*, ModuleDescription::ExprIndex> expr_indices{};
    {
        usz index{0};
        for (auto* decl : exports()) {
            if (
                ModuleDescription::DeclarationHeader::get_kind(decl)
                == ModuleDescription::DeclarationHeader::Kind::TEMPLATE
            ) {
                auto* v = as<VarDecl>(decl);
                LCC_ASSERT(v->init(), "Named Template has NULL body");
                calculate_indices(expr_indices, index, v->init());
            }
        }
    }

    std::vector<Type*> type_cache{};
    std::vector<Type*> type_current{};

    std::vector<Expr*> expr_cache{};
    std::vector<Expr*> expr_current{};

    for (auto* decl : exports()) {
        // Decl: DeclHeader, length :u8, name :u8[length]

        // Prepare declaration header
        ModuleDescription::TypeIndex type_index = serialise(
            types_data,
            type_cache,
            type_current,
            type_indices,
            decl->type()
        );
        ModuleDescription::DeclarationHeader decl_hdr{
            u16(ModuleDescription::DeclarationHeader::get_kind(decl)),
            type_index
        };

        if (decl_hdr.kind == +ModuleDescription::DeclarationHeader::Kind::TEMPLATE) {
            for (auto template_parameter : as<TemplateExpr>(as<VarDecl>(decl)->init())->params()) {
                (void) serialise(
                    types_data,
                    type_cache,
                    type_current,
                    type_indices,
                    template_parameter.type
                );
            };
            decl_hdr.expr_index = serialise_expr(
                exprs_data,
                expr_cache,
                expr_current,
                expr_indices,
                type_indices,
                as<VarDecl>(decl)->init()
            );
        }

        auto decl_hdr_bytes = lcc::utils::to_bytes(decl_hdr);

        // Prepare length
        LCC_ASSERT(
            decl->name().length() <= std::numeric_limits<u8>::max(),
            "Exported declaration has over-long name and cannot be encoded in binary format"
        );
        u8 length = u8(decl->name().length());

        declarations.insert(
            declarations.end(),
            decl_hdr_bytes.begin(),
            decl_hdr_bytes.end()
        );
        declarations.push_back(length);
        declarations.insert(
            declarations.end(),
            decl->name().begin(),
            decl->name().end()
        );
    }

    // TODO: Make name serialisable
    LCC_ASSERT(
        not name().empty(),
        "Cannot serialise unnamed module"
    );
    serialised_name.insert(
        serialised_name.end(),
        name().begin(),
        name().end()
    );
    LCC_ASSERT(
        std::find(serialised_name.begin(), serialised_name.end(), '\0') == serialised_name.end(),
        "Cannot serialise module with NULL character in name"
    );
    serialised_name.push_back('\0');

    // Final header fixups now that nothing will change.
    hdr.size = u32(
        sizeof(ModuleDescription::Header)
        + declarations.size()
        + types_data.size()
        + exprs_data.size()
        + serialised_name.size()
    );
    hdr.type_table_offset = u32(
        sizeof(ModuleDescription::Header)
        + declarations.size()
    );
    hdr.expr_table_offset = u32(
        sizeof(ModuleDescription::Header)
        + declarations.size()
        + types_data.size()
    );
    hdr.name_offset = u32(
        sizeof(ModuleDescription::Header)
        + declarations.size()
        + types_data.size()
        + exprs_data.size()
    );
    hdr.declaration_count = u16(exports().size());
    hdr.type_count = u16(type_cache.size());
    hdr.expr_count = u16(expr_cache.size());

    // TODO: Set to false if there are no top level non-declaration
    // expressions.
    hdr.requires_initialisation = true;

    // Convert header to byte representation that is easy to serialise.
    auto hdr_bytes = lcc::utils::to_bytes(hdr);

    for (auto e : expr_cache) {
        fmt::print("Serialised expression:\n");
        e->print(true);
    }

    std::vector<u8> out{};
    out.insert(out.end(), hdr_bytes.begin(), hdr_bytes.end());
    out.insert(out.end(), declarations.begin(), declarations.end());
    out.insert(out.end(), types_data.begin(), types_data.end());
    out.insert(out.end(), exprs_data.begin(), exprs_data.end());
    out.insert(out.end(), serialised_name.begin(), serialised_name.end());
    return out;
}
