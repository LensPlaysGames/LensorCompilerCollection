#include <bit>
#include <laye/ast.hh>
#include <laye/parser.hh>
#include <lcc/context.hh>
#include <lcc/target.hh>
#include <lcc/utils/ast_printer.hh>
#include <lcc/utils/rtti.hh>

namespace layec = lcc::laye;

void* layec::Scope::operator new(size_t sz, Parser& parser) {
    LCC_ASSERT(not parser.IsInSpeculativeParse(), "Should never be allocating syntax scopes while in speculative parse mode.");
    auto ptr = ::operator new(sz);
    parser.module->scopes.push_back(static_cast<Scope*>(ptr));
    return ptr;
}

void* layec::Scope::operator new(size_t sz, Module& module) {
    auto ptr = ::operator new(sz);
    module.scopes.push_back(static_cast<Scope*>(ptr));
    return ptr;
}

void* layec::SemaNode::operator new(size_t sz, Parser& parser) {
    LCC_ASSERT(not parser.IsInSpeculativeParse(), "Should never be allocating syntax nodes while in speculative parse mode.");
    return operator new(sz, *parser.module);
}

void* layec::SemaNode::operator new(size_t sz, Module& module) {
    auto ptr = ::operator new(sz);
    module.nodes.push_back(static_cast<Statement*>(ptr));
    return ptr;
}

auto layec::LayeContext::parse_laye_file(File& file) -> Module* {
    auto lookup_path = fs::absolute(file.path()).string();
    if (not lookup_module(lookup_path)) {
        auto mod = lcc::laye::Parser::Parse(this, file);
        add_module(fs::absolute(file.path()).string(), mod);
        return mod;
    } else return nullptr;
}

layec::Module::Module(LayeContext* laye_context, File* file)
    : _laye_context(laye_context), _file(file) {
    _exports = new (*this) Scope(nullptr);
}

void layec::Module::add_export(NamedDecl* decl) {
    // Diag::Note("Declaring an export in scope ({}) named {}", (void*)_exports, decl->name());
    [[maybe_unused]] auto res = _exports->declare(this, decl->name(), decl);
}

auto layec::Scope::declare(
    Module* module,
    std::string name,
    NamedDecl* decl
) -> Result<NamedDecl*> {
    if (
        auto it = symbols.find(name);
        it != symbols.end() and
        not is<FunctionDecl>(it->second) and
        not is<FunctionDecl>(decl)
    ) return Diag::Error(module->context(), decl->location(), "Redeclaration of '{}'", name);

    /// Otherwise, add the symbol.
    symbols.emplace(std::move(name), decl);
    return decl;
}

layec::FunctionDecl::FunctionDecl(Module* module, Location location, std::vector<DeclModifier> mods, Type* returnType, std::string name, std::vector<TemplateParam> template_params, std::vector<BindingDecl*> params, Statement* body)
    : NamedDecl(Kind::DeclFunction, module, location, mods, name, template_params), _returnType(returnType), _params(std::move(params)), _body(body) {
    std::vector<Type*> param_types{};
    for (auto param : _params) {
        param_types.push_back(param->type());
    }

    _function_type = new (*module) FuncType{location, returnType, param_types};
}

std::string layec::ToString(layec::TokenKind kind) {
    using TokenKind = layec::TokenKind;
    switch (kind) {
        case TokenKind::Eof: return "eof";
        case TokenKind::Tilde: return "~";
        case TokenKind::Bang: return "!";
        case TokenKind::Percent: return "%";
        case TokenKind::Ampersand: return "&";
        case TokenKind::Star: return "*";
        case TokenKind::OpenParen: return "(";
        case TokenKind::CloseParen: return ")";
        case TokenKind::Minus: return "-";
        case TokenKind::Equal: return "=";
        case TokenKind::Plus: return "+";
        case TokenKind::OpenBracket: return "[";
        case TokenKind::CloseBracket: return "]";
        case TokenKind::OpenBrace: return "{";
        case TokenKind::CloseBrace: return "}";
        case TokenKind::Pipe: return "|";
        case TokenKind::SemiColon: return ";";
        case TokenKind::Colon: return ":";
        case TokenKind::Comma: return ",";
        case TokenKind::Less: return "<";
        case TokenKind::Greater: return ">";
        case TokenKind::Dot: return ".";
        case TokenKind::Slash: return "/";
        case TokenKind::Question: return "?";
        case TokenKind::Ident: return "<ident>";
        case TokenKind::LitInt: return "<literal int>";
        case TokenKind::LitFloat: return "<literal float>";
        case TokenKind::LitString: return "<literal string>";
        case TokenKind::LitRune: return "<literal rune>";
        case TokenKind::LessLess: return "<<";
        case TokenKind::GreaterGreater: return ">>";
        case TokenKind::EqualEqual: return "==";
        case TokenKind::BangEqual: return "!=";
        case TokenKind::PlusEqual: return "+=";
        case TokenKind::MinusEqual: return "-=";
        case TokenKind::SlashEqual: return "/=";
        case TokenKind::StarEqual: return "*=";
        case TokenKind::PercentEqual: return "%=";
        case TokenKind::LessEqual: return "<=";
        case TokenKind::GreaterEqual: return ">=";
        case TokenKind::AmpersandEqual: return "&=";
        case TokenKind::PipeEqual: return "|=";
        case TokenKind::TildeEqual: return "~=";
        case TokenKind::LessLessEqual: return "<<=";
        case TokenKind::GreaterGreaterEqual: return ">>=";
        case TokenKind::EqualGreater: return "=>";
        case TokenKind::ColonColon: return "::";
        case TokenKind::Bool: return "bool";
        case TokenKind::Int: return "int";
        case TokenKind::UInt: return "uint";
        case TokenKind::Float: return "float";
        // case TokenKind::Context: return "context";
        case TokenKind::True: return "true";
        case TokenKind::False: return "false";
        case TokenKind::Nil: return "nil";
        case TokenKind::Global: return "global";
        case TokenKind::If: return "if";
        case TokenKind::Else: return "else";
        case TokenKind::For: return "for";
        case TokenKind::Do: return "do";
        case TokenKind::Switch: return "switch";
        case TokenKind::Case: return "case";
        case TokenKind::Default: return "default";
        case TokenKind::Return: return "return";
        case TokenKind::Break: return "break";
        case TokenKind::Continue: return "continue";
        case TokenKind::Goto: return "goto";
        case TokenKind::Struct: return "struct";
        case TokenKind::Variant: return "variant";
        case TokenKind::Enum: return "enum";
        case TokenKind::Alias: return "alias";
        case TokenKind::Test: return "test";
        case TokenKind::Import: return "import";
        case TokenKind::Export: return "export";
        case TokenKind::From: return "from";
        case TokenKind::As: return "as";
        case TokenKind::Operator: return "operator";
        case TokenKind::Mut: return "mut";
        case TokenKind::New: return "new";
        case TokenKind::Delete: return "delete";
        case TokenKind::Cast: return "cast";
        case TokenKind::Try: return "try";
        case TokenKind::Catch: return "catch";
        case TokenKind::Discard: return "discard";
        case TokenKind::Sizeof: return "sizeof";
        case TokenKind::Alignof: return "alignof";
        case TokenKind::Offsetof: return "offsetof";
        case TokenKind::Not: return "not";
        case TokenKind::And: return "and";
        case TokenKind::Or: return "or";
        case TokenKind::Xor: return "xor";
        case TokenKind::Varargs: return "varargs";
        case TokenKind::Const: return "const";
        case TokenKind::Foreign: return "foreign";
        case TokenKind::Inline: return "inline";
        case TokenKind::Callconv: return "callconv";
        // case TokenKind::Impure: return "impure";
        case TokenKind::Void: return "void";
        case TokenKind::Var: return "var";
        case TokenKind::Noreturn: return "noreturn";
        case TokenKind::Rawptr: return "rawptr";
        default: return "<unknown>";
    }
}

std::string layec::ToString(layec::OperatorKind kind) {
    switch (kind) {
        case OperatorKind::Invalid: return "<invalid>";
        case OperatorKind::Add: return "+";
        case OperatorKind::Sub: return "-";
        case OperatorKind::Mul: return "*";
        case OperatorKind::Div: return "/";
        case OperatorKind::Mod: return "%";
        case OperatorKind::Greater: return ">";
        case OperatorKind::GreaterEqual: return ">=";
        case OperatorKind::Less: return "<";
        case OperatorKind::LessEqual: return "<=";
        case OperatorKind::Equal: return "==";
        case OperatorKind::NotEqual: return "!=";
        case OperatorKind::Pos: return "+";
        case OperatorKind::Neg: return "-";
        case OperatorKind::Address: return "&";
        case OperatorKind::Deref: return "*";
        case OperatorKind::Compl: return "~";
        case OperatorKind::And: return "&";
        case OperatorKind::Or: return "|";
        case OperatorKind::Xor: return "~";
        case OperatorKind::Lsh: return "<<";
        case OperatorKind::Rsh: return ">>";
        case OperatorKind::Assign: return "=";
        case OperatorKind::AddEqual: return "+=";
        case OperatorKind::SubEqual: return "-=";
        case OperatorKind::DivEqual: return "/=";
        case OperatorKind::MulEqual: return "*/";
        case OperatorKind::ModEqual: return "%=";
        case OperatorKind::AndEqual: return "&=";
        case OperatorKind::OrEqual: return "|=";
        case OperatorKind::XorEqual: return "~=";
        case OperatorKind::LshEqual: return "<<=";
        case OperatorKind::RshEqual: return ">>=";
        case OperatorKind::Call: return "()";
        case OperatorKind::Cast: return "cast";
        case OperatorKind::Index: return "[]";
        case OperatorKind::IndexEqual: return "[]=";
        case OperatorKind::New: return "new";
        case OperatorKind::Delete: return "delete";
        default: return "<unknown>";
    }
}

std::string layec::ToString(Statement::Kind kind) {
    using Sk = Statement::Kind;
    switch (kind) {
        case Sk::OverloadSet: return "OverloadSet";
        case Sk::DeclBinding: return "DeclBinding";
        case Sk::DeclFunction: return "DeclFunction";
        case Sk::DeclStruct: return "DeclStruct";
        case Sk::DeclEnum: return "DeclEnum";
        case Sk::DeclAlias: return "DeclAlias";
        case Sk::DeclImport: return "DeclImport";
        case Sk::Block: return "Block";
        case Sk::Assign: return "Assign";
        case Sk::Delete: return "Delete";
        case Sk::Discard: return "Discard";
        case Sk::Expr: return "Expr";
        case Sk::Empty: return "Empty";
        case Sk::If: return "If";
        case Sk::For: return "For";
        case Sk::ForEach: return "ForEach";
        case Sk::DoFor: return "DoFor";
        case Sk::Switch: return "Switch";
        case Sk::Return: return "Return";
        case Sk::Break: return "Break";
        case Sk::Continue: return "Continue";
        case Sk::Fallthrough: return "Fallthrough";
        case Sk::Defer: return "Defer";
        case Sk::Goto: return "Goto";
        case Sk::Test: return "Test";
        default: return "<unknown>";
    }
}

std::string layec::ToString(Expr::Kind kind) {
    using Ek = Expr::Kind;
    switch (kind) {
        case Ek::Unary: return "Unary";
        case Ek::Binary: return "Binary";
        case Ek::And: return "And";
        case Ek::Or: return "Or";
        case Ek::Xor: return "Xor";
        case Ek::UnwrapNilable: return "UnwrapNilable";
        case Ek::Constant: return "Constant";
        case Ek::LookupName: return "LookupName";
        case Ek::LookupPath: return "LookupPath";
        case Ek::FieldIndex: return "FieldIndex";
        case Ek::ValueIndex: return "ValueIndex";
        case Ek::Slice: return "Slice";
        case Ek::Call: return "Call";
        case Ek::Ctor: return "Ctor";
        case Ek::Not: return "Not";
        case Ek::Cast: return "Cast";
        case Ek::New: return "New";
        case Ek::Try: return "Try";
        case Ek::Catch: return "Catch";
        case Ek::Do: return "Do";
        case Ek::Sizeof: return "Sizeof";
        case Ek::Offsetof: return "Offsetof";
        case Ek::Alignof: return "Alignof";
        case Ek::LitNil: return "LitNil";
        case Ek::LitBool: return "LitBool";
        case Ek::LitString: return "LitString";
        case Ek::LitInt: return "LitInt";
        case Ek::LitFloat: return "LitFloat";
        case Ek::TypeInfer: return "TypeInfer";
        case Ek::TypeNilable: return "TypeNilable";
        case Ek::TypeErrUnion: return "TypeErrUnion";
        case Ek::TypeOverloadSet: return "TypeOverloadSet";
        case Ek::TypeLookupName: return "TypeLookupName";
        case Ek::TypeLookupPath: return "TypeLookupPath";
        case Ek::TypeLiteralString: return "TypeLiteralString";
        case Ek::TypeArray: return "TypeArray";
        case Ek::TypeSlice: return "TypeSlice";
        case Ek::TypePointer: return "TypePointer";
        case Ek::TypeBuffer: return "TypeBuffer";
        case Ek::TypeFunc: return "TypeFunc";
        case Ek::TypeStruct: return "TypeStruct";
        case Ek::TypeVariant: return "TypeVariant";
        case Ek::TypeEnum: return "TypeEnum";
        case Ek::TypeNoreturn: return "TypeNoreturn";
        case Ek::TypeRawptr: return "TypeRawptr";
        case Ek::TypeVoid: return "TypeVoid";
        case Ek::TypeBool: return "TypeBool";
        case Ek::TypeInt: return "TypeInt";
        case Ek::TypeFloat: return "TypeFloat";
        case Ek::TypePoison: return "TypePoison";
        case Ek::TypeReference: return "TypeReference";
        default: return "<unknown>";
    }
}

bool layec::Expr::evaluate(const layec::LayeContext* laye_context, layec::EvalResult& out, bool required) {
    LCC_ASSERT(!required || sema_ok(), "Cannot evaluate ill-formed or unchecked expression");
    switch (kind()) {
        case Kind::LitString: {
            out = as<LitStringExpr>(this);
            return true;
        }

        case Kind::LitInt: {
            out = i64(as<LitIntExpr>(this)->value());
            return true;
        }

        default: return false;
    }
}

auto layec::Type::size(const Context* ctx) const -> usz {
    if (auto st = cast<SizableType>(this)) {
        return (usz) st->bit_width();
    }

    switch (kind()) {
        default: LCC_ASSERT(false, "unhandled type {} in type->size()", ToString(kind()));

        case Expr::Kind::TypeRawptr:
        case Expr::Kind::TypePointer:
        case Expr::Kind::TypeReference:
        case Expr::Kind::TypeBuffer: return ctx->target()->size_of_pointer;

        case Expr::Kind::TypeVariant: {
            auto t = as<VariantType>(this);
            return t->root_struct_type()->size(ctx);
        }

        case Expr::Kind::TypeStruct: {
            auto t = as<layec::StructType>(this);

            usz current_size = 0;
            usz current_align = 1;

            if (not t->variants().empty()) {
                current_size = ctx->target()->size_of_pointer;
                current_align = ctx->target()->align_of_pointer;
            }

            for (auto& field : t->fields()) {
                usz field_size = field.type->size(ctx);
                usz field_align = field.type->align(ctx);

                if (field_align > current_align)
                    current_align = field_align;

                usz padding = (field_align - (current_size & (field_align - 1))) & (field_align - 1);
                usz aligned_offset = current_size + padding;

                current_size = aligned_offset + field_size;
            }

            for (auto& variant : t->variants()) {
                usz variant_size = variant->size_alone(ctx);
                usz variant_align = variant->align_alone(ctx);

                if (variant_align > current_align)
                    current_align = variant_align;

                usz padding = (variant_align - (current_size & (variant_align - 1))) & (variant_align - 1);
                usz aligned_offset = current_size + padding;

                current_size = aligned_offset + variant_size;
            }

            current_size += (current_align - (current_size & (current_align - 1))) & (current_align - 1);
            return current_size;
        }
    }
}

auto layec::Type::align(const Context* ctx) const -> usz {
    if (auto st = cast<SizableType>(this)) {
        return std::bit_ceil((usz) st->bit_width());
    }

    switch (kind()) {
        default: LCC_TODO();

        case Expr::Kind::TypeRawptr:
        case Expr::Kind::TypePointer:
        case Expr::Kind::TypeReference:
        case Expr::Kind::TypeBuffer: return ctx->target()->align_of_pointer;

        case Expr::Kind::TypeVariant: {
            auto t = as<VariantType>(this);
            return t->root_struct_type()->align(ctx);
        }

        case Expr::Kind::TypeStruct: {
            auto t = as<layec::StructType>(this);

            usz current_align = 1;
            for (auto& field : t->fields()) {
                usz field_align = field.type->align(ctx);
                if (field_align > current_align)
                    current_align = field_align;
            }

            for (auto& variant : t->variants()) {
                usz variant_align = variant->align_alone(ctx);
                if (variant_align > current_align)
                    current_align = variant_align;
            }

            return current_align;
        }
    }
}

auto layec::VariantType::size_alone(const Context* ctx) const -> usz {
    auto t = this;

    usz current_size = 0;
    usz current_align = 1;

    // NOTE(local): the size of a variant does not contain any tag data, even if it
    // contains other nested variants; the root struct tag covers all variant cases.

    for (auto& field : t->fields()) {
        usz field_size = field.type->size(ctx);
        usz field_align = field.type->align(ctx);

        if (field_align > current_align)
            current_align = field_align;

        usz padding = (field_align - (current_size & (field_align - 1))) & (field_align - 1);
        usz aligned_offset = current_size + padding;

        current_size = aligned_offset + field_size;
    }

    for (auto& variant : t->variants()) {
        usz variant_size = variant->size_alone(ctx);
        usz variant_align = variant->align_alone(ctx);

        if (variant_align > current_align)
            current_align = variant_align;

        usz padding = (variant_align - (current_size & (variant_align - 1))) & (variant_align - 1);
        usz aligned_offset = current_size + padding;

        current_size = aligned_offset + variant_size;
    }

    current_size += (current_align - (current_size & (current_align - 1))) & (current_align - 1);
    return current_size;
}

auto layec::VariantType::align_alone(const Context* ctx) const -> usz {
    auto t = this;

    usz current_align = 1;
    for (auto& field : t->fields()) {
        usz field_align = field.type->align(ctx);
        if (field_align > current_align)
            current_align = field_align;
    }

    for (auto& variant : t->variants()) {
        usz variant_align = variant->align_alone(ctx);
        if (variant_align > current_align)
            current_align = variant_align;
    }

    return current_align;
}

auto layec::Type::string(bool use_colours) const -> std::string {
    lcc::utils::Colours C{use_colours};
    using enum lcc::utils::Colour;

    auto TemplateArgsToString = [&](const std::vector<layec::Expr*>& template_args) {
        std::string out{};
        if (template_args.empty()) return out;
        out += fmt::format("<{}", C(White));
        for (lcc::usz i = 0; i < template_args.size(); i++) {
            if (i > 0) out += fmt::format("{}, ", C(White));
            if (auto type_arg = cast<layec::Type>(template_args[i])) {
                out += type_arg->string(use_colours);
            } else {
                out += "(expr)";
            }
        }
        out += fmt::format("{}>", C(Reset));
        return out;
    };

    auto AccessString = [&](TypeAccess access, bool lhs_space = true) {
        return access == TypeAccess::ReadOnly ? "" : (lhs_space ? fmt::format(" {}mut{}", C(Cyan), C(Reset)) : fmt::format("{}mut{} ", C(Cyan), C(Reset)));
    };

    switch (kind()) {
        default: LCC_ASSERT(false, "unhandled type {} in type->string()", ToString(kind()));

        case Kind::TypePoison: return fmt::format("{}poison", C(Cyan));

        case Kind::TypeInfer: return fmt::format("{}var", C(Cyan));
        case Kind::TypeNilable: return fmt::format("{}?", as<NilableType>(this)->elem_type()->string(use_colours));
        case Kind::TypeErrUnion: {
            auto e = as<ErrUnionType>(this);
            return fmt::format("{}!{}", e->error_name(), e->value_type()->string(use_colours));
        }

        case Kind::TypeLookupName: {
            return fmt::format(
                "{}{}{}",
                C(White),
                as<NameType>(this)->name(),
                TemplateArgsToString(as<NameType>(this)->template_args())
            );
        }

        case Kind::TypeLookupPath: {
            std::string path{};
            auto path_names = as<PathType>(this)->names();
            for (lcc::usz i = 0; i < path_names.size(); i++) {
                if (i > 0) path += fmt::format("{}::", C(White));
                path += fmt::format("{}{}", C(White), path);
            }
            return path + TemplateArgsToString(as<PathType>(this)->template_args());
        }

        case Kind::TypeArray: {
            auto t = as<ArrayType>(this);
            if (sema_ok()) {
                LCC_TODO();
            }

            return fmt::format(
                "{}{}[{} array]",
                t->elem_type()->string(use_colours),
                AccessString(t->access()),
                t->rank_lengths().size()
            );
        }

        case Kind::TypeSlice: {
            auto t = as<SliceType>(this);
            return fmt::format("{}{}[]", t->elem_type()->string(use_colours), AccessString(t->access()));
        }

        case Kind::TypePointer: {
            auto t = as<PointerType>(this);
            return fmt::format("{}{}*", t->elem_type()->string(use_colours), AccessString(t->access()));
        }

        case Kind::TypeReference: {
            auto t = as<ReferenceType>(this);
            return fmt::format("{}{}&", t->elem_type()->string(use_colours), AccessString(t->access()));
        }

        case Kind::TypeBuffer: {
            auto t = as<BufferType>(this);
            return fmt::format("{}{}[*]", t->elem_type()->string(use_colours), AccessString(t->access()));
        }

        case Kind::TypeFunc: {
            auto f = as<FuncType>(this);
            std::string params_string{};
            auto params = f->param_types();
            for (lcc::usz i = 0; i < params.size(); i++) {
                if (i > 0) params_string += fmt::format("{}, ", C(White));
                params_string += fmt::format("{}", params[i]->string(use_colours));
            }
            return fmt::format("{}({})", f->return_type()->string(use_colours), params_string);
        }

        case Kind::TypeStruct: {
            return fmt::format(
                "{}{}",
                C(White),
                as<StructType>(this)->name()
            );
        }

        case Kind::TypeVariant: {
            auto f = as<VariantType>(this);
            std::string result{};

            std::function<void(const StructType*)> AppendToResult;
            AppendToResult = [&](const StructType* struct_type) {
                if (auto variant_type = cast<VariantType>(struct_type)) {
                    AppendToResult(variant_type->parent_struct());
                    result += fmt::format("{}::", C(Red));
                }
                result += fmt::format("{}{}", C(White), struct_type->name());
            };

            AppendToResult(f);
            return result;
        }

        case Kind::TypeNoreturn: return fmt::format("{}noreturn{}", C(Cyan), C(Reset));
        case Kind::TypeRawptr: return fmt::format("{}rawptr{}", C(Cyan), C(Reset));
        case Kind::TypeVoid: return fmt::format("{}void{}", C(Cyan), C(Reset));
        case Kind::TypeLiteralString: return fmt::format("{}literal string{}", C(Cyan), C(Reset));

        case Kind::TypeBool: {
            auto t = as<BoolType>(this);
            if (t->is_platform()) return fmt::format("{}bool{}", C(Cyan), C(Reset));
            return fmt::format("{}b{}{}", C(Cyan), t->bit_width(), C(Reset));
        }

        case Kind::TypeInt: {
            auto t = as<IntType>(this);
            if (t->is_platform()) return fmt::format("{}{}int{}", C(Cyan), t->is_signed() ? "" : "u", C(Reset));
            return fmt::format("{}{}{}{}", C(Cyan), t->is_signed() ? "i" : "u", t->bit_width(), C(Reset));
        }

        case Kind::TypeFloat: {
            auto t = as<FloatType>(this);
            if (t->is_platform()) return fmt::format("{}float{}", C(Cyan), C(Reset));
            return fmt::format("{}f{}{}", C(Cyan), t->bit_width(), C(Reset));
        }
    }
}

bool layec::Type::is_signed_integer() const {
    return is_integer() and as<IntType>(this)->is_signed();
}

bool layec::Type::is_unsigned_integer() const {
    return is_integer() and not as<IntType>(this)->is_signed();
}

auto layec::Type::strip_pointers_and_references() -> Type* {
    auto ty = this;
    while (is<PointerType, ReferenceType>(ty)) ty = as<SingleElementType>(ty)->elem_type();
    return ty;
}

auto layec::Type::strip_references() -> Type* {
    auto ty = this;
    while (is<ReferenceType>(ty)) ty = as<ReferenceType>(ty)->elem_type();
    return ty;
}

bool layec::Expr::is_lvalue() const {
    if (not type()) return false;
    return is<ReferenceType>(type());
}

bool layec::Type::Equal(const Type* a, const Type* b) {
    if (a->kind() != b->kind()) return false;
    if ((void*)a == (void*)b) return true;
    switch (a->kind()) {
        // the `var` type should not exist past sema and should never
        // never equal another type.
        case Kind::TypeInfer: return false;

        // nilable types are equal if their element types are equal.
        case Kind::TypeNilable: {
            auto a2 = as<NilableType>(a);
            auto b2 = as<NilableType>(b);
            return Type::Equal(a2->elem_type(), b2->elem_type());
        }

        case Kind::TypeErrUnion: {
            auto a2 = as<ErrUnionType>(a);
            auto b2 = as<ErrUnionType>(b);
            if (a2->has_error_name() != b2->has_error_name())
                return false;
            if (a2->has_error_name() and a2->error_name() != b2->error_name())
                return false;
            return Type::Equal(a2->value_type(), b2->value_type());
        }

        case Kind::TypeLookupName:
        case Kind::TypeLookupPath:
            return false;

        case Kind::TypeArray: {
            auto a2 = as<ArrayType>(a);
            auto b2 = as<ArrayType>(b);
            if (a2->access() != b2->access() or a2->rank() != b2->rank())
                return false;
            for (usz i = 0; i < a2->rank(); i++) {
                if (a2->nth_length(i) != b2->nth_length(i))
                    return false;
            }
            return Type::Equal(a2->elem_type(), b2->elem_type());
        }

        case Kind::TypeSlice: {
            auto a2 = as<SliceType>(a);
            auto b2 = as<SliceType>(b);
            return a2->access() == b2->access() and Type::Equal(a2->elem_type(), b2->elem_type());
        }

        case Kind::TypePointer: {
            auto a2 = as<PointerType>(a);
            auto b2 = as<PointerType>(b);
            return a2->access() == b2->access() and Type::Equal(a2->elem_type(), b2->elem_type());
        }

        case Kind::TypeReference: {
            auto a2 = as<ReferenceType>(a);
            auto b2 = as<ReferenceType>(b);
            return a2->access() == b2->access() and Type::Equal(a2->elem_type(), b2->elem_type());
        }

        case Kind::TypeBuffer: {
            auto a2 = as<BufferType>(a);
            auto b2 = as<BufferType>(b);
            return a2->access() == b2->access() and Type::Equal(a2->elem_type(), b2->elem_type());
        }

        case Kind::TypeFunc: {
            auto a2 = as<FuncType>(a);
            auto b2 = as<FuncType>(b);
            auto a2params = a2->param_types();
            auto b2params = b2->param_types();
            // TODO(local): varargs
            if (a2params.size() != b2params.size())
                return false;
            for (usz i = 0; i < a2params.size(); i++) {
                if (!Type::Equal(a2params[i], b2params[i]))
                    return false;
            }
            return a2->calling_convention() == b2->calling_convention() and
                   Type::Equal(a2->return_type(), b2->return_type());
        }

        // all "instances" of these types are identical.
        case Kind::TypeNoreturn:
        case Kind::TypeRawptr:
        case Kind::TypeVoid:
        case Kind::TypeLiteralString:
            return true;

        case Kind::TypeBool: {
            auto a2 = as<BoolType>(a);
            auto b2 = as<BoolType>(b);
            if (a2->is_platform())
                return b2->is_platform();
            return a2->bit_width() == b2->bit_width();
        }

        case Kind::TypeInt: {
            auto a2 = as<IntType>(a);
            auto b2 = as<IntType>(b);
            if (a2->is_signed() != b2->is_signed())
                return false;
            if (a2->is_platform())
                return b2->is_platform();
            return a2->bit_width() == b2->bit_width();
        }

        case Kind::TypeFloat: {
            auto a2 = as<FloatType>(a);
            auto b2 = as<FloatType>(b);
            if (a2->is_platform())
                return b2->is_platform();
            return a2->bit_width() == b2->bit_width();
        }

        default: LCC_UNREACHABLE(); return false;
    }

    LCC_UNREACHABLE();
}

bool layec::Statement::is_noreturn() const {
    switch (kind()) {
        default: return false;

        case Statement::Kind::Block: {
            auto s = as<BlockStatement>(this);
            // if anything within the block is noreturn,
            // then the whole block is.
            for (auto& c : s->children()) {
                if (c->is_noreturn())
                    return true;
            }
            return false;
        }

        case Statement::Kind::Assign: {
            auto s = as<AssignStatement>(this);
            // an assignment being noreturn is not semantically valid,
            // though if it were then either the target or the value would cause it to be.
            return s->value()->is_noreturn() or s->target()->is_noreturn();
        }

        case Statement::Kind::Delete: {
            auto s = as<DeleteStatement>(this);
            // a delete statement being noreturn is not semantically valid,
            // though if it were then the expression determines it.
            return s->expr()->is_noreturn();
        }

        case Statement::Kind::Discard: {
            auto s = as<DiscardStatement>(this);
            // a discard statement being noreturn is not semantically valid (because a value
            // is required to be discarded, and a noreturn expression doesn't return a value),
            // though if it were then the expression determines it.
            return s->expr()->is_noreturn();
        }

        case Statement::Kind::Expr: {
            auto s = as<ExprStatement>(this);
            return s->expr()->is_noreturn();
        }

        case Statement::Kind::If: {
            auto s = as<IfStatement>(this);
            if (s->condition()->is_noreturn())
                return true;
            // fail is not required, but if it's NOT noreturn
            // then the whole statement isn't either. (yes, even if the pass block is noreturn)
            if (auto fail = s->fail(); not fail->is_noreturn())
                return false;
            return s->pass()->is_noreturn();
        }

        case Statement::Kind::For: {
            auto s = as<ForStatement>(this);
            // If the init is noreturn, then so is the whole for loop.
            // The init is always executed, and always executed -first-, so
            // if it blocks control flow then nothing else can execute.
            if (auto init = s->init(); init->is_noreturn())
                return true;
            // Similar to the init, the condition is always run second if it exists.
            // If the condition is noreturn, even if that's a semantic error,
            // the rest of the for loop can't continue.
            if (auto condition = s->condition(); condition->is_noreturn())
                return true;
            // For pass/fail, the for loop is only noreturn if both are noreturn.
            // The increment is lumped in with the pass block, since it always executes
            // after the pass block.
            // If either is noreturn, the "pass" path should be considered noreturn.
            return (s->pass()->is_noreturn() or (s->increment() and s->increment()->is_noreturn())) and
                   (s->fail() && s->fail()->is_noreturn());
        }

        case Statement::Kind::ForEach: {
            auto s = as<ForEachStatement>(this);
            if (s->sequence()->is_noreturn())
                return true;
            // For pass/fail, the for each loop is only noreturn if both are noreturn.
            return s->pass()->is_noreturn() and
                   (s->fail() && s->fail()->is_noreturn());
        }

        case Statement::Kind::DoFor: {
            auto s = as<DoForStatement>(this);
            // The condition may come last, but it always runs.
            // If a do-for condition is noreturn, the whole loop is.
            if (auto condition = s->condition(); condition->is_noreturn())
                return true;
            // obviously, if the loop body is noreturn then the whole loop is.
            return s->body()->is_noreturn();
        }

        case Statement::Kind::Switch: {
            auto s = as<SwitchStatement>(this);
            if (s->target()->is_noreturn())
                return true;
            // a switch is only noreturn if *all* cases are noreturn,
            // since only one case needs to exist which continue control flow.
            for (auto& c : s->cases()) {
                if (not c.body->is_noreturn())
                    return false;
            }
            return true;
        }

        // return is always noreturn, control never proceeds past one.
        case Statement::Kind::Return: return true;
    }
}

bool layec::Expr::is_noreturn() const {
    switch (kind()) {
        default: return false;

        // none of these are semantically valid, but must be handled
        case Expr::Kind::Unary: return as<UnaryExpr>(this)->value()->is_noreturn();
        case Expr::Kind::Binary: {
            auto s = as<BinaryExpr>(this);
            return s->lhs()->is_noreturn() or s->rhs()->is_noreturn();
        }

        case Expr::Kind::And: {
            auto s = as<AndExpr>(this);
            return s->lhs()->is_noreturn() or s->rhs()->is_noreturn();
        }

        case Expr::Kind::Or: {
            auto s = as<OrExpr>(this);
            return s->lhs()->is_noreturn() or s->rhs()->is_noreturn();
        }

        case Expr::Kind::Xor: {
            auto s = as<XorExpr>(this);
            return s->lhs()->is_noreturn() or s->rhs()->is_noreturn();
        }

        case Expr::Kind::UnwrapNilable: return as<UnwrapNilableExpr>(this)->value()->is_noreturn();
        case Expr::Kind::FieldIndex: return as<FieldIndexExpr>(this)->target()->is_noreturn();
        case Expr::Kind::ValueIndex: {
            auto s = as<ValueIndexExpr>(this);
            // again, not semantically valid, but any index makes the whole thing noreturn
            for (auto& ind : s->indices()) {
                if (ind->is_noreturn())
                    return true;
            }
            return s->target()->is_noreturn();
        }

        case Expr::Kind::Slice: {
            auto s = as<SliceExpr>(this);
            if (auto offset = s->offset(); offset->is_noreturn())
                return true;
            if (auto length = s->length(); length->is_noreturn())
                return true;
            return s->target()->is_noreturn();
        }

        case Expr::Kind::Call: {
            auto s = as<CallExpr>(this);
            if (auto callee_type = s->type(); callee_type->is_function()) {
                if (as<FuncType>(callee_type)->return_type()->is_noreturn())
                    return true;
            }
            // again, not semantically valid, but any argument makes the whole thing noreturn
            for (auto& arg : s->args()) {
                if (arg->is_noreturn())
                    return true;
            }
            return s->target()->is_noreturn();
        }

        case Expr::Kind::Ctor: {
            auto s = as<CtorExpr>(this);
            for (auto& init : s->inits()) {
                if (init.value->is_noreturn())
                    return true;
            }
            return false;
        }

        case Expr::Kind::Not: return as<NotExpr>(this)->value()->is_noreturn();
        case Expr::Kind::Cast: return as<CastExpr>(this)->value()->is_noreturn();

        case Expr::Kind::New: {
            auto s = as<NewExpr>(this);
            for (auto& arg : s->args()) {
                if (arg->is_noreturn())
                    return true;
            }
            for (auto& init : s->inits()) {
                if (init.value->is_noreturn())
                    return true;
            }
            return false;
        }

        case Expr::Kind::Try: return as<TryExpr>(this)->value()->is_noreturn();
        // for catch, the body is only executed conditionally with no alternative to check against,
        // so only the expression value of the catch, not the error block body, is necessary to check.
        case Expr::Kind::Catch: return as<CatchExpr>(this)->value()->is_noreturn();

        case Expr::Kind::Do: LCC_TODO();
    }
}

namespace {
using lcc::as;
using lcc::cast;
using lcc::is;

using namespace lcc;

struct ASTPrinter : lcc::utils::ASTPrinter<ASTPrinter, layec::SemaNode, layec::Type> {
    void PrintTemplateParams(const std::vector<layec::TemplateParam>& template_params) {
        if (template_params.empty()) return;
        out += fmt::format("{}<", C(White));
        for (lcc::usz i = 0; i < template_params.size(); i++) {
            if (i > 0) out += fmt::format("{}, ", C(White));
            auto param = template_params[i];
            if (param.is_value_param()) {
                out += fmt::format("{} ", param.value_type->string(use_colour));
            }
            out += fmt::format("{}{}", C(Green), param.name);
        }
        out += fmt::format("{}>", C(White));
    }

    void PrintTemplateArgs(const std::vector<layec::Expr*>& template_args) {
        if (template_args.empty()) return;
        out += fmt::format("{}<", C(White));
        for (lcc::usz i = 0; i < template_args.size(); i++) {
            if (i > 0) out += fmt::format("{}, ", C(White));
            if (auto type_arg = cast<layec::Type>(template_args[i])) {
                out += type_arg->string(use_colour);
            } else {
                out += "(expr)";
            }
        }
        out += fmt::format("{}>", C(White));
    }

    void PrintModifiers(const std::vector<layec::DeclModifier>& mods) {
        using K = layec::TokenKind;
        out += C(Red);
        for (const auto& m : mods) {
            switch (m.decl_kind) {
                default: break;
                case K::Foreign: out += "Foreign "; break;
                case K::Const: out += "Const "; break;
                case K::Inline: out += "Inline "; break;
                case K::Callconv: {
                    switch (m.call_conv) {
                        default: break;
                        case lcc::CallConv::C: out += "CallConv(CDecl) "; break;
                        case lcc::CallConv::Laye: out += "CallConv(Laye) "; break;
                        case lcc::CallConv::Intercept: out += "CallConv(Intercept) "; break;
                    }
                } break;
            }
        }
    }

    void PrintStatementHeader(const layec::Statement* s) {
        using K = layec::Statement::Kind;
        switch (s->kind()) {
            default: {
                PrintBasicHeader("<??? Statement>", s);
                out += fmt::format(" {}{}\n", C(Magenta), +s->kind());
            } break;

            case K::DeclBinding: {
                auto n = cast<layec::BindingDecl>(s);
                PrintLinkage(n->linkage());
                PrintModifiers(n->mods());
                PrintBasicHeader("BindingDecl", n);
                out += fmt::format(
                    " {} {}{}",
                    n->type()->string(use_colour),
                    C(Green),
                    n->name()
                );
                PrintTemplateParams(n->template_params());
                out += "\n";
            } break;

            case K::DeclFunction: {
                auto n = cast<layec::FunctionDecl>(s);
                PrintLinkage(n->linkage());
                PrintModifiers(n->mods());
                PrintBasicHeader("FunctionDecl", n);
                out += fmt::format(
                    " {} {}{}",
                    n->return_type()->string(use_colour),
                    C(Green),
                    n->name()
                );
                PrintTemplateParams(n->template_params());
                out += fmt::format("{}(", C(White));
                auto params = n->params();
                for (lcc::usz i = 0; i < params.size(); i++) {
                    if (i > 0) out += fmt::format("{}, ", C(White));
                    out += fmt::format("{} {}{}", params[i]->type()->string(use_colour), C(White), params[i]->name());
                }
                out += fmt::format("{})\n", C(White));
            } break;

            case K::DeclStruct: {
                auto n = cast<layec::StructDecl>(s);
                PrintBasicHeader("StructDecl", n);
                out += fmt::format(
                    " {}{}",
                    C(Green),
                    n->name()
                );
                PrintTemplateParams(n->template_params());
                out += "\n";
            } break;

            case K::DeclEnum: {
                auto n = cast<layec::EnumDecl>(s);
                PrintBasicHeader("EnumDecl", n);
                out += fmt::format(
                    " {}{}\n",
                    C(Green),
                    n->name()
                );
            } break;

            case K::DeclAlias: {
                auto n = cast<layec::AliasDecl>(s);
                PrintBasicHeader("AliasDecl", n);
                out += fmt::format(
                    " {}{}",
                    C(Green),
                    n->name()
                );
                PrintTemplateParams(n->template_params());
                out += "\n";
            } break;

            case K::DeclImport: {
                auto n = cast<layec::ImportHeader>(s);
                PrintBasicHeader("ImportHeader", n);
                out += "\n";
            } break;

            case K::Block: {
                auto n = cast<layec::BlockStatement>(s);
                PrintBasicHeader("BlockStatement", n);
                out += "\n";
            } break;

            case K::Assign: {
                auto n = cast<layec::AssignStatement>(s);
                PrintBasicHeader("AssignStatement", n);
                out += fmt::format(" {}{}\n", C(White), ToString(n->assign_op()));
            } break;

            case K::Delete: {
                auto n = cast<layec::DeleteStatement>(s);
                PrintBasicHeader("DeleteStatement", n);
                out += "\n";
            } break;

            case K::Expr: {
                auto n = cast<layec::ExprStatement>(s);
                PrintExprHeader(n->expr());
            } break;

            case K::Empty: {
                auto n = cast<layec::EmptyStatement>(s);
                PrintBasicHeader("EmptyStatement", n);
                out += "\n";
            } break;

            case K::If: {
                auto n = cast<layec::IfStatement>(s);
                PrintBasicHeader("IfStatement", n);
                out += "\n";
            } break;

            case K::For: {
                auto n = cast<layec::ForStatement>(s);
                PrintBasicHeader("ForStatement", n);
                out += "\n";
            } break;

            case K::ForEach: {
                auto n = cast<layec::ForEachStatement>(s);
                PrintBasicHeader("ForEachStatement", n);
                out += fmt::format(
                    " {} {}{}\n",
                    n->type()->string(use_colour),
                    C(Green),
                    n->name()
                );
            } break;

            case K::DoFor: {
                auto n = cast<layec::DoForStatement>(s);
                PrintBasicHeader("DoForStatement", n);
                out += "\n";
            } break;

            case K::Switch: {
                auto n = cast<layec::SwitchStatement>(s);
                PrintBasicHeader("SwitchStatement", n);
                out += "\n";
            } break;

            case K::Return: {
                auto n = cast<layec::ReturnStatement>(s);
                PrintBasicHeader("ReturnStatement", n);
                out += "\n";
            } break;

            case K::Break: {
                auto n = cast<layec::BreakStatement>(s);
                PrintBasicHeader("BreakStatement", n);
                out += "\n";
            } break;

            case K::Continue: {
                auto n = cast<layec::ContinueStatement>(s);
                PrintBasicHeader("ContinueStatement", n);
                out += "\n";
            } break;

            case K::Fallthrough: {
                auto n = cast<layec::FallthroughStatement>(s);
                PrintBasicHeader("FallthroughStatement", n);
                out += "\n";
            } break;

            case K::Defer: {
                auto n = cast<layec::DeferStatement>(s);
                PrintBasicHeader("DeferStatement", n);
                out += "\n";
            } break;

            case K::Goto: {
                auto n = cast<layec::GotoStatement>(s);
                PrintBasicHeader("GotoStatement", n);
                out += "\n";
            } break;
        }
    }

    void PrintExprHeader(const layec::Expr* e) {
        using K = layec::Expr::Kind;
        switch (e->kind()) {
            default: {
                PrintBasicHeader(R"(<??? Expr>)", e);
                out += fmt::format(" {}{}\n", C(Magenta), +e->kind());
            } break;

            case K::Unary: {
                auto n = cast<layec::UnaryExpr>(e);
                PrintBasicHeader("UnaryExpr", n);
                out += "\n";
            } break;

            case K::Binary: {
                auto n = cast<layec::BinaryExpr>(e);
                PrintBasicNode("BinaryExpr", n, n->type(), false);
                out += fmt::format(" {}{}\n", C(White), ToString(n->operator_kind()));
            } break;

            case K::And: {
                auto n = cast<layec::AndExpr>(e);
                PrintBasicHeader("AndExpr", n);
                out += "\n";
            } break;

            case K::Or: {
                auto n = cast<layec::OrExpr>(e);
                PrintBasicHeader("OrExpr", n);
                out += "\n";
            } break;

            case K::Xor: {
                auto n = cast<layec::XorExpr>(e);
                PrintBasicHeader("XorExpr", n);
                out += "\n";
            } break;

            case K::UnwrapNilable: {
                auto n = cast<layec::UnwrapNilableExpr>(e);
                PrintBasicHeader("UnwrapNilableExpr", n);
                out += "\n";
            } break;

            case K::LookupName: {
                auto n = cast<layec::NameExpr>(e);
                PrintBasicNode("NameExpr", e, n->type(), false);
                out += fmt::format(" {}{}", C(Green), n->name());
                PrintTemplateArgs(n->template_args());
                out += "\n";
            } break;

            case K::LookupPath: {
                auto n = cast<layec::PathExpr>(e);
                PrintBasicNode("PathExpr", e, n->type(), false);
                out += " ";
                for (usz i = 0; i < n->names().size(); i++) {
                    if (i > 0) out += "::";
                    out += fmt::format("{}{}", C(Green), n->names()[i]);
                }
                PrintTemplateArgs(n->template_args());
                out += "\n";
            } break;

            case K::FieldIndex: {
                auto n = cast<layec::FieldIndexExpr>(e);
                PrintBasicHeader("FieldIndexExpr", n);
                out += fmt::format(" {} {}{}\n", n->type()->string(use_colour), C(Green), n->field_name());
            } break;

            case K::ValueIndex: {
                auto n = cast<layec::ValueIndexExpr>(e);
                PrintBasicHeader("ValueIndexExpr", n);
                out += "\n";
            } break;

            case K::Slice: {
                auto n = cast<layec::SliceExpr>(e);
                PrintBasicHeader("SliceExpr", n);
                out += "\n";
            } break;

            case K::Call: {
                auto n = cast<layec::CallExpr>(e);
                PrintBasicHeader("CallExpr", n);
                out += "\n";
            } break;

            case K::Ctor: {
                auto n = cast<layec::CtorExpr>(e);
                PrintBasicHeader("CtorExpr", n);
                out += "\n";
            } break;

            case K::Not: {
                auto n = cast<layec::NotExpr>(e);
                PrintBasicHeader("NotExpr", n);
                out += "\n";
            } break;

            case K::Cast: {
                auto n = cast<layec::CastExpr>(e);
                PrintBasicHeader("CastExpr", n);
                out += fmt::format(" {}{}", n->type()->string(use_colour), C(Reset));
                out += "\n";
            } break;

            case K::New: {
                auto n = cast<layec::NewExpr>(e);
                PrintBasicHeader("NewExpr", n);
                out += "\n";
            } break;

            case K::Try: {
                auto n = cast<layec::TryExpr>(e);
                PrintBasicHeader("TryExpr", n);
                out += "\n";
            } break;

            case K::Catch: {
                auto n = cast<layec::CatchExpr>(e);
                PrintBasicHeader("CatchExpr", n);
                if (not n->error_name().empty())
                    out += fmt::format(" {}{}\n", C(Green), n->error_name());
            } break;

            case K::Do: {
                PrintBasicNode("DoExpr", e, nullptr);
            } break;

            case K::Sizeof: {
                PrintBasicNode("SizeofExpr", e, nullptr);
            } break;

            case K::Offsetof: {
                PrintBasicNode("OffsetofExpr", e, nullptr);
            } break;

            case K::Alignof: {
                PrintBasicNode("AlignofExpr", e, nullptr);
            } break;

            case K::LitNil: {
                PrintBasicNode("LitNilExpr", e, nullptr);
            } break;

            case K::LitBool: {
                auto n = cast<layec::LitBoolExpr>(e);
                PrintBasicNode("LitBoolExpr", n, n->type(), false);
                out += fmt::format(" {}{}\n", C(Cyan), n->value());
            } break;

            case K::LitString: {
                auto n = cast<layec::LitStringExpr>(e);
                PrintBasicNode("LitStringExpr", n, n->type(), false);
                out += fmt::format(" {}{}\n", C(Cyan), n->value());
            } break;

            case K::LitInt: {
                auto n = cast<layec::LitIntExpr>(e);
                PrintBasicNode("LitIntExpr", n, n->type(), false);
                out += fmt::format(" {}{}\n", C(Cyan), n->value());
            } break;

            case K::LitFloat: {
                auto n = cast<layec::LitFloatExpr>(e);
                PrintBasicNode("LitFloatExpr", n, n->type(), false);
                out += fmt::format(" {}{}\n", C(Cyan), n->value());
            } break;

            case K::Constant: {
                auto n = cast<layec::ConstantExpr>(e);
                PrintBasicNode("ConstantExpr", e, n->type(), false);
                auto value = n->value();
                if (value.is_i64()) {
                    out += fmt::format(" {}{}\n", C(Cyan), value.as_i64());
                } else if (value.is_string()) {
                    out += fmt::format(" {}{}\n", C(Cyan), value.as_string()->value());
                } else {
                    out += " ???\n";
                }
            } break;
        }

        out += fmt::format("{}", C(Reset));
    }

    void PrintHeader(const layec::SemaNode* b) {
        if (b->is_statement())
            PrintStatementHeader(static_cast<const layec::Statement*>(b));
        else PrintExprHeader(static_cast<const layec::Expr*>(b));
    }

    void PrintStatement(const layec::Statement* s, std::string leading_text) {
        using K = layec::Statement::Kind;
        switch (s->kind()) {
            default: break;

            case K::DeclBinding: {
                auto n = as<layec::BindingDecl>(s);
                if (n->init()) {
                    layec::Expr* children[] = {n->init()};
                    PrintChildren(children, leading_text);
                }
            } break;

            case K::DeclFunction: {
                auto n = as<layec::FunctionDecl>(s);
                if (not n->body()) break;
                layec::Statement* children[] = {n->body()};
                PrintChildren(children, leading_text);
            } break;

            case K::DeclStruct: {
                auto n = as<layec::StructDecl>(s);
                auto fields = n->fields();
                auto variants = n->variants();

                std::vector<layec::Decl*> children{};
                children.insert(children.end(), fields.begin(), fields.end());
                children.insert(children.end(), variants.begin(), variants.end());
                PrintChildren(children, leading_text);
            } break;

            case K::DeclEnum: {
                auto n = as<layec::EnumDecl>(s);
                auto variants = n->variants();

                for (lcc::usz i = 0; i < variants.size(); i++) {
                    const bool last = i == variants.size() - 1;
                    out += fmt::format("{}{}{}", C(Red), leading_text, last ? "└─" : "├─");

                    auto variant = variants[i];
                    PrintBasicHeader("EnumVariant", n);

                    out += fmt::format(
                        " {}{}\n",
                        C(Green),
                        variant.name
                    );

                    if (auto init = variant.init) {
                        layec::SemaNode* children[] = {init};
                        PrintChildren(children, leading_text + (last ? "  " : "│ "));
                    }
                }
            } break;

            case K::Block: {
                auto n = cast<layec::BlockStatement>(s);
                PrintChildren(n->children(), leading_text);
            } break;

            case K::Assign: {
                auto n = as<layec::AssignStatement>(s);
                layec::Expr* children[] = {n->target(), n->value()};
                PrintChildren(children, leading_text);
            } break;

            case K::Delete: {
                auto n = as<layec::DeleteStatement>(s);
                layec::Expr* children[] = {n->expr()};
                PrintChildren(children, leading_text);
            } break;

            case K::Expr: {
                auto n = cast<layec::ExprStatement>(s);
                PrintExpr(n->expr(), leading_text);
            } break;

            case K::If: {
                auto n = as<layec::IfStatement>(s);
                layec::SemaNode* children[] = {n->condition(), n->pass(), n->fail()};
                PrintChildren(children, leading_text);
            } break;

            case K::For: {
                auto n = as<layec::ForStatement>(s);
                std::vector<layec::SemaNode*> children{};
                if (n->init()) children.push_back(n->init());
                if (n->condition()) children.push_back(n->condition());
                if (n->increment()) children.push_back(n->increment());
                if (n->pass()) children.push_back(n->pass());
                if (n->fail()) children.push_back(n->fail());
                PrintChildren(children, leading_text);
            } break;

            case K::ForEach: {
                auto n = as<layec::ForEachStatement>(s);
                std::vector<layec::SemaNode*> children{};
                if (n->sequence()) children.push_back(n->sequence());
                if (n->pass()) children.push_back(n->pass());
                if (n->fail()) children.push_back(n->fail());
                PrintChildren(children, leading_text);
            } break;

            case K::DoFor: {
                auto n = as<layec::DoForStatement>(s);
                layec::SemaNode* children[] = {n->body(), n->condition()};
                PrintChildren(children, leading_text);
            } break;

            case K::Switch: {
                auto n = as<layec::SwitchStatement>(s);
                auto cases = n->cases();

                for (lcc::usz i = 0; i < cases.size(); i++) {
                    const bool last = i == cases.size() - 1;
                    out += fmt::format("{}{}{}", C(Red), leading_text, last ? "└─" : "├─");

                    auto case_ = cases[i];

                    if (case_.is_default()) {
                        PrintBasicHeader("Default", n);
                        layec::SemaNode* children[] = {case_.body};
                        PrintChildren(children, leading_text + (last ? "  " : "│ "));
                    } else {
                        PrintBasicHeader("Case", n);
                        layec::SemaNode* children[] = {case_.value, case_.body};
                        PrintChildren(children, leading_text + (last ? "  " : "│ "));
                    }
                }
            } break;

            case K::Return: {
                auto n = as<layec::ReturnStatement>(s);
                if (n->value()) {
                    layec::SemaNode* children[] = {n->value()};
                    PrintChildren(children, leading_text);
                }
            } break;

            case K::Defer: {
                auto n = as<layec::DeferStatement>(s);
                layec::SemaNode* children[] = {n->statement()};
                PrintChildren(children, leading_text);
            } break;
        }
    }

    void PrintExpr(const layec::Expr* e, std::string leading_text) {
        using K = layec::Expr::Kind;
        switch (e->kind()) {
            default: break;

            case K::Unary: {
                auto n = as<layec::UnaryExpr>(e);
                layec::SemaNode* children[] = {n->value()};
                PrintChildren(children, leading_text);
            } break;

            case K::Binary: {
                auto n = as<layec::BinaryExpr>(e);
                layec::SemaNode* children[] = {n->lhs(), n->rhs()};
                PrintChildren(children, leading_text);
            } break;

            case K::And: {
                auto n = as<layec::AndExpr>(e);
                layec::SemaNode* children[] = {n->lhs(), n->rhs()};
                PrintChildren(children, leading_text);
            } break;

            case K::Or: {
                auto n = as<layec::OrExpr>(e);
                layec::SemaNode* children[] = {n->lhs(), n->rhs()};
                PrintChildren(children, leading_text);
            } break;

            case K::Xor: {
                auto n = as<layec::XorExpr>(e);
                layec::SemaNode* children[] = {n->lhs(), n->rhs()};
                PrintChildren(children, leading_text);
            } break;

            case K::UnwrapNilable: {
                auto n = as<layec::UnwrapNilableExpr>(e);
                layec::SemaNode* children[] = {n->value()};
                PrintChildren(children, leading_text);
            } break;

            case K::LookupName: {
                auto n = as<layec::NameExpr>(e);
                (void) n;
            } break;

            case K::LookupPath: {
                auto n = as<layec::PathExpr>(e);
                (void) n;
            } break;

            case K::FieldIndex: {
                auto n = as<layec::FieldIndexExpr>(e);
                layec::SemaNode* children[] = {n->target()};
                PrintChildren(children, leading_text);
            } break;

            case K::ValueIndex: {
                auto n = as<layec::ValueIndexExpr>(e);
                auto indices = n->indices();
                if (indices.empty()) {
                    layec::Expr* children[] = {n->target()};
                    PrintChildren(children, leading_text);
                } else {
                    std::vector<layec::Expr*> children{};
                    children.push_back(n->target());
                    children.insert(children.end(), indices.begin(), indices.end());
                    PrintChildren(children, leading_text);
                }
            } break;

            case K::Slice: {
                auto n = as<layec::SliceExpr>(e);
                std::vector<layec::Expr*> children{};
                children.push_back(n->target());
                if (n->offset()) children.push_back(n->offset());
                if (n->length()) children.push_back(n->length());
                PrintChildren(children, leading_text);
            } break;

            case K::Call: {
                auto n = as<layec::CallExpr>(e);
                auto args = n->args();
                if (args.empty()) {
                    layec::Expr* children[] = {n->target()};
                    PrintChildren(children, leading_text);
                } else {
                    std::vector<layec::Expr*> children{};
                    children.push_back(n->target());
                    children.insert(children.end(), args.begin(), args.end());
                    PrintChildren(children, leading_text);
                }
            } break;

            case K::Ctor: {
                auto n = as<layec::CtorExpr>(e);
                auto inits = n->inits();

                for (lcc::usz i = 0; i < inits.size(); i++) {
                    const bool last = i == inits.size() - 1;
                    out += fmt::format("{}{}{}", C(Red), leading_text, last ? "└─" : "├─");

                    auto init = inits[i];
                    PrintBasicHeader("CtorFieldInit", n);
                    out += "\n";

                    layec::SemaNode* children[] = {init.value};
                    PrintChildren(children, leading_text + (last ? "  " : "│ "));
                }
            } break;

            case K::Not: {
                auto n = as<layec::NotExpr>(e);
                layec::SemaNode* children[] = {n->value()};
                PrintChildren(children, leading_text);
            } break;

            case K::Cast: {
                auto n = as<layec::CastExpr>(e);
                layec::SemaNode* children[] = {n->value()};
                PrintChildren(children, leading_text);
            } break;

            case K::New: {
                auto n = as<layec::NewExpr>(e);
                auto inits = n->inits();

                PrintChildren(n->args(), leading_text, inits.size() == 0);

                for (lcc::usz i = 0; i < inits.size(); i++) {
                    const bool last = i == inits.size() - 1;
                    out += fmt::format("{}{}{}", C(Red), leading_text, last ? "└─" : "├─");

                    auto init = inits[i];
                    PrintBasicHeader("CtorFieldInit", n);
                    out += "\n";

                    layec::SemaNode* children[] = {init.value};
                    PrintChildren(children, leading_text + (last ? "  " : "│ "));
                }
            } break;

            case K::Try: {
                auto n = as<layec::TryExpr>(e);
                layec::Expr* children[] = {n->value()};
                PrintChildren(children, leading_text);
            } break;

            case K::Catch: {
                auto n = as<layec::CatchExpr>(e);
                layec::SemaNode* children[] = {n->value(), n->body()};
                PrintChildren(children, leading_text);
            } break;

            case K::Do: {
                auto n = as<layec::DoExpr>(e);
                PrintChildren(n->statements(), leading_text);
            } break;

            case K::Constant: {
                auto n = as<layec::ConstantExpr>(e);
                layec::SemaNode* children[] = {n->expr()};
                PrintChildren(children, leading_text);
            } break;
        }
    }

    void operator()(const layec::SemaNode* b, std::string leading_text = "") {
        PrintHeader(b);
        if (b->is_statement())
            PrintStatement(static_cast<const layec::Statement*>(b), leading_text);
        else PrintExpr(static_cast<const layec::Expr*>(b), leading_text);
    }
};
} // namespace

void layec::Module::print() {
    ASTPrinter p{true};
    p.out += fmt::format("{};; Laye module -- {}\n", p.C(ASTPrinter::White), file()->path().string());
    for (auto* node : _top_level_decls) p(node);
}

void layec::LayeContext::print_modules() {
    for (auto module : _modules) {
        module.second->print();
    }
}
