#include <lcc/context.hh>
#include <lcc/core.hh>
#include <lcc/utils.hh>
#include <lcc/utils/macros.hh>

#include <glint/ast.hh>
#include <glint/error_ids.hh>
#include <glint/module_description.hh>
#include <glint/parser.hh>
#include <glint/sema.hh>

template <bool PerformConversion>
auto lcc::glint::Sema::ConvertImpl(
    lcc::glint::Expr** expr_ptr,
    lcc::glint::Type* to
) -> int {
    LCC_ASSERT(
        expr_ptr and *expr_ptr and to,
        "Pointers mustn't be null"
    );

    enum : int {
        TypesContainErrors = -2,
        ConversionImpossible = -1,
        NoOp = 0,
    };

    // Caching "from" always caused a whole bunch of problems so this is the
    // never-cache solution while still providing a nice name
#define from ((*expr_ptr)->type())

    // Cannot convert if the any of types contain errors or are unknown.
    if (
        from->is_unknown() or from->sema_errored()
        or to->is_unknown() or to->sema_errored()
    ) return TypesContainErrors;

    // This score variable is here mostly so we don't forget we performed
    // lvalue to rvalue conversion.
    int score = 0;
    auto Score = [&](int i) {
        LCC_ASSERT(i, "Score must be 1 or greater. Use the enum constants above for values <= 0");
        return i + int(score);
    };

    // Any type can be converted to void.
    if (to->is_void())
        return NoOp;

    // Any type can be converted to auto.
    if (TemplatedFuncDecl::is_auto(*to))
        return NoOp;

    // Any type can be converted to itself.
    if (Type::Equal(from, to)) {
        // lvalue expression must be converted to rvalue if we want a value of the given type.
        if ((*expr_ptr)->is_lvalue()) {
            if constexpr (PerformConversion)
                LValueToRValue(expr_ptr);
            return Score(1);
        }

        return NoOp;
    }

    // Casting to a supplanted member can be done from an lvalue.
    // TODO: When sum types are supplant-able, add that here too.
    if (
        from->strip_references()->is_struct()
        and to->strip_references()->is_struct()
    ) {
        auto from_stripped = from->strip_pointers_and_references();
        auto members = as<StructType>(from_stripped)->members();
        for (auto m : members) {
            if (m.supplanted and Type::Equal(m.type, to->strip_references())) {
                if constexpr (PerformConversion) {
                    *expr_ptr = new (mod) MemberAccessExpr(
                        *expr_ptr,
                        m.name,
                        from->location()
                    );
                    (void) Analyse(expr_ptr);
                }
                return Score(1);
            }
        }
    }

    // Casting to an array view can be done from an lvalue.

    // Fixed Array to Array View
    bool from_array_ref = (from->is_pointer() or from->is_reference())
                      and from->elem()->is_array();
    if ((from->is_array() or from_array_ref) and to->is_view()) {
        auto* from_elem = from->elem();
        if (from_array_ref)
            from_elem = from_elem->elem();

        // TODO: If fixed array isn't an lvalue, then we need to create a
        // temporary and everything for it (I think).

        // Underlying types have to be convertible. This checks if they are equal,
        // since we can only check if convertible if we have an expression (which
        // we don't). Gotta change stupid API built by stupid people.
        if (not Type::Equal(from_elem, to->elem()))
            return ConversionImpossible;

        if constexpr (PerformConversion)
            InsertImplicitCast(expr_ptr, to);
        return Score(1);
    }

    // Dynamic Array to Array View
    // TODO: Handle .ptr, .ref variations
    if (from->is_dynamic_array() and to->is_view()) {
        if (not Type::Equal(from->elem(), to->elem()))
            return ConversionImpossible;

        if constexpr (PerformConversion)
            InsertImplicitCast(expr_ptr, to);

        return Score(1);
    }

    // All conversions beside reference binding require lvalue-to-rvalue conversion.
    if (to->is_reference() and Type::Equal(from, to->elem())) {
        if ((*expr_ptr)->is_lvalue()) {
            if constexpr (PerformConversion)
                WrapWithCast(expr_ptr, to, CastKind::LValueToReference);
            return NoOp;
        }

        return ConversionImpossible;
    }

    // Lvalue to rvalue conversion is required.
    score += (*expr_ptr)->is_lvalue();
    if constexpr (PerformConversion)
        LValueToRValue(expr_ptr, false);

    // Get reference-to-reference conversions out of the way early.
    if (from->is_reference() and to->is_reference()) {
        // A reference can be converted to the same reference.
        if (Type::Equal(from, to)) return NoOp;

        // References to arrays can be converted to references to
        // the first element.
        auto* arr = cast<ArrayType>(from->elem());
        if (arr and Type::Equal(arr->element_type(), to->elem())) {
            if constexpr (PerformConversion)
                InsertImplicitCast(expr_ptr, to);
            return Score(1);
        }

        return ConversionImpossible;
    }

    // Strip reference from `from` if need be.
    if (cast<ReferenceType>(from)) {
        score += 1;
        if constexpr (PerformConversion)
            LValueToRValue(expr_ptr);
    }

    // Function types can be converted to their corresponding function pointer
    // types.
    if (
        from->is_function() and to->is_pointer()
        and Type::Equal(to->elem(), from)
    ) {
        if constexpr (PerformConversion)
            InsertImplicitCast(expr_ptr, to);
        return NoOp;
    }

    // Try deproceduring (convert a function into a call to that function).
    if (Deproceduring(expr_ptr))
        return Score(1);

    // Now check if the types are equal. In many cases, lvalue-to-rvalue
    // conversion is all we need.
    if (Type::Equal(from, to))
        return NoOp;

    // Pointer to pointer conversions.
    if (from->is_pointer() and to->is_pointer()) {
        /// Pointers to arrays are convertible to pointers to the first element.
        auto* arr = cast<ArrayType>(from->elem());
        if (arr and Type::Equal(arr->element_type(), to->elem())) {
            if constexpr (PerformConversion)
                InsertImplicitCast(expr_ptr, to);
            return Score(1);
        }

        /// Any pointer is convertible to `@void`.
        if (Type::Equal(to, Type::VoidPtr)) {
            if constexpr (PerformConversion)
                InsertImplicitCast(expr_ptr, to);
            return Score(1);
        }
    }

    // Array to array conversions.
    // FIXME: We kind of need to check that the base types are convertible,
    // but, uhhh, we can't really do that right now without an expression of
    // that type due to how Convert works ... I wonder what idiot built it
    // that way.

    // Fixed Array to Fixed Array
    if (from->is_array() and to->is_array()) {
        auto* from_arr = as<ArrayType>(from);
        auto* to_arr = as<ArrayType>(to);

        // If the array we are converting from is larger than the resulting array,
        // it wouldn't fit and that conversion is impossible.
        if (from_arr->dimension() > to_arr->dimension())
            return ConversionImpossible;
        if (not Type::Equal(from_arr->element_type(), to_arr->element_type()))
            return ConversionImpossible;

        if constexpr (PerformConversion)
            InsertImplicitCast(expr_ptr, to);
        return Score(1);
    }

    // Fixed Array to Dynamic Array
    // TODO: We probably want to disallow this being an implicit cast (but
    // explicit should be allowed).
    if (from->is_array() and to->is_dynamic_array()) {
        if (not Type::Equal(from->elem(), to->elem()))
            return ConversionImpossible;

        if constexpr (PerformConversion)
            InsertImplicitCast(expr_ptr, to);
        return Score(1);
    }

    // Function types can be converted to their corresponding function types.
    if (
        from->is_function() and to->is_pointer()
        and Type::Equal(to->elem(), from)
    ) {
        if constexpr (PerformConversion)
            InsertImplicitCast(expr_ptr, to);
        return NoOp;
    }

    // Integer to boolean and vis versa implicit conversions.
    if (
        (from->is_integer() and to->is_bool())
        or (from->is_bool() and to->is_integer())
    ) {
        if constexpr (PerformConversion)
            InsertImplicitCast(expr_ptr, to);
        return Score(1);
    }

    // FIXME: I'm pretty sure enum to enum should never happen, since sema
    // should lower every enum access to it's underlying type?
    // TODO: Handle enum to underlying
    if (is<EnumType>(from)) {
        // TODO: If underlying type is convertible to `to` type, not just equal check.
        // I think we may be able to "attempt" that via changing the type of
        // `from` to the underlying type (possible via inserting an
        // implicit cast) and then calling Convert on that.
        if (Type::Equal(from->elem(), to)) {
            if constexpr (PerformConversion)
                InsertImplicitCast(expr_ptr, to);
            return NoOp;
        }

        return ConversionImpossible;
    }

    // Integer to integer
    //
    // For portability, we would ideally not make any assumptions about
    // the size of `int`, but the issue with that is that it would make
    // most code rather cumbersome to write as you’d have to, e.g., cast
    // an `i16` to `int` manually. C FFI types suffer from similar problems,
    // so we just use their width on the target.
    if (from->is_integer() and to->is_integer()) {
        // Integer types are always convertible to each other if the value is
        // known at compile time and in range for the type it is being converted
        // to.
        EvalResult res;
        if ((*expr_ptr)->evaluate(context, res, false)) {
            // Note: We currently don’t support integer constants larger than 64
            // bits internally, so if the type has a bit width larger than 64, it
            // will always fit.
            auto val = res.as_int();

            // Signed to Unsigned Conversion
            if (val.slt(0) and to->is_unsigned_int(context))
                return ConversionImpossible;

            // Unsigned to Unsigned Conversion
            auto bits = to->size(context);
            if (
                from->is_unsigned_int(context)
                and bits < 64
                and val > u64(utils::MaxBitValue(bits))
            ) return ConversionImpossible;

            if constexpr (PerformConversion) {
                InsertImplicitCast(expr_ptr, to);
                *expr_ptr = new (mod) ConstantExpr(*expr_ptr, res);
            }
            return Score(1);
        }

        // Otherwise, if not known at compile-time, we will just go by what
        // doesn't cause a memory error. If it fits, it ships.
        if (from->size(context) <= to->size(context)) {
            if constexpr (PerformConversion)
                InsertImplicitCast(expr_ptr, to);
            return Score(1);
        }

        return ConversionImpossible;
    }

    // Try deproceduring one last time.
    if (Deproceduring(expr_ptr)) return Score(1);

#undef from

    return ConversionImpossible;
}

auto lcc::glint::Sema::Convert__RemoveReferences(Expr** expr) -> bool {
    LCC_ASSERT(expr and *expr);

    if (is<ReferenceType>((*expr)->type())) {
        /// Don’t strip reference here since we want an lvalue.
        LValueToRValue(expr, false);
        WrapWithCast(
            expr,
            as<TypeWithOneElement>((*expr)->type())->element_type(),
            CastKind::ReferenceToLValue
        );
    }

    return (*expr)->is_lvalue();
}

bool lcc::glint::Sema::ConvertOrError(Expr** expr, Type* to) {
    LCC_ASSERT(expr and to, "Pointers mustn't be null");
    if (not Convert(expr, to)) {
        Error(
            (*expr)->location(),
            "Expression is not convertible to type {}",
            to
        );
        (*expr)->set_sema_errored();
        return false;
    }
    return true;
}

auto lcc::glint::Sema::ConvertToCommonType(Expr** a, Expr** b) -> bool {
    LCC_ASSERT(a and b, "Pointers mustn't be null");
    // An integer literal should always be converted into the type of the
    // other side STRIPPED OF REFERENCES.
    bool a_is_literal = is<IntegerLiteral>(*a);
    bool b_is_literal = is<IntegerLiteral>(*b);
    bool both_literals = a_is_literal and b_is_literal;
    if (not both_literals) {
        if (a_is_literal)
            return Convert(a, (*b)->type()->strip_references())
                or Convert(b, (*a)->type());
        if (b_is_literal)
            return Convert(b, (*a)->type()->strip_references())
                or Convert(a, (*b)->type());
    }
    return Convert(a, (*b)->type())
        or Convert(b, (*a)->type());
}

auto lcc::glint::Sema::TryConvert(Expr** expr, Type* type) -> ConversionStatus {
    return {
        .score = ConvertImpl<false>(expr, type)
    };
}

auto lcc::glint::Sema::Convert(Expr** expr, Type* type) -> bool {
    LCC_ASSERT(expr and type);
    if ((*expr)->sema_errored()) return true;
    return ConvertImpl<true>(expr, type) >= 0;
}
