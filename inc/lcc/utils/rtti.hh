#ifndef LCC_RTTI_HH
#define LCC_RTTI_HH

#include <lcc/utils.hh>

namespace lcc::detail {
// Check that an object is a pointer to a class type.
template <typename Type>
concept ClassPointer = std::is_pointer_v<std::remove_reference_t<Type>>
                   and std::is_class_v<std::remove_pointer_t<std::remove_reference_t<Type>>>;

// Return const To if either From or To is const.
template <typename From, typename To>
using merge_const = std::conditional_t<std::is_const_v<From>, std::add_const_t<To>, To>;

// This function implements (checked) casting between types.
template <bool checked, typename Target, typename Value>
auto cast_impl(Value&& value) {
    // Make sure Target is at most const-qualified.
    static_assert(
        std::is_same_v<std::remove_cvref_t<Target>, std::remove_const_t<Target>>,
        "Target type of class may at most be const-qualified"
    );

    // Make sure Target is a class type.
    static_assert(
        std::is_class_v<Target>,
        "Target type of cast must be a (const-qualified) class type"
    );

    // Value must be a pointer type.
    static_assert(
        std::is_pointer_v<std::remove_cvref_t<Value>>,
        "Argument of cast function must be a pointer to a class type"
    );

    // Strip references and one level of pointers.
    using ClassType = std::remove_pointer_t<std::remove_reference_t<Value>>;

    // Class type must be a class.
    static_assert(
        std::is_class_v<ClassType>,
        "Value type of cast must be a (const-qualified) pointer or to a class type"
    );

    // Result type is a (const) pointer to Target.
    using ResultType = merge_const<ClassType, Target>*;

    // If the types are the same, or if Target is a base class of the
    // value class, just cast to the target type.
    if constexpr (std::is_same_v<ClassType, Target> or std::is_base_of_v<Target, ClassType>)
        return static_cast<ResultType>(value);

    // If To is a derived class of From, then, if the dynamic type
    // of From is-a To, perform the cast, otherwise, return null.
    else if constexpr (std::is_base_of_v<ClassType, Target>) {
        LCC_ASSERT(value, "Cannot perform dynamic cast from null");

        // If the dynamic types match, return the cast value.
        if (Target::classof(value))
            return static_cast<ResultType>(value);

        // Otherwise, issue an error if requested.
        if constexpr (checked)
            LCC_ASSERT(false, "Unexpected dynamic type");

        // And return nullptr.
        return static_cast<ResultType>(nullptr);
    }

    // Otherwise, if From and To are not related at all, then this
    // is a compile-time error.
    else {
        static_assert(
            always_false<Target>,
            "Cannot cast between unrelated types"
        );
    }
}
} // namespace lcc::detail

namespace lcc {
/// \brief Cast a value to a target type.
///
/// The \c Target type must be a (possibly const-qualified) class
/// type, and the value must be a pointer to a class type; this
/// function checks if the dynamic type of the value is-a \c Target,
/// and if so, returns a pointer to the value cast to \c Target.
///
/// If the conversion is known to be safe at compile time (e.g.
/// a cast from a derived class to the base class), then no runtime
/// checks are performed.
///
/// If the conversion is impossible, e.g. because the types are
/// unrelated, then this is a compile-time error.
///
/// If either \c Target or value is const, the resulting pointer will
/// be a \c const \c Target* instead of just a \c Target*.
///
/// \tparam Target The type to cast to.
/// \param value The value to cast.
/// \return A pointer to the value cast to \c Target, or \c nullptr if
///         the dynamic type of the value is-not-a \c Target.
///
/// \see as()
/// \see is()
template <typename Target, detail::ClassPointer Object>
auto cast(Object&& value) { return detail::cast_impl<false, Target>(value); }

/// \brief Perform a checked cast to a target type.
///
/// This performs the same operation as \c cast(), except
/// that it terminates the program if the cast fails.
///
/// \tparam Target The type to cast to.
/// \param value The value to cast.
/// \return A pointer to the value cast to \c Target.
///
/// \see cast()
/// \see is()
template <typename Target, detail::ClassPointer Object>
auto as(Object&& value) { return detail::cast_impl<true, Target>(value); }

/// \brief Check if the dynamic type of a value is one of a set of types.
///
/// This function performs the same operation as \c cast(), except
/// that it tries to cast to each of the types in the parameter pack
/// in turn, and returns \c true if any of the casts succeed.
///
/// \tparam Types The types to check for.
/// \param value The value to check.
/// \return \c true if the dynamic type of \c value is one of the
///         types in \c Types, \c false otherwise.
///
/// \see as()
/// \see cast()
template <typename... Types, detail::ClassPointer Object>
bool is(Object&& value) { return (bool(detail::cast_impl<false, Types>(value)) or ...); }
} // namespace lcc

#endif // LCC_RTTI_HH
