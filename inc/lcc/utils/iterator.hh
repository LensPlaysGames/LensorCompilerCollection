#ifndef LCC_ITERATOR_HH
#define LCC_ITERATOR_HH

#include <lcc/utils.hh>

#include <cstddef>
#include <iterator>
#include <memory>
#include <type_traits>
#include <vector>

// If debug iterators haven't been asked for, but we are doing a debug
// build, use debug iterators.
#if ! defined(LCC_DEBUG_ITERATORS) && ! defined(NDEBUG)
#    define LCC_DEBUG_ITERATORS
#endif

namespace lcc::utils {
#if defined(LCC_DEBUG_ITERATORS)

/// An iterator that checks for errors.
template <typename ContainerType, typename IterType>
requires (
    not std::is_reference_v<ContainerType>
    and not std::is_reference_v<IterType>
)
struct CheckedIterator {
private:
    /// The container we’re iterating over.
    ContainerType* container;

    /// The current iterator value.
    IterType it;

    /// The end iterator.
    IterType end;

    /// Check for iterator invalidation.
    ///
    /// Note that there hopefully shouldn’t be a way for the begin iterator
    /// to move but for the end iterator to stay the same, so we don’t check
    /// for that.
    void Check() {
        LCC_ASSERT(
            container->end() == end,
            "Iterator invalidation: End iterator has changed"
        );

        LCC_ASSERT(
            std::ssize(*container) == std::distance(end, it),
            "Iterator invalidation: Container size has changed"
        );
    }

public:
    CheckedIterator(ContainerType& c, IterType it_)
        : container(std::addressof(c)), it(it_) {
        end = container->end();
    }

    /// Get the current iterator value.
    auto operator*() -> decltype(*it) {
        LCC_ASSERT(it != end, "Dereferencing end iterator");
        Check();
        return *it;
    }

    /// Advance the iterator.
    CheckedIterator& operator++() {
        LCC_ASSERT(it != end, "Advancing end iterator");
        Check();
        ++it;
        return *this;
    }

    /// Compare two iterators.
    bool operator==(const CheckedIterator& other) const {
        LCC_ASSERT(
            container == other.container,
            "Comparing iterators from different containers"
        );
        Check();
        return it == other.it;
    }

    /// Add an offset to the iterator.
    CheckedIterator operator+(std::ptrdiff_t offset) const {
        Check();
        auto ret = *this;
        ret.it += offset;
        LCC_ASSERT(
            ret.it <= end,
            "Adding offset to iterator would move past end"
        );
        return ret;
    }

    /// Subtract an offset from the iterator.
    CheckedIterator operator-(std::ptrdiff_t offset) const {
        Check();
        auto ret = *this;
        ret.it -= offset;
        LCC_ASSERT(
            ret.it >= container->begin(),
            "Subtracting offset from iterator would move before begin"
        );
        return ret;
    }
};

/// Checked iterator for vectors.
template <typename T>
using VectorIterator
    = CheckedIterator<
        std::vector<T>,
        typename std::vector<T>::iterator>;

/// Checked const iterator for vectors.
template <typename T>
using VectorConstIterator
    = CheckedIterator<
        const std::vector<T>,
        typename std::vector<T>::const_iterator>;

#else
template <typename IterType>
using CheckedIterator = IterType;

template <typename ContainerType, typename IterType>
requires (
    not std::is_reference_v<ContainerType>
    and not std::is_reference_v<IterType>
)
struct PassthroughIterator : public IterType {
    PassthroughIterator(ContainerType& _, IterType it_)
        : IterType(it_) {}
};

template <typename T>
using VectorIterator
    = PassthroughIterator<
        std::vector<T>,
        typename std::vector<T>::iterator>;

template <typename T>
using VectorConstIterator
    = PassthroughIterator<
        const std::vector<T>,
        typename std::vector<T>::const_iterator>;
#endif
} // namespace lcc::utils

#endif // LCC_ITERATOR_HH
