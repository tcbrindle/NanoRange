// nanorange/detail/iterator/concepts.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_DETAIL_ITERATOR_CONCEPTS_HPP_INCLUDED
#define NANORANGE_DETAIL_ITERATOR_CONCEPTS_HPP_INCLUDED

#include <nanorange/detail/concepts/object.hpp>
#include <nanorange/detail/iterator/associated_types.hpp>
#include <nanorange/detail/iterator/traits.hpp>

NANO_BEGIN_NAMESPACE

// [iterators.concept.readable]
namespace detail {

struct readable_concept {
    template <typename In>
    auto requires_() -> decltype(
        std::declval<iter_value_t<In>>(),
        std::declval<iter_reference_t<In>>(),
        std::declval<iter_rvalue_reference_t<In>>()
    );

    template <typename>
    static auto test(long) -> std::false_type;

    template <typename In>
    static auto test(int) -> std::enable_if_t<
        detail::requires_<readable_concept, In> &&
        common_reference_with<iter_reference_t<In>&&, iter_value_t<In>&> &&
        common_reference_with<iter_reference_t<In>&&, iter_rvalue_reference_t<In>&&> &&
        common_reference_with<iter_rvalue_reference_t<In>&&, const iter_value_t<In>&>,
        std::true_type>;
};

} // namespace detail

template <typename In>
NANO_CONCEPT readable = decltype(detail::readable_concept::test<In>(0))::value;

// [iterator.concept.writable]
namespace detail {

struct writable_concept {
    template <typename Out, typename T>
    auto requires_(Out&& o, T&& t) -> decltype(
        *o = std::forward<T>(t),
        *std::forward<Out>(o) = std::forward<T>(t),
        const_cast<const iter_reference_t<Out>&&>(*o) = std::forward<T>(t),
        const_cast<const iter_reference_t<Out>&&>(*std::forward<Out>(o)) =
            std::forward<T>(t));
};

} // namespace detail

template <typename Out, typename T>
NANO_CONCEPT writable = detail::requires_<detail::writable_concept, Out, T>;

// [iterator.concept.weaklyincrementable]

namespace detail {

template <typename T>
inline constexpr bool is_integer_like = integral<T>;

template <typename T>
inline constexpr bool is_signed_integer_like = signed_integral<T>;

struct weakly_incrementable_concept {
    template <typename I>
    auto requires_(I i) -> decltype(
        std::declval<iter_difference_t<I>>(),
        requires_expr<is_signed_integer_like<iter_difference_t<I>>>{},
        requires_expr<same_as<decltype(++i), I&>>{},
        i++
    );

};

} // namespace detail

template <typename I>
NANO_CONCEPT weakly_incrementable = default_initializable<I> && movable<I> &&
    detail::requires_<detail::weakly_incrementable_concept, I>;

// [iterator.concept.incrementable]
namespace detail {

struct incrementable_concept {
    template <typename I>
    auto requires_(I i) -> decltype(
        requires_expr<same_as<decltype(i++), I>>{}
    );
};

} // namespace detail

template <typename I>
NANO_CONCEPT incrementable = regular<I> && weakly_incrementable<I> &&
    detail::requires_<detail::incrementable_concept, I>;

// [iterator.concept.iterator]

namespace detail {

struct input_or_output_iterator_concept {
    template <typename I>
    auto requires_(I i) -> decltype(
            requires_expr<can_reference<decltype(*i)>>{});
};

} // namespace detail

template <typename I>
NANO_CONCEPT input_or_output_iterator =
    detail::requires_<detail::input_or_output_iterator_concept, I> &&
    weakly_incrementable<I>;

// [iterator.concept.sentinel]

template <typename S, typename I>
NANO_CONCEPT sentinel_for =
    semiregular<S> && input_or_output_iterator<I> && 
    detail::weakly_equality_comparable_with<S, I>;

// [iterator.concept.sizedsentinel]

template <typename S, typename I>
inline constexpr bool disable_sized_sentinel = false;

namespace detail {

struct sized_sentinel_for_concept {
    template <typename S, typename I>
    auto requires_(const S& s, const I& i)
        -> decltype(requires_expr<same_as<decltype(s - i), iter_difference_t<I>>>{},
                    requires_expr<same_as<decltype(i - s), iter_difference_t<I>>>{});
};

} // namespace detail

template <typename S, typename I>
NANO_CONCEPT sized_sentinel_for =
    sentinel_for<S, I> &&
    !disable_sized_sentinel<std::remove_cv_t<S>, std::remove_cv_t<I>> &&
    detail::requires_<detail::sized_sentinel_for_concept, S, I>;

// This is a hack, but I'm fed up with my tests breaking because GCC
// has a silly extension
template <typename S>
NANO_CONCEPT sized_sentinel_for<S, void*> = false;

template <typename I>
NANO_CONCEPT sized_sentinel_for<void*, I> = false;

template <>
NANO_CONCEPT sized_sentinel_for<void*, void*> = false;

// [iterator.concept.input]

namespace detail {

// FIXME: Use ITER_CONCEPT, not iterator_category_t
struct input_iterator_concept {
    template <typename I>
    auto requires_() -> iterator_category_t<I>;

    template <typename>
    static auto test(long) -> std::false_type;

    template <typename I>
    static auto test(int) -> std::enable_if_t<
        input_or_output_iterator<I> &&
        readable<I> &&
        detail::requires_<input_iterator_concept, I> &&
        derived_from<iterator_category_t<I>, input_iterator_tag>,
        std::true_type>;
};

}

template <typename I>
NANO_CONCEPT input_iterator =
    decltype(detail::input_iterator_concept::test<I>(0))::value;

// [iterator.concept.output]

namespace detail {

struct output_iterator_concept {
    template <typename I, typename T>
    auto requires_(I i, T&& t) -> decltype(
        *i++ = std::forward<T>(t)
    );
};

} // namespace detail

template <typename I, typename T>
NANO_CONCEPT output_iterator =
    input_or_output_iterator<I> &&
    writable<I, T> &&
    detail::requires_<detail::output_iterator_concept, I, T>;

// [ranges.iterators.forward]

namespace detail {

struct forward_iterator_concept {
    template <typename>
    static auto test(long) -> std::false_type;

    template <typename I>
    static auto test(int) -> std::enable_if_t<
        input_iterator<I> &&
        derived_from<iterator_category_t<I>, forward_iterator_tag> &&
        incrementable<I> &&
        sentinel_for<I, I>,
        std::true_type>;


};

}

template <typename I>
NANO_CONCEPT forward_iterator =
    decltype(detail::forward_iterator_concept::test<I>(0))::value;

// [iterator.concept.bidir]
namespace detail {

struct bidirectional_iterator_concept {
    template <typename I>
    auto requires_(I i) -> decltype(
        requires_expr<same_as<decltype(--i), I&>>{},
        requires_expr<same_as<decltype(i--), I>>{}
    );

    template <typename>
    static auto test(long) -> std::false_type;

    template <typename I>
    static auto test(int) -> std::enable_if_t<
        forward_iterator<I> &&
        derived_from<iterator_category_t<I>, bidirectional_iterator_tag> &&
        detail::requires_<bidirectional_iterator_concept, I>,
        std::true_type>;
};

} // namespace detail

template <typename I>
NANO_CONCEPT bidirectional_iterator =
    decltype(detail::bidirectional_iterator_concept::test<I>(0))::value;

// [iterator.concept.random.access]

namespace detail {

struct random_access_iterator_concept {
    template <typename>
    static auto test(long) -> std::false_type;

    template <typename I>
    static auto test(int) -> std::enable_if_t<
        bidirectional_iterator<I> &&
        derived_from<iterator_category_t<I>, random_access_iterator_tag> &&
        totally_ordered<I> &&
        sized_sentinel_for<I, I> &&
        detail::requires_<random_access_iterator_concept, I>,
        std::true_type>;

    template <typename I>
    auto requires_(I i, const I j, const iter_difference_t<I> n) -> decltype(
        requires_expr<same_as<decltype(i += n), I&>>{},
        requires_expr<same_as<decltype(j +  n), I>>{},
#ifndef _MSC_VER
        requires_expr<same_as<decltype(n +  j), I>>{}, // FIXME: MSVC doesn't like this when I = int*
#endif
        requires_expr<same_as<decltype(i -= n), I&>>{},
        requires_expr<same_as<decltype(j -  n), I>>{},
        requires_expr<same_as<decltype(j[n]), iter_reference_t<I>>>{}
    );
};

} // namespace detail

template <typename I>
NANO_CONCEPT random_access_iterator =
        decltype(detail::random_access_iterator_concept::test<I>(0))::value;

namespace detail {

struct contiguous_iterator_concept {
    template <typename>
    static auto test(long) -> std::false_type;

    template <typename I>
    static auto test(int) -> std::enable_if_t<
        random_access_iterator<I> &&
        derived_from<iterator_category_t<I>, contiguous_iterator_tag> &&
        std::is_lvalue_reference_v<iter_reference_t<I>> &&
        same_as<iter_value_t<I>, remove_cvref_t<iter_reference_t<I>>>,
        std::true_type>;
};

}

template <typename I>
NANO_CONCEPT contiguous_iterator =
    decltype(detail::contiguous_iterator_concept::test<I>(0))::value;

NANO_END_NAMESPACE

#endif
