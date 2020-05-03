// nanorange/detail/ranges/concepts.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_DETAIL_RANGES_CONCEPTS_HPP_INCLUDED
#define NANORANGE_DETAIL_RANGES_CONCEPTS_HPP_INCLUDED

#include <nanorange/detail/ranges/basic_range_types.hpp>
#include <nanorange/detail/ranges/begin_end.hpp>
#include <nanorange/detail/ranges/borrowed_range.hpp>
#include <nanorange/detail/ranges/primitives.hpp>
#include <nanorange/detail/ranges/range_concept.hpp>

#include <initializer_list>

// Avoid dragging in the large <set> and <unordered_set> headers
// This is technically undefined behaviour: define the symbol
// NANORANGE_NO_STD_FORWARD_DECLARATIONS
// to enforce standard-compliant mode
#ifndef NANORANGE_NO_STD_FORWARD_DECLARATIONS
NANO_BEGIN_NAMESPACE_STD
template <typename, typename> class basic_string_view;
template <typename, typename, typename> class set;
template <typename, typename, typename> class multiset;
template <typename, typename, typename, typename> class unordered_set;
template <typename, typename, typename, typename> class unordered_multiset;
template <typename, typename> class match_results;
NANO_END_NAMESPACE_STD
#else
#include <string_view>
#include <regex>
#include <set>
#include <unordered_set>
#endif

NANO_BEGIN_NAMESPACE

template <typename T>
NANO_CONCEPT borrowed_range = range<T> &&
    (std::is_lvalue_reference_v<T> || enable_borrowed_range<remove_cvref_t<T>>);

// Special-case std::string_view
template <typename CharT, typename Traits>
inline constexpr bool
    enable_borrowed_range<std::basic_string_view<CharT, Traits>> = true;


// [range.sized]
namespace detail {

struct sized_range_concept {
    template <typename T>
    auto requires_(T& t) -> decltype(ranges::size(t));
};

} // namespace detail

template <typename T>
NANO_CONCEPT sized_range =
    range<T> &&
    !disable_sized_range<remove_cvref_t<T>> &&
    detail::requires_<detail::sized_range_concept, T>;


// [range.views]
struct view_base { };

namespace detail {

template <typename>
inline constexpr bool is_std_non_view = false;

template <typename T>
inline constexpr bool is_std_non_view<std::initializer_list<T>> = true;

template <typename K, typename C, typename A>
inline constexpr bool is_std_non_view<std::set<K, C, A>> = true;

template <typename K, typename C, typename A>
inline constexpr bool is_std_non_view<std::multiset<K, C, A>> = true;

template <typename K, typename H, typename E, typename A>
inline constexpr bool is_std_non_view<std::unordered_set<K, H, E, A>> = true;

template <typename K, typename H, typename E, typename A>
inline constexpr bool is_std_non_view<std::unordered_multiset<K, H, E, A>> = true;

template <typename B, typename A>
inline constexpr bool is_std_non_view<std::match_results<B, A>> = true;

template <typename T>
constexpr bool enable_view_helper()
{
    if constexpr (derived_from<T, view_base>) {
        return true;
    } else if constexpr (is_std_non_view<T>) {
        return false;
    } else if constexpr (range<T> && range<const T>) {
        return same_as<range_reference_t<T>, range_reference_t<const T>>;
    } else {
        return true;
    }
}

}

template <typename T>
inline constexpr bool enable_view = detail::enable_view_helper<T>();

template <typename T>
NANO_CONCEPT view = range<T> && semiregular<T> && enable_view<T>;

// [range.refinements]
namespace detail {

struct output_range_concept {
    template <typename, typename>
    static auto test(long) -> std::false_type;

    template <typename R, typename T>
    static auto test(int) -> std::enable_if_t<
        range<R> && output_iterator<iterator_t<R>, T>,
        std::true_type>;
};

}

template <typename R, typename T>
NANO_CONCEPT output_range =
    decltype(detail::output_range_concept::test<R, T>(0))::value;

namespace detail {

struct input_range_concept {
    template <typename>
    static auto test(long) -> std::false_type;

    template <typename T>
    static auto test(int) -> std::enable_if_t<
        range<T> && input_iterator<iterator_t<T>>,
        std::true_type>;
};

}

template <typename T>
NANO_CONCEPT input_range =
    decltype(detail::input_range_concept::test<T>(0))::value;

namespace detail {

struct forward_range_concept {
    template <typename>
    static auto test(long) -> std::false_type;

    template <typename T>
    static auto test(int) -> std::enable_if_t<
        input_range<T> && forward_iterator<iterator_t<T>>,
        std::true_type>;
};

}

template <typename T>
NANO_CONCEPT forward_range =
    decltype(detail::forward_range_concept::test<T>(0))::value;

namespace detail {

struct bidirectional_range_concept {
    template <typename>
    static auto test(long) -> std::false_type;

    template <typename T>
    static auto test(int) -> std::enable_if_t<
        forward_range<T> && bidirectional_iterator<iterator_t<T>>,
        std::true_type>;
};

}

template <typename T>
NANO_CONCEPT bidirectional_range =
    decltype(detail::bidirectional_range_concept::test<T>(0))::value;

namespace detail {

struct random_access_range_concept {
    template <typename>
    static auto test(long) -> std::false_type;

    template <typename T>
    static auto test(int) -> std::enable_if_t<
        bidirectional_range<T> && random_access_iterator<iterator_t<T>>,
        std::true_type>;
};

}

template <typename T>
NANO_CONCEPT random_access_range =
    decltype(detail::random_access_range_concept::test<T>(0))::value;

namespace detail {

// FIXME: Not to spec
// We only require random_access_iterator, not contiguous_iterator
// This is so that vector::iterator, string::iterator etc can model
// contiguous_range.
// If we do range-v3-style deep integration with iterator_traits then
// this could be fixed
struct contiguous_range_concept {
    template <typename>
    static auto test(long) -> std::false_type;

    template <typename T>
    static auto test(int) -> std::enable_if_t<
        random_access_range<T> && /* contiguous_iterator<iterator_t<T>> && */
        detail::requires_<contiguous_range_concept, T>,
        std::true_type>;

    template <typename T>
    auto requires_(T& t) -> decltype(
        requires_expr<same_as<decltype(ranges::data(t)),
                      std::add_pointer_t<range_reference_t<T>>>>{}
    );
};

}

template <typename R>
NANO_CONCEPT contiguous_range =
    decltype(detail::contiguous_range_concept::test<R>(0))::value;

namespace detail {

struct common_range_concept {
    template <typename>
    static auto test(long) -> std::false_type;

    template <typename T>
    static auto test(int) -> std::enable_if_t<
        range<T> && same_as<iterator_t<T>, sentinel_t<T>>,
        std::true_type>;
};

}

template <typename T>
NANO_CONCEPT common_range =
    decltype(detail::common_range_concept::test<T>(0))::value;

template <typename T>
NANO_CONCEPT viewable_range =
    range<T> && (borrowed_range<T> || view<remove_cvref_t<T>>);


// [range.dangling]

struct dangling {
    constexpr dangling() noexcept = default;

    template <typename... Args>
    constexpr dangling(Args&&...) noexcept {}
};

template <typename R>
using borrowed_iterator_t = detail::conditional_t<
    borrowed_range<R>, iterator_t<R>, dangling>;

// Helper concepts

namespace detail {

struct simple_view_concept {
    template <typename>
    static auto test(long) -> std::false_type;

    template <typename R>
    static auto test(int) -> std::enable_if_t<
        view<R> && range<const R> &&
        same_as<iterator_t<R>, iterator_t<const R>> &&
        same_as<sentinel_t<R>, sentinel_t<const R>>,
        std::true_type>;

};

template <typename R>
NANO_CONCEPT simple_view = decltype(simple_view_concept::test<R>(0))::value;

struct has_arrow_concept {
    template <typename I>
    auto requires_(I i) -> decltype(i.operator->());
};

template <typename I>
NANO_CONCEPT has_arrow = input_iterator<I> &&
    (std::is_pointer_v<I> || detail::requires_<has_arrow_concept, I>);


template <typename T, typename U>
NANO_CONCEPT not_same_as = !same_as<remove_cvref_t<T>, remove_cvref_t<U>>;

}

NANO_END_NAMESPACE

#endif
