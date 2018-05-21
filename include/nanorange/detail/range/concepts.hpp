// nanorange/detail/range/concepts.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_DETAIL_RANGE_CONCEPTS_HPP_INCLUDED
#define NANORANGE_DETAIL_RANGE_CONCEPTS_HPP_INCLUDED

#include <nanorange/detail/range/access.hpp>
#include <nanorange/detail/range/primitives.hpp>

// These are, sadly, needed for view-predicate specialisations,
// because we're not allowed to forward-declare std classes

#include <initializer_list>
#include <set>
#include <unordered_set>

NANO_BEGIN_NAMESPACE

template <typename T>
using iterator_t = decltype(ranges::begin(std::declval<T&>()));

template <typename T>
using sentinel_t = decltype(ranges::end(std::declval<T&>()));

namespace detail {

template <typename T>
using checked_iterator_t = test_t<iterator_t, T>;

template <typename T>
using checked_sentinel_t = test_t<sentinel_t, T>;

} // namespace detail

template <typename T>
struct enable_view {
};

struct view_base {
};

// [range.range]

namespace detail {

struct Range_req {
    template <typename T>
    auto requires_(T&& t)
        -> decltype(valid_expr(ranges::begin(t), ranges::end(t)));
};

} // namespace detail

template <typename T>
NANO_CONCEPT Range = detail::requires_<detail::Range_req, T>;

// [range.sized]

namespace detail {

template <typename T, typename Deduced>
auto convertible_to_helper(Deduced)
    -> std::enable_if_t<ConvertibleTo<Deduced, T>, int>;

struct SizedRange_req {
    template <typename T>
    auto requires_(T& t) -> decltype(
        valid_expr(convertible_to_helper<difference_type_t<iterator_t<T>>>(
            ranges::size(t))));
};

} // namespace detail

template <typename T>
NANO_CONCEPT SizedRange =
    Range<T> &&
    !disable_sized_range<std::remove_cv_t<std::remove_reference_t<T>>> &&
    detail::requires_<detail::SizedRange_req, T>;

// [range.view]

namespace detail {

template <typename, typename = void>
constexpr bool view_predicate = true;

template <typename T>
using enable_view_t = typename enable_view<T>::type;

template <typename T>
constexpr bool has_enable_view_v = exists_v<enable_view_t, T>;

template <typename T>
constexpr bool view_predicate<T, std::enable_if_t<has_enable_view_v<T>>> =
    enable_view<T>::type::value;

template <typename T>
constexpr bool view_predicate<
    T, std::enable_if_t<!has_enable_view_v<T> && DerivedFrom<T, view_base>>> =
    true;

template <typename T>
constexpr bool view_predicate<std::initializer_list<T>> = false;

template <typename K, typename C, typename A>
constexpr bool view_predicate<std::set<K, C, A>> = false;

template <typename K, typename C, typename A>
constexpr bool view_predicate<std::multiset<K, C, A>> = false;

template <typename K, typename H, typename E, typename A>
constexpr bool view_predicate<std::unordered_set<K, H, E, A>> = false;

template <typename K, typename H, typename E, typename A>
constexpr bool view_predicate<std::unordered_multiset<K, H, E, A>> = false;

template <typename T>
constexpr bool view_predicate_helper =
    !has_enable_view_v<T> && !DerivedFrom<T, view_base> && Range<T> &&
    Range<const T> &&
    !Same<checked_reference_t<checked_iterator_t<T>>,
          checked_reference_t<checked_iterator_t<const T>>>;

template <typename T>
constexpr bool view_predicate<T, std::enable_if_t<view_predicate_helper<T>>> =
    false;

} // namespace detail

template <typename T>
NANO_CONCEPT View = Range<T>&& Semiregular<T>&& detail::view_predicate<T>;

// [range.common]

template <typename T>
NANO_CONCEPT CommonRange = Range<T>&&
    Same<detail::checked_iterator_t<T>, detail::checked_sentinel_t<T>>;

// [ranges.viewable]

template <typename T>
NANO_CONCEPT ViewableRange = Range<T> && (std::is_lvalue_reference<T>::value ||
                                          View<std::decay_t<T>>);

// [range.input]

template <typename T>
NANO_CONCEPT InputRange =
    Range<T>&& InputIterator<detail::checked_iterator_t<T>>;

template <typename R, typename T>
NANO_CONCEPT OutputRange =
    Range<R>&& OutputIterator<detail::checked_iterator_t<R>, T>;

template <typename T>
NANO_CONCEPT ForwardRange =
    InputRange<T>&& ForwardIterator<detail::checked_iterator_t<T>>;

template <typename T>
NANO_CONCEPT BidirectionalRange =
    ForwardRange<T>&& BidirectionalIterator<detail::checked_iterator_t<T>>;

template <typename T>
NANO_CONCEPT RandomAccessRange =
    BidirectionalRange<T>&& RandomAccessIterator<detail::checked_iterator_t<T>>;

NANO_END_NAMESPACE

#endif
