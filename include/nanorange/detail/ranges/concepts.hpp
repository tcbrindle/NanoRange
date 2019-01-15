// nanorange/detail/ranges/concepts.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_DETAIL_RANGES_CONCEPTS_HPP_INCLUDED
#define NANORANGE_DETAIL_RANGES_CONCEPTS_HPP_INCLUDED

#include <nanorange/detail/ranges/begin_end.hpp>
#include <nanorange/detail/ranges/primitives.hpp>

#include <initializer_list>

// Avoid dragging in the large <set> and <unordered_set> headers
// This is technically undefined behaviour: define the symbol
// NANORANGE_NO_STD_FORWARD_DECLARATIONS
// to enforce standard-compliant mode
#ifndef NANORANGE_NO_STD_FORWARD_DECLARATIONS
NANO_BEGIN_NAMESPACE_STD
template <typename, typename, typename> class set;
template <typename, typename, typename> class multiset;
template <typename, typename, typename, typename> class unordered_set;
template <typename, typename, typename, typename> class unordered_multiset;
NANO_END_NAMESPACE_STD
#else
#include <set>
#include <unordered_set>
#endif

NANO_BEGIN_NAMESPACE

template <typename T>
using iterator_t = decltype(ranges::begin(std::declval<T&>()));

template <typename T>
using sentinel_t = decltype(ranges::end(std::declval<T&>()));

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
        -> decltype(valid_expr(ranges::begin(std::forward<T>(t)),
                               ranges::end(std::forward<T>(t))));
};

template <typename T>
NANO_CONCEPT RangeImpl = requires_<Range_req, T>;

template <typename>
auto Range_fn(long) -> std::false_type;

template <typename T>
auto Range_fn(int) -> std::enable_if_t<RangeImpl<T&>, std::true_type>;

} // namespace detail

template <typename T>
NANO_CONCEPT Range = decltype(detail::Range_fn<T>(0))::value;

namespace detail {

template <typename T>
NANO_CONCEPT ForwardingRange = Range<T> && RangeImpl<T>;

}

// [range.sized]

namespace detail {

template <typename T, typename Deduced>
auto convertible_to_helper(Deduced)
    -> std::enable_if_t<ConvertibleTo<Deduced, T>, int>;

struct SizedRange_req {
    template <typename T>
    auto requires_(T& t) -> decltype(
        valid_expr(convertible_to_helper<iter_difference_t<iterator_t<T>>>(
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
struct view_predicate : std::true_type {};

template <typename T>
constexpr bool view_predicate_v = view_predicate<T>::value;

template <typename T>
using enable_view_t = typename enable_view<T>::type;

template <typename T>
constexpr bool has_enable_view_v = exists_v<enable_view_t, T>;

template <typename T>
struct view_predicate<T, std::enable_if_t<has_enable_view_v<T>>> {
    static constexpr bool value = enable_view<T>::type::value;
};

template <typename T>
struct view_predicate<
    T, std::enable_if_t<!has_enable_view_v<T> && DerivedFrom<T, view_base>>>
    : std::true_type {};

template <typename T>
struct view_predicate<std::initializer_list<T>> : std::false_type {};

template <typename K, typename C, typename A>
struct view_predicate<std::set<K, C, A>> : std::false_type {};

template <typename K, typename C, typename A>
struct view_predicate<std::multiset<K, C, A>>  : std::false_type {};

template <typename K, typename H, typename E, typename A>
struct view_predicate<std::unordered_set<K, H, E, A>> : std::false_type {};

template <typename K, typename H, typename E, typename A>
struct view_predicate<std::unordered_multiset<K, H, E, A>> : std::false_type {};

template <typename>
auto view_predicate_helper_fn(long) -> std::false_type;

template <typename T>
auto view_predicate_helper_fn(int) -> std::enable_if_t<
        !has_enable_view_v<T> &&
        !DerivedFrom<T, view_base> &&
        Range<T> &&
        Range<const T> &&
        !Same<iter_reference_t<iterator_t<T>>, iter_reference_t<iterator_t<const T>>>,
    std::true_type>;

template <typename T>
constexpr bool view_predicate_helper =
    decltype(view_predicate_helper_fn<T>(0))::value;

template <typename T>
struct view_predicate<T, std::enable_if_t<view_predicate_helper<T>>>
   : std::false_type {};

} // namespace detail

template <typename T>
NANO_CONCEPT View = Range<T>&& Semiregular<T>&& detail::view_predicate_v<T>;

// [range.common]
namespace detail {

template <typename>
auto CommonRange_fn(long) -> std::false_type;

template <typename T>
auto CommonRange_fn(int) -> std::enable_if_t<
    Range<T> &&
    Same<iterator_t<T>, sentinel_t<T>>,
        std::true_type>;

}

template <typename T>
NANO_CONCEPT CommonRange = decltype(detail::CommonRange_fn<T>(0))::value;

// [ranges.viewable]

template <typename T>
NANO_CONCEPT ViewableRange = Range<T> && (detail::ForwardingRange<T> ||
                                          View<std::decay_t<T>>);

// [range.input]

namespace detail {

template <typename>
auto InputRange_fn(long) -> std::false_type;

template <typename T>
auto InputRange_fn(int) -> std::enable_if_t<
        Range<T> &&
        InputIterator<iterator_t<T>>,
    std::true_type>;

}

template <typename T>
NANO_CONCEPT InputRange =
    decltype(detail::InputRange_fn<T>(0))::value;

namespace detail {

template <typename, typename >
auto OutputRange_fn(long) -> std::false_type;

template <typename R, typename T>
auto OutputRange_fn(int) -> std::enable_if_t<
        Range<R> && OutputIterator<iterator_t<R>, T>,
        std::true_type>;

}

template <typename R, typename T>
NANO_CONCEPT OutputRange =
    decltype(detail::OutputRange_fn<R, T>(0))::value;

namespace detail {

template <typename>
auto ForwardRange_fn(long) -> std::false_type;

template <typename T>
auto ForwardRange_fn(int) -> std::enable_if_t<
        InputRange<T> && ForwardIterator<iterator_t<T>>,
        std::true_type>;

}

template <typename T>
NANO_CONCEPT ForwardRange =
    decltype(detail::ForwardRange_fn<T>(0))::value;

namespace detail {

template <typename>
auto BidirectionalRange_fn(long) -> std::false_type;

template <typename T>
auto BidirectionalRange_fn(int) -> std::enable_if_t<
        ForwardRange<T> && BidirectionalIterator<iterator_t<T>>,
        std::true_type>;

}

template <typename T>
NANO_CONCEPT BidirectionalRange =
    decltype(detail::BidirectionalRange_fn<T>(0))::value;

namespace detail {

template <typename>
auto RandomAccessRange_fn(long) -> std::false_type;

template <typename T>
auto RandomAccessRange_fn(int) -> std::enable_if_t<
        BidirectionalRange<T> && RandomAccessIterator<iterator_t<T>>,
        std::true_type>;

}

template <typename T>
NANO_CONCEPT RandomAccessRange =
    decltype(detail::RandomAccessRange_fn<T>(0))::value;

namespace detail {

// Not to spec: P0944 requires that R's iterator_t models ContiguousIterator,
// but we only require RandomAccessIterator.
// This is so that std::vector, std::string etc can model ContiguousRange
struct ContiguousRange_req {
    template <typename R>
    auto requires_(R& r) -> decltype(
        requires_expr<Same<decltype(ranges::data(r)), std::add_pointer_t<iter_reference_t<iterator_t<R>>>>>{}
    );
};


template <typename>
auto ContiguousRange_fn(long) -> std::false_type;

template <typename R>
auto ContiguousRange_fn(int) -> std::enable_if_t<
        Range<R> && RandomAccessIterator<iterator_t<R>> &&
        requires_<ContiguousRange_req, R>,
                std::true_type>;

}

template <typename R>
NANO_CONCEPT ContiguousRange =
    decltype(detail::ContiguousRange_fn<R>(0))::value;

template <typename R>
using safe_iterator_t = std::enable_if_t<Range<R>,
        decltype(ranges::begin(std::declval<R>()))>;

// Helper concepts

// TODO: Add SimpleView and HasArrow

namespace detail {

template <typename T, typename U>
NANO_CONCEPT NotSameAs = !Same<remove_cvref_t<T>, remove_cvref_t<U>>;

}

NANO_END_NAMESPACE

#endif
