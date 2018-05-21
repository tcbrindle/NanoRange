// nanorange/view/subrange.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_VIEW_SUBRANGE_HPP_INCLUDED
#define NANORANGE_VIEW_SUBRANGE_HPP_INCLUDED

#include <nanorange/view/interface.hpp>

NANO_BEGIN_NAMESPACE

// [ranges.subrange]

enum class subrange_kind : bool { unsized, sized };

namespace detail {

struct PairLike_req {
    template <std::size_t I, typename T>
    int test_func(const std::tuple_element_t<I, T>&);

    template <typename T>
    auto requires_(T t) -> decltype(
        valid_expr(std::enable_if_t<Integral<std::tuple_size<T>::value>, int>{},
                   std::enable_if_t<std::tuple_size<T>::value == 2, int>{},
                   decltype(this->test_func<0, T>(std::get<0>(t))){},
                   decltype(this->test_func<1, T>(std::get<1>(t))){}));
};

template <typename T>
NANO_CONCEPT PairLike = detail::requires_<detail::PairLike_req, T>;

struct PairLikeConvertibleTo_req {
    template <typename T, typename U, typename V>
    auto requires_(T&& t) -> decltype(
        valid_expr(convertible_to_helper<U>(std::get<0>(std::forward<T>(t))),
                   convertible_to_helper<V>(std::get<1>(std::forward<T>(t)))));
};

template <typename T, typename U, typename V>
NANO_CONCEPT PairlikeConvertibleTo =
    !Range<T> && PairLike<std::decay_t<T>> &&
    detail::requires_<PairLikeConvertibleTo_req, T, U, V>;

template <typename T, typename U, typename V>
NANO_CONCEPT PairLikeConvertibleFrom = !Range<T> && Same<T, std::decay_t<T>> &&
                                       PairLike<T> && Constructible<T, U, V>;

template <typename T>
NANO_CONCEPT IteratorSentinelPair =
    !Range<T> && Same<T, std::decay_t<T>> && PairLike<T> &&
    Sentinel<std::tuple_element_t<1, T>, std::tuple_element_t<0, T>>;

template <typename T, typename U>
NANO_CONCEPT NotSameAs = !Same<remove_cvref_t<T>, remove_cvref_t<U>>;

template <typename I, typename S, bool = SizedSentinel<S, I>>
subrange_kind default_subrange_kind = subrange_kind::unsized;

template <typename I, typename S>
subrange_kind default_subrange_kind<I, S, true> = subrange_kind::sized;

template <typename I, typename S, bool StoreSize = false>
struct subrange_data {
    I begin_{};
    S end_{};
};

template <typename I, typename S>
struct subrange_data<I, S, true> {
    I begin_{};
    S end_{};
    difference_type_t<I> size_ = 0;
};

} // namespace detail

template <typename I, typename S,
          subrange_kind K = detail::default_subrange_kind<I, S>>
class subrange : public view_interface<subrange<I, S, K>> {
    static_assert(Iterator<I>, "");
    static_assert(Sentinel<S, I>, "");
    static_assert(K == subrange_kind::sized || !SizedSentinel<S, I>, "");

private:
    static constexpr bool StoreSize =
        K == subrange_kind::sized && !SizedSentinel<S, I>;

    detail::subrange_data<I, S, StoreSize> data_{};

public:
    using iterator = I;
    using sentinel = S;

    subrange() = default;

    constexpr subrange(I i, S s);

    constexpr subrange(I i, S s, difference_type_t<I> n);

    template <typename X, typename Y, subrange_kind Z>
    constexpr subrange(subrange<X, Y, Z> r);

    template <typename X, typename Y, subrange_kind Z>
    constexpr subrange(subrange<X, Y, Z> r, difference_type_t<I> n);

    template <typename PairLike_>
    constexpr subrange(PairLike_&& r);

    template <typename PairLike_>
    constexpr subrange(PairLike_&& r, difference_type_t<I> n);

    template <typename R>
    constexpr subrange(R& r);

    template <typename PairLike_>
    constexpr operator PairLike_() const;

    constexpr I begin() const;

    constexpr S end() const;

    constexpr bool empty() const;

    constexpr difference_type_t<I> size() const;

    [[nodiscard]] constexpr subrange next(difference_type_t<I> n = 1) const;

    [[nodiscard]] constexpr subrange prev(difference_type_t<I> n = 1) const;

    constexpr subrange& advance(difference_type_t<I> n);
};

NANO_END_NAMESPACE

#endif
