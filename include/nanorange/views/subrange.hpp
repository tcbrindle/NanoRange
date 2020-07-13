// nanorange/views/subrange.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_VIEWS_SUBRANGE_HPP_INCLUDED
#define NANORANGE_VIEWS_SUBRANGE_HPP_INCLUDED

#include <nanorange/iterator/operations.hpp>
#include <nanorange/views/interface.hpp>

NANO_BEGIN_NAMESPACE

// [ranges.subrange]

enum class subrange_kind : bool { unsized, sized };

namespace detail {

template <typename I, typename S, bool = sized_sentinel_for<S, I>>
struct default_subrange_kind {
    static constexpr subrange_kind kind = subrange_kind::unsized;
};

template <typename I, typename S>
struct default_subrange_kind<I, S, true> {
    static constexpr subrange_kind kind = subrange_kind::sized;
};

}

namespace subrange_ {

template <typename I, typename S = I,
          subrange_kind = detail::default_subrange_kind<I, S>::kind>
class subrange;

}

using subrange_::subrange;

namespace detail {

struct convertible_to_non_slicing_concept {
    template <typename, typename>
    static auto test(long) -> std::false_type;

    template <typename From, typename To>
    static auto test(int) -> std::enable_if_t<
        convertible_to<From, To> &&
        !(std::is_pointer_v<std::decay_t<From>> &&
          std::is_pointer_v<std::decay_t<To>> &&
          not_same_as<std::remove_pointer_t<std::decay_t<From>>,
                      std::remove_pointer_t<std::decay_t<To>>>),
        std::true_type>;
};

template <typename From, typename To>
NANO_CONCEPT convertible_to_non_slicing =
    decltype(convertible_to_non_slicing_concept::test<From, To>(0))::value;

struct pair_like_concept {
    template <typename>
    static auto test(long) -> std::false_type;

    template <typename T,
              typename = typename std::tuple_size<T>::type,
              typename = std::enable_if_t<detail::requires_<pair_like_concept, T>>>
    static auto test(int) -> std::true_type;

    template <typename T>
    auto requires_(T t) -> decltype(
        requires_expr<derived_from<std::tuple_size<T>, std::integral_constant<std::size_t, 2>>>{},
        std::declval<std::tuple_element_t<0, std::remove_const_t<T>>>(),
        std::declval<std::tuple_element_t<1, std::remove_const_t<T>>>(),
        requires_expr<convertible_to<decltype(std::get<0>(t)), const std::tuple_element<0, T>&>>{},
        requires_expr<convertible_to<decltype(std::get<1>(t)), const std::tuple_element<1, T>&>>{}
    );
};

template <typename T>
NANO_CONCEPT pair_like = !std::is_reference_v<T> &&
    decltype(pair_like_concept::test<T>(0))::value;

struct pair_like_convertible_from_concept {
    template <typename, typename, typename>
    static auto test(long) -> std::false_type;

    template <typename T, typename U, typename V,
              std::enable_if_t<!range<T>, int> = 0,
              std::enable_if_t<pair_like<T>, int> = 0,
              std::enable_if_t<constructible_from<T, U, V>, int> = 0,
              std::enable_if_t<convertible_to_non_slicing<U, std::tuple_element<0, T>>, int> = 0,
              std::enable_if_t<convertible_to<V, std::tuple_element<1, T>>, int> = 0>
    static auto test(int) -> std::true_type;
};

template <typename T, typename U, typename V>
NANO_CONCEPT pair_like_convertible_from =
    decltype(pair_like_convertible_from_concept::test<T, U, V>(0))::value;


struct iterator_sentinel_pair_concept {
    template <typename T>
    static auto test(long) -> std::false_type;

    template <typename T>
    static auto test(int) -> std::enable_if_t<
        !range<T> && pair_like<T> &&
        sentinel_for<std::tuple_element_t<1, T>,  std::tuple_element_t<0, T>>,
        std::true_type>;
};

template <typename T>
NANO_CONCEPT iterator_sentinel_pair =
    decltype(iterator_sentinel_pair_concept::test<T>(0))::value;

template <typename I, typename S, bool StoreSize = false>
struct subrange_data {
    constexpr subrange_data() = default;

    constexpr subrange_data(I&& begin, S&& end)
        : begin_(std::move(begin)), end_(std::move(end))
    {}

    constexpr subrange_data(I&& begin, S&& end, std::make_unsigned_t<iter_difference_t<I>> /*unused*/)
        : begin_(std::move(begin)), end_(std::move(end))
    {}

    I begin_{};
    S end_{};
};

template <typename I, typename S>
struct subrange_data<I, S, true> {
    constexpr subrange_data() = default;

    constexpr subrange_data(I&& begin, S&& end, std::make_unsigned_t<iter_difference_t<I>> size)
        : begin_(std::move(begin)), end_(std::move(end)), size_(size)
    {}

    I begin_{};
    S end_{};
    std::make_unsigned_t<iter_difference_t<I>> size_ = 0;
};

// MSVC gets confused if enable_if conditions in template param lists are too
// complex, so give it some help by calculating the constraints in a helper
// variable
template <typename R, typename I, typename S, subrange_kind K>
auto subrange_range_constructor_constraint_helper_fn(long) -> std::false_type;

template <typename R, typename I, typename S, subrange_kind K>
auto subrange_range_constructor_constraint_helper_fn(int) -> std::enable_if_t<borrowed_range<R>&&
                convertible_to_non_slicing<iterator_t<R>, I> &&
                convertible_to<sentinel_t<R>, S>, std::true_type>;

template <typename R, typename I, typename S, subrange_kind K>
constexpr bool subrange_range_constructor_constraint_helper =
    decltype(subrange_range_constructor_constraint_helper_fn<R, I, S, K>(0))::value;

template <typename R>
constexpr subrange_kind subrange_deduction_guide_helper()
{
    return (sized_range<R> || sized_sentinel_for<sentinel_t<R>, iterator_t<R>>)
           ? subrange_kind::sized : subrange_kind::unsized;
}

} // namespace detail

namespace subrange_ {

template <typename I, typename S, subrange_kind K>
class subrange : public view_interface<subrange<I, S, K>> {
    static_assert(input_or_output_iterator<I>);
    static_assert(sentinel_for<S, I>);
    static_assert(K == subrange_kind::sized || !sized_sentinel_for<S, I>, "");

private:
    static constexpr bool StoreSize =
            K == subrange_kind::sized && !sized_sentinel_for<S, I>;

    detail::subrange_data<I, S, StoreSize> data_{};

public:
    subrange() = default;

    template <typename II, bool SS = StoreSize,
             typename = std::enable_if_t<
                 detail::convertible_to_non_slicing<II, I> &&
                 !SS>>
    constexpr subrange(II i, S s)
            : data_{std::move(i), std::move(s)} {}

    template <typename II, subrange_kind KK = K,
              typename = std::enable_if_t<
                  detail::convertible_to_non_slicing<II, I> &&
                  KK == subrange_kind::sized>>
    constexpr subrange(II i, S s, std::make_unsigned_t<iter_difference_t<I>> n)
            : data_{std::move(i), std::move(s), n} {}

    template <typename R, bool SS = StoreSize,
            std::enable_if_t<detail::not_same_as<R, subrange>, int> = 0,
            std::enable_if_t<
                    detail::subrange_range_constructor_constraint_helper<R, I, S, K>
                    && SS && sized_range<R>, int> = 0>
    constexpr subrange(R&& r)
            : subrange(ranges::begin(r), ranges::end(r), ranges::size(r)) {}

    template <typename R, bool SS = StoreSize,
            std::enable_if_t<detail::not_same_as<R, subrange>, int> = 0,
            std::enable_if_t<
                    detail::subrange_range_constructor_constraint_helper<R, I, S, K>
                     && !SS, int> = 0>
    constexpr subrange(R&& r)
            : subrange(ranges::begin(r), ranges::end(r)) {}

    template <typename R, subrange_kind KK = K, std::enable_if_t<
            borrowed_range<R> &&
            detail::convertible_to_non_slicing<iterator_t<R>, I> &&
            convertible_to<sentinel_t<R>, S> &&
            KK == subrange_kind::sized, int> = 0>
    constexpr subrange(R&& r, std::make_unsigned_t<iter_difference_t<I>> n)
            : subrange(ranges::begin(r), ranges::end(r), n) {}


    template <typename PairLike_,
            std::enable_if_t<detail::not_same_as<PairLike_, subrange>, int> = 0,
            std::enable_if_t<detail::pair_like_convertible_from<
                                PairLike_, const I&, const S&>, int> = 0>
    constexpr operator PairLike_() const
    {
        return PairLike_(begin(), end());
    }

    constexpr I begin() const { return data_.begin_; }

    constexpr S end() const { return data_.end_; }

    [[nodiscard]] constexpr bool empty() const
    {
        return data_.begin_ == data_.end_;
    }

    template <subrange_kind KK = K>
    constexpr auto size() const
        -> std::enable_if_t<KK == subrange_kind::sized, std::make_unsigned_t<iter_difference_t<I>>>
    {
        if constexpr (StoreSize) {
            return data_.size_;
        } else {
            constexpr auto make_unsigned_like = [](auto i) {
                return std::make_unsigned_t<decltype(i)>(i);
            };
            return make_unsigned_like(data_.end_ - data_.begin_);
        }
    }

    [[nodiscard]] constexpr subrange next(iter_difference_t<I> n = 1) const
    {
        auto tmp = *this;
        tmp.advance(n);
        return tmp;
    }

    template <typename II = I>
    [[nodiscard]] constexpr auto prev(iter_difference_t<I> n = 1) const
        -> std::enable_if_t<bidirectional_iterator<II>, subrange>
    {
        auto tmp = *this;
        tmp.advance(-n);
        return tmp;
    }

    constexpr subrange& advance(iter_difference_t<I> n)
    {
        if constexpr (StoreSize) {
            data_.size_ -= n - ranges::advance(data_.begin_, n, data_.end_);
        } else {
            ranges::advance(data_.begin_, n, data_.end_);
        }
        return *this;
    }
};

template <typename I, typename S,
          std::enable_if_t<input_or_output_iterator<I> && sentinel_for<S, I>, int> = 0>
subrange(I, S) -> subrange<I, S>;

template <typename I, typename S, std::enable_if_t<input_or_output_iterator<I> && sentinel_for<S, I>, int> = 0>
subrange(I, S, std::make_unsigned_t<iter_difference_t<I>>) -> subrange<I, S, subrange_kind::sized>;

template <typename P, std::enable_if_t<detail::iterator_sentinel_pair<P>, int> = 0>
subrange(P) -> subrange<std::tuple_element_t<0, P>, std::tuple_element_t<1, P>>;

template <typename P, std::enable_if_t<detail::iterator_sentinel_pair<P>, int> = 0>
subrange(P, std::make_unsigned_t<iter_difference_t<std::tuple_element_t<0, P>>>) ->
    subrange<std::tuple_element_t<0, P>, std::tuple_element_t<1, P>, subrange_kind::sized>;

template <typename R, std::enable_if_t<borrowed_range<R>, int> = 0>
subrange(R&&) ->
    subrange<iterator_t<R>, sentinel_t<R>,
             detail::subrange_deduction_guide_helper<R>()>;

template <typename R, std::enable_if_t<borrowed_range<R>, int> = 0>
subrange(R&&, std::make_unsigned_t<iter_difference_t<iterator_t<R>>>) ->
    subrange<iterator_t<R>, sentinel_t<R>, subrange_kind::sized>;

} // namespace subrange_

template <typename I, typename S, subrange_kind K>
inline constexpr bool enable_borrowed_range<subrange<I, S, K>> = true;

template <std::size_t N, typename I, typename S, subrange_kind K,
          std::enable_if_t<(N < 2), int> = 0>
constexpr auto get(const subrange<I, S, K>& r)
{
    if constexpr (N == 0) {
        return r.begin();
    } else {
        return r.end();
    }
}

template <typename R>
using borrowed_subrange_t =
    detail::conditional_t<borrowed_range<R>, subrange<iterator_t<R>>, dangling>;

NANO_END_NAMESPACE

namespace std {

template <typename I, typename S, ::nano::subrange_kind K>
class tuple_size<::nano::subrange<I, S, K>>
    : public integral_constant<size_t, 2> {
};

template <typename I, typename S, ::nano::subrange_kind K>
class tuple_element<0, ::nano::subrange<I, S, K>> {
public:
    using type = I;
};

template <typename I, typename S, ::nano::subrange_kind K>
class tuple_element<1, ::nano::subrange<I, S, K>> {
public:
    using type = S;
};

using ::nano::ranges::get;

} // namespace std

#endif
