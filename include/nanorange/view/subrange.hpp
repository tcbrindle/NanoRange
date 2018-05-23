// nanorange/view/subrange.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_VIEW_SUBRANGE_HPP_INCLUDED
#define NANORANGE_VIEW_SUBRANGE_HPP_INCLUDED

#include <nanorange/view/interface.hpp>
#include <nanorange/iterator.hpp>

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
constexpr subrange_kind default_subrange_kind = subrange_kind::unsized;

template <typename I, typename S>
constexpr subrange_kind default_subrange_kind<I, S, true> = subrange_kind::sized;

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

    template <bool SS = StoreSize, typename = std::enable_if_t<!SS>>
    constexpr subrange(I i, S s)
        : data_{std::move(i), std::move(s)}
    {}

    template <subrange_kind KK = K, typename = std::enable_if_t<
            KK == subrange_kind::sized>>
    constexpr subrange(I i, S s, difference_type_t<I> n)
        : data_{std::move(i), std::move(s), n}
    {}

    template <typename X, typename Y, subrange_kind Z, bool SS = StoreSize,
              std::enable_if_t<
                ConvertibleTo<X, I> &&
                ConvertibleTo<Y, S> &&
                Z == subrange_kind::sized &&
                SS,
              int> = 0>
    constexpr subrange(subrange<X, Y, Z> r)
        : subrange{r.begin(), r.end(), r.size()}
    {}

    template <typename X, typename Y, subrange_kind Z, bool SS = StoreSize,
            std::enable_if_t<
            ConvertibleTo<X, I> &&
            ConvertibleTo<Y, S> &&
            !SS,
            int> = 0>
    constexpr subrange(subrange<X, Y, Z> r)
            : subrange{r.begin(), r.end()}
    {}

    template <typename X, typename Y, subrange_kind Z, subrange_kind KK = K,
              std::enable_if_t<KK == subrange_kind::sized, int> = 0>
    constexpr subrange(subrange<X, Y, Z> r, difference_type_t<I> n)
        : subrange{r.begin(), r.end(), n}
    {}

    template <typename PairLike_, bool SS = StoreSize, std::enable_if_t<
        detail::NotSameAs<PairLike_, subrange> &&
        detail::PairlikeConvertibleTo<PairLike_, I, S> &&
        !SS,
    int> = 0>
    constexpr subrange(PairLike_&& r)
        : subrange{std::get<0>(std::forward<PairLike_>(r)),
                   std::get<1>(std::forward<PairLike_>(r))}
    {}

    template <typename PairLike_, subrange_kind KK = K,
              std::enable_if_t<
                  detail::PairlikeConvertibleTo<PairLike_, I, S> &&
                  KK == subrange_kind::sized,
              int> = 0>
    constexpr subrange(PairLike_&& r, difference_type_t<I> n)
        : subrange{std::get<0>(std::forward<PairLike_>(r)),
                   std::get<1>(std::forward<PairLike_>(r)),
                   n}
    {}

    template <typename R, bool SS = StoreSize, std::enable_if_t<
                  detail::NotSameAs<R, subrange> &&
                  Range<R> &&
                  ConvertibleTo<iterator_t<R>, I> &&
                  ConvertibleTo<sentinel_t<R>, S> &&
                  SS && SizedRange<R>,
              int> = 0>
    constexpr subrange(R& r)
        : subrange{ranges::begin(r), ranges::end(r), distance(r)}
    {}

    template <typename R, bool SS = StoreSize, std::enable_if_t<
            detail::NotSameAs<R, subrange> &&
            Range<R> &&
            ConvertibleTo<iterator_t<R>, I> &&
            ConvertibleTo<sentinel_t<R>, S> &&
            !SS,
            int> = 0>
    constexpr subrange(R& r)
            : subrange{ranges::begin(r), ranges::end(r)}
    {}

    template <typename PairLike_, std::enable_if_t<
                detail::NotSameAs<PairLike_, subrange> &&
                detail::PairLikeConvertibleFrom<PairLike_, const I&, const S&>,
              int> = 0>
    constexpr operator PairLike_() const
    {
        return PairLike_(begin(), end());
    }

    constexpr I begin() const
    {
        return data_.begin_;
    }

    constexpr S end() const
    {
        return data_.end_;
    }

    NANO_NODISCARD constexpr bool empty() const
    {
        return data_.begin_ == data_.end_;
    }

    template <subrange_kind KK = K, bool SS = StoreSize>
    constexpr auto size() const
        -> std::enable_if_t<KK == subrange_kind::sized && SS,
                            difference_type_t<I>>
    {
        return data_.size_;
    }

    template <subrange_kind KK = K, bool SS = StoreSize>
    constexpr auto size() const
    -> std::enable_if_t<KK == subrange_kind::sized && !SS,
            difference_type_t<I>>
    {
        return data_.end_ - data_.begin_;
    }


    NANO_NODISCARD constexpr subrange next(difference_type_t<I> n = 1) const
    {
        auto tmp = *this;
        tmp.advance(n);
        return tmp;
    }

    template <typename II = I>
    NANO_NODISCARD constexpr auto prev(difference_type_t<I> n = 1) const
        -> std::enable_if_t<BidirectionalIterator<I>, subrange>
    {
        auto tmp = *this;
        tmp.advance(-n);
        return tmp;
    }

    template <bool SS = StoreSize>
    constexpr auto advance(difference_type_t<I> n)
        -> std::enable_if_t<SS, subrange&>
    {
        data_.size_ -= n - ranges::advance(data_.begin_, n, data_.end_);
        return *this;
    }

    template <bool SS = StoreSize>
    constexpr auto advance(difference_type_t<I> n)
    -> std::enable_if_t<!SS, subrange&>
    {
        ranges::advance(data_.begin_, n, data_.end_);
        return *this;
    }
};

#ifdef NANO_HAVE_DEDUCTION_GUIDES

template <typename R, typename = std::enable_if_t<Range<R> && !SizedRange<R>>>
subrange(R&) -> subrange<iterator_t<R>, sentinel_t<R>>;

template <typename R, typename = std::enable_if_t<SizedRange<R>>>
subrange(R&) -> subrange<iterator_t<R>, sentinel_t<R>, subrange_kind::sized>;

#endif

namespace detail {

template <std::size_t, typename, typename, subrange_kind>
struct subrange_get_helper;

template <typename I, typename S, subrange_kind K>
struct subrange_get_helper<0, I, S, K> {
    constexpr I operator()(const subrange<I, S, K>& s) const
    {
        return s.begin();
    }
};

template <typename I, typename S, subrange_kind K>
struct subrange_get_helper<1, I, S, K> {
    constexpr S operator()(const subrange<I, S, K>& s) const
    {
        return s.end();
    }
};

}

template <std::size_t N, typename I, typename S, subrange_kind K>
constexpr auto get(const subrange<I, S, K>& r)
    -> decltype(detail::subrange_get_helper<N, I, S, K>{}(r))
{
    return detail::subrange_get_helper<N, I, S, K>{}(r);
}


// Extensions for C++14 compilers without CTAD
// These basically replicate the subrange constructors above

template <typename I, typename S>
constexpr auto make_subrange(I i, S s)
    -> std::enable_if_t<Iterator<I> && Sentinel<S, I>,
            decltype(subrange<I, S>{std::move(i), std::move(s)})>
{
    return {std::move(i), std::move(s)};
}

template <typename I, typename S>
constexpr auto make_subrange(I i, S s, difference_type_t<I> n)
    -> std::enable_if_t<Iterator<I> && Sentinel<S, I>,
        decltype(subrange<I, S>{std::move(i), std::move(s)}, n)>
{
    return {std::move(i), std::move(s), n};
}

template <typename R>
constexpr auto make_subrange(R& r)
    -> std::enable_if_t<Range<R> && !SizedRange<R>,
                        subrange<iterator_t<R>, sentinel_t<R>>>
{
    return {r};
}

template <typename R>
constexpr auto make_subrange(R& r)
-> std::enable_if_t<SizedRange<R>,
                    subrange<iterator_t<R>, sentinel_t<R>, subrange_kind::sized>>
{
    return {r};
}

NANO_END_NAMESPACE

namespace std {

template <typename I, typename S, ::nano::subrange_kind K>
class tuple_size<::nano::subrange<I, S, K>>
    : public integral_constant<size_t, 2> {};

template <typename I, typename S, ::nano::subrange_kind K>
class tuple_element<0, ::nano::subrange<I, S, K>>
{
public:
    using type = I;
};

template <typename I, typename S, ::nano::subrange_kind K>
class tuple_element<1, ::nano::subrange<I, S, K>>
{
public:
    using type = S;
};

}

#endif
