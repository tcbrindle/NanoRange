// nanorange/views/take.hpp
//
// Copyright (c) 2019 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_VIEWS_TAKE_HPP_INCLUDED
#define NANORANGE_VIEWS_TAKE_HPP_INCLUDED

#include <nanorange/algorithm/min.hpp>
#include <nanorange/detail/views/range_adaptors.hpp>
#include <nanorange/iterator/counted_iterator.hpp>
#include <nanorange/views/all.hpp>
#include <nanorange/views/interface.hpp>

NANO_BEGIN_NAMESPACE

template <typename V>
struct take_view : view_interface<take_view<V>> {
private:
    static_assert(view<V>);

    V base_ = V();
    range_difference_t<V> count_ = 0;

    template <bool Const>
    struct sentinel {
    private:
        friend struct sentinel<!Const>;
        using Base = detail::conditional_t<Const, const V, V>;
        using CI = counted_iterator<iterator_t<Base>>;

        sentinel_t<Base> end_ = sentinel_t<Base>();

    public:
        sentinel() = default;

        constexpr explicit sentinel(sentinel_t<Base> end)
            : end_(std::move(end))
        {}

        // Use deduced type to avoid constraint recursion in GCC8
        template <typename S,
            std::enable_if_t<same_as<S, sentinel<!Const>>, int> = 0,
            bool C = Const, typename VV = V,
            std::enable_if_t<C &&
                convertible_to<sentinel_t<VV>, sentinel_t<Base>>, int> = 0>
        constexpr explicit sentinel(S s)
            : end_(std::move(s.end_))
        {}

        constexpr sentinel_t<Base> base() const { return end_; }

        friend constexpr bool operator==(const CI& y, const sentinel& x)
        {
            return y.count() == 0 || y.base() == x.end_;
        }

        friend constexpr bool operator==(const sentinel& x, const CI& y)
        {
            return y == x;
        }

        friend constexpr bool operator!=(const CI& y, const sentinel& x)
        {
            return !(y == x);
        }

        friend constexpr bool operator!=(const sentinel& x, const CI& y)
        {
            return !(y == x);
        }
    };

public:
    take_view() = default;

    constexpr take_view(V base, range_difference_t<V> count)
        : base_(std::move(base)),
          count_(count)
    {}

    template <typename R, std::enable_if_t<
        viewable_range<R> && constructible_from<V, all_view<R>>, int> = 0>
    constexpr take_view(R&& r, range_difference_t<V> count)
        : base_(views::all(std::forward<R>(r))),
          count_(count)
    {}

    constexpr V base() const { return base_; }

    template <typename VV = V, std::enable_if_t<!detail::simple_view<VV>, int> = 0>
    constexpr auto begin()
    {
        if constexpr (sized_range<V>) {
            if constexpr (random_access_range<V>) {
                return ranges::begin(base_);
            } else {
                // N.B spec doesn't static_cast here, but I'm pretty
                // sure it should
                return counted_iterator{ranges::begin(base_),
                                        static_cast<range_difference_t<V>>(size())};
            }
        } else {
            return counted_iterator{ranges::begin(base_), count_};
        }
    }

    template <typename VV = V, std::enable_if_t<range<const VV>, int> = 0>
    constexpr auto begin() const
    {
        if constexpr (sized_range<const V>) {
            if constexpr (random_access_range<V>) {
                return ranges::begin(base_);
            } else {
                // N.B spec doesn't static_cast here, but I'm pretty
                // sure it should
                return counted_iterator{ranges::begin(base_),
                                        static_cast<range_difference_t<V>>(size())};
            }
        } else {
            return counted_iterator{ranges::begin(base_), count_};
        }
    }

    template <typename VV = V, std::enable_if_t<!detail::simple_view<VV>, int> = 0>
    constexpr auto end()
    {
        if constexpr (sized_range<V>) {
            if constexpr (random_access_range<V>) {
                return ranges::begin(base_) + size();
            } else {
                return default_sentinel;
            }
        } else {
            return sentinel<false>{ranges::end(base_)};
        }
    }

    template <typename VV = V, std::enable_if_t<range<const VV>, int> = 0>
    constexpr auto end() const
    {
        if constexpr (sized_range<const V>) {
            if constexpr (random_access_range<const V>) {
                return ranges::begin(base_) + size();
            } else {
                return default_sentinel;
            }
        } else {
            return sentinel<true>{ranges::end(base_)};
        }
    }

    template <typename VV = V, std::enable_if_t<sized_range<VV>, int> = 0>
    constexpr auto size()
    {
        auto n = ranges::size(base_);
        return ranges::min(n, static_cast<decltype(n)>(count_));
    }

    template <typename VV = V, std::enable_if_t<sized_range<const VV>, int> = 0>
    constexpr auto size() const
    {
        auto n = ranges::size(base_);
        return ranges::min(n, static_cast<decltype(n)>(count_));
    }
};

template <typename R, std::enable_if_t<range<R>, int> = 0>
take_view(R&&, range_difference_t<R>) -> take_view<all_view<R>>;

namespace detail {

#ifdef NANO_MSVC_LAMBDA_PIPE_WORKAROUND
template <typename R>
using take_view_helper_t = take_view<all_view<R>>;
#endif

struct take_view_fn {

    template <typename C>
    constexpr auto operator()(C c) const
    {

        return detail::rao_proxy{[c = std::move(c)](auto&& r) mutable
#ifdef NANO_MSVC_LAMBDA_PIPE_WORKAROUND
            -> take_view_helper_t<decltype(r)>
#else
            -> decltype(take_view{std::forward<decltype(r)>(r), std::declval<C&&>()})
#endif
        {
            return take_view{std::forward<decltype(r)>(r), std::move(c)};
        }};
    }

    template <typename E, typename F>
    constexpr auto operator()(E&& e, F&& f) const
        -> decltype(take_view{std::forward<E>(e), std::forward<F>(f)})
    {
        return take_view{std::forward<E>(e), std::forward<F>(f)};
    }

};

}

namespace views {

NANO_INLINE_VAR(nano::detail::take_view_fn, take)

}

NANO_END_NAMESPACE

#endif
