// nanorange/views/drop.hpp
//
// Copyright (c) 2019 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_VIEWS_DROP_HPP_INCLUDED
#define NANORANGE_VIEWS_DROP_HPP_INCLUDED

#include <nanorange/detail/views/range_adaptors.hpp>
#include <nanorange/views/all.hpp>
#include <nanorange/views/interface.hpp>

#include <optional>

NANO_BEGIN_NAMESPACE

namespace detail {

template <bool IsRandomAccess, typename>
struct drop_view_cache {};

template <typename I>
struct drop_view_cache<false, I> {
    std::optional<I> cached{};
};

}

template <typename R>
struct drop_view
    : view_interface<drop_view<R>>,
      private detail::drop_view_cache<random_access_range<R>, iterator_t<R>> {

    static_assert(view<R>);

    drop_view() = default;

    constexpr drop_view(R base, range_difference_t<R> count)
        : base_(std::move(base)),
          count_(count)
    {}

    constexpr R base() const { return base_; }

    template <typename RR = R, std::enable_if_t<
        !(detail::simple_view<RR> && random_access_range<RR>), int> = 0>
    constexpr auto begin()
    {
        if constexpr (random_access_range<R>) {
            return ranges::next(ranges::begin(base_), count_, ranges::end(base_));
        } else {
            auto& c = this->cached;
            if (!c.has_value()) {
                c = ranges::next(ranges::begin(base_), count_, ranges::end(base_));
            }
            return *c;
        }
    }

    template <typename RR = R, std::enable_if_t<random_access_range<const RR>, int> = 0>
    constexpr auto begin() const
    {
        return ranges::next(ranges::begin(base_), count_, ranges::end(base_));
    }

    template <typename RR = R, std::enable_if_t<!detail::simple_view<RR>, int> = 0>
    constexpr auto end()
    {
        return ranges::end(base_);
    }

    template <typename RR = R, std::enable_if_t<range<const RR>, int> = 0>
    constexpr auto end()
    {
        return ranges::end(base_);
    }

    template <typename RR = R, std::enable_if_t<sized_range<RR>, int> = 0>
    constexpr auto size()
    {
        const auto s = ranges::size(base_);
        const auto c = static_cast<decltype(s)>(count_);
        return s < c ? 0 : s - c;
    }

    template <typename RR = R, std::enable_if_t<sized_range<const RR>, int> = 0>
    constexpr auto size() const
    {
        const auto s = ranges::size(base_);
        const auto c = static_cast<decltype(s)>(count_);
        return s < c ? 0 : s - c;
    }

private:
    R base_ = R();
    range_difference_t<R> count_ = 0;
};

template <typename R>
drop_view(R&&, range_difference_t<R>) -> drop_view<all_view<R>>;

namespace detail {

struct drop_view_fn {

    template <typename E, typename F>
    constexpr auto operator()(E&& e, F&& f) const
        -> decltype(drop_view{std::forward<E>(e), std::forward<F>(f)})
    {
        return drop_view{std::forward<E>(e), std::forward<F>(f)};
    }

    template <typename C>
    constexpr auto operator()(C c) const
    {
        return detail::rao_proxy{[c = std::move(c)](auto&& r) mutable
#ifndef NANO_MSVC_LAMBDA_PIPE_WORKAROUND
            -> decltype(drop_view{std::forward<decltype(r)>(r), std::declval<C&&>()})
#endif
        {
            return drop_view{std::forward<decltype(r)>(r), std::move(c)};
        }};
    }

};

}

namespace views {

NANO_INLINE_VAR(nano::detail::drop_view_fn, drop)

}

NANO_END_NAMESPACE

#endif
