// nanorange/view/reverse.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_VIEW_REVERSE_HPP_INCLUDED
#define NANORANGE_VIEW_REVERSE_HPP_INCLUDED

#include <nanorange/iterator/reverse_iterator.hpp>
#include <nanorange/view/all.hpp>

#include <optional>

NANO_BEGIN_NAMESPACE

namespace detail {

template <bool IsCommonRange, typename>
struct reverse_view_cache {};

template <typename I>
struct reverse_view_cache<false, I> {
    std::optional<I> cached{};
};

}

template <typename V>
struct reverse_view
    : view_interface<reverse_view<V>>,
      private detail::reverse_view_cache<CommonRange<V>, iterator_t<V>> {

    static_assert(View<V> && BidirectionalRange<V>, "");

    reverse_view() = default;

    constexpr explicit reverse_view(V r)
        : base_(std::move(r))
    {}

    template <typename R,
              /* FIXME: This is not to spec, but we get in horrible recursive trouble if it's omitted */
              std::enable_if_t<detail::NotSameAs<R, reverse_view>, int> = 0,
              std::enable_if_t<
                  ViewableRange<R> &&
                  BidirectionalRange<R> &&
                  Constructible<V, all_view<R>>, int> = 0>
    constexpr explicit reverse_view(R&& r)
        : base_(view::all(std::forward<R>(r)))
    {}

    constexpr reverse_view(const reverse_view& other)
        : base_(other.base_)
    {}

    constexpr V base() const { return base_; }

    constexpr reverse_iterator<iterator_t<V>> begin()
    {
        if constexpr (CommonRange<V>) {
            return nano::make_reverse_iterator(ranges::end(base_));
        } else {
            auto& c = this->cached;
            if (!c.has_value()) {
                c = ranges::next(ranges::begin(base_), ranges::end(base_));
            }
            return nano::make_reverse_iterator(*c);
        }
    }

    template <typename VV = V>
    constexpr auto begin() const
        -> std::enable_if_t<CommonRange<const VV>,
                            reverse_iterator<iterator_t<const VV>>>
    {
        return nano::make_reverse_iterator(ranges::end(base_));
    }

    constexpr reverse_iterator<iterator_t<V>> end()
    {
        return nano::make_reverse_iterator(ranges::begin(base_));
    }

    template <typename VV = V>
    constexpr auto end() const
        -> std::enable_if_t<CommonRange<const VV>, reverse_iterator<iterator_t<const VV>>>
    {
        return nano::make_reverse_iterator(ranges::begin(base_));
    }

    template <typename VV = V, std::enable_if_t<SizedRange<VV>, int> = 0>
    constexpr auto size()
    {
        return ranges::size(base_);
    }

    template <typename VV = V, std::enable_if_t<SizedRange<const VV>, int> = 0>
    constexpr auto size() const
    {
        return ranges::size(base_);
    }

private:
    V base_ = V{};
};

template <typename R>
reverse_view(R&&) -> reverse_view<all_view<R>>;

namespace detail {

struct reverse_view_fn {
    template <typename R>
    constexpr std::enable_if_t<
            ViewableRange<R> &&
            BidirectionalRange<R>,
    reverse_view<all_view<R>>>
    operator()(R&& r) const
    {
        return reverse_view<all_view<R>>(std::forward<R>(r));
    }
};

template <>
inline constexpr bool is_raco<reverse_view_fn> = true;

} // namespace detail

namespace view {

NANO_INLINE_VAR(nano::detail::reverse_view_fn, reverse)

} // namespace view

NANO_END_NAMESPACE

#endif
