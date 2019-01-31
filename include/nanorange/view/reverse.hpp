// nanorange/view/reverse.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_VIEW_REVERSE_HPP_INCLUDED
#define NANORANGE_VIEW_REVERSE_HPP_INCLUDED

#include <nanorange/iterator/reverse_iterator.hpp>
#include <nanorange/view/all.hpp>

NANO_BEGIN_NAMESPACE

namespace detail {

template <bool IsCommonRange, typename>
struct reverse_view_cache {};

template <typename I>
struct reverse_view_cache<false, I> {
    I cached = I{};
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

    template <typename VV = V>
    constexpr auto begin()
        -> std::enable_if_t<!CommonRange<VV>, reverse_iterator<iterator_t<V>>>
    {
        using I = iterator_t<V>;
        I& c = this->cached;
        if (c == I{}) {
            c = ranges::next(ranges::begin(base_), ranges::end(base_));
        }
        return nano::make_reverse_iterator(c);
    }

    template <typename VV = V>
    constexpr auto begin()
        -> std::enable_if_t<CommonRange<VV>, reverse_iterator<iterator_t<V>>>
    {
        return nano::make_reverse_iterator(ranges::end(base_));
    }

    template <typename VV = V>
    constexpr auto begin() const
        -> std::enable_if_t<CommonRange<const VV>,
                            reverse_iterator<iterator_t<const V>>>
    {
        return nano::make_reverse_iterator(ranges::end(base_));
    }

    constexpr reverse_iterator<iterator_t<V>> end()
    {
        return nano::make_reverse_iterator(ranges::begin(base_));
    }

    template <typename VV = V>
    constexpr auto end() const
        -> std::enable_if_t<CommonRange<const VV>, reverse_iterator<iterator_t<const V>>>
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

#ifdef NANO_HAVE_DEDUCTION_GUIDES

template <typename R>
reverse_view(R&&) -> reverse_view<all_view<R>>;

#endif // NANO_HAVE_DEDUCTION_GUIDES

namespace view {

namespace detail {

struct reverse_fn {
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

} // namespace detail

NANO_INLINE_VAR(detail::reverse_fn, reverse)

} // namespace view

NANO_END_NAMESPACE

#endif
