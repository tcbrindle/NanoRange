// nanorange/views/reverse.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_VIEWS_REVERSE_HPP_INCLUDED
#define NANORANGE_VIEWS_REVERSE_HPP_INCLUDED

#include <nanorange/iterator/reverse_iterator.hpp>
#include <nanorange/views/all.hpp>

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
      private detail::reverse_view_cache<common_range<V>, iterator_t<V>> {

    static_assert(view<V>);
    static_assert(bidirectional_range<V>);

    reverse_view() = default;

    constexpr explicit reverse_view(V r)
        : base_(std::move(r))
    {}

    template <typename R,
              /* FIXME: This is not to spec, but we get in horrible recursive trouble if it's omitted */
              std::enable_if_t<detail::not_same_as<R, reverse_view>, int> = 0,
              std::enable_if_t<viewable_range<R> && bidirectional_range<R> &&
                  constructible_from<V, all_view<R>>, int> = 0>
    constexpr explicit reverse_view(R&& r)
        : base_(views::all(std::forward<R>(r)))
    {}

    constexpr reverse_view(const reverse_view& other)
        : base_(other.base_)
    {}

    constexpr V base() const { return base_; }

    constexpr reverse_iterator<iterator_t<V>> begin()
    {
        if constexpr (common_range<V>) {
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
        -> std::enable_if_t<common_range<const VV>,
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
        -> std::enable_if_t<common_range<const VV>, reverse_iterator<iterator_t<const VV>>>
    {
        return nano::make_reverse_iterator(ranges::begin(base_));
    }

    template <typename VV = V, std::enable_if_t<sized_range<VV>, int> = 0>
    constexpr auto size()
    {
        return ranges::size(base_);
    }

    template <typename VV = V, std::enable_if_t<sized_range<const VV>, int> = 0>
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
private:
    template <typename R>
    static constexpr auto impl(reverse_view<R> r, detail::priority_tag<1>)
    {
        return r.base();
    }

    template <typename I, subrange_kind K>
    static constexpr auto impl(subrange<reverse_iterator<I>, reverse_iterator<I>, K> s,
                               detail::priority_tag<1>)
    {
        if constexpr (K == subrange_kind::sized) {
            return subrange<I, I, K>(s.end().base(), s.begin().base(), s.size());
        } else {
            return subrange<I, I, K>(s.end().base(), s.begin().base());
        }
    }

    template <typename R>
    static constexpr auto impl(R&& r, detail::priority_tag<0>)
        -> decltype(reverse_view{std::forward<R>(r)})
    {
        return reverse_view{std::forward<R>(r)};
    }

public:
    template <typename R>
    constexpr auto operator()(R&& r) const
        -> decltype(impl(std::forward<R>(r), priority_tag<1>{}))
    {
        return impl(std::forward<R>(r), priority_tag<1>{});
    }
};

template <>
inline constexpr bool is_raco<reverse_view_fn> = true;

} // namespace detail

namespace views {

NANO_INLINE_VAR(nano::detail::reverse_view_fn, reverse)

} // namespace views

NANO_END_NAMESPACE

#endif
