// nanorange/views/common.hpp
//
// Copyright (c) 2019 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_VIEWS_COMMON_HPP_INCLUDED
#define NANORANGE_VIEWS_COMMON_HPP_INCLUDED

#include <nanorange/iterator/common_iterator.hpp>
#include <nanorange/views/all.hpp>

NANO_BEGIN_NAMESPACE

template <typename V>
class common_view : public view_interface<common_view<V>> {

    static_assert(view<V> && !common_range<V>, "");

    template <typename VV>
    using random_and_sized_t =
            std::integral_constant<bool,
                               random_access_range<VV> && sized_range<VV>>;


    V base_ = V();

    template <typename VV>
    static constexpr auto do_begin(VV& base, std::true_type)
    {
        return ranges::begin(base);
    }

    template <typename VV>
    static constexpr auto do_begin(VV& base, std::false_type)
    {
        return common_iterator<iterator_t<VV>, sentinel_t<VV>>(
                ranges::begin(base));
    }

    template <typename VV>
    static constexpr auto do_end(VV& base, std::true_type)
    {
        return ranges::begin(base) + ranges::size(base);
    }

    template <typename VV>
    static constexpr auto do_end(VV& base, std::false_type)
    {
        return common_iterator<iterator_t<VV>, sentinel_t<VV>>(
                ranges::end(base));
    }


public:
    common_view() = default;

    constexpr explicit common_view(V r)
        : base_(std::move(r))
    {}

    template <typename R,
        std::enable_if_t<detail::not_same_as<R, common_view>, int> = 0,
        std::enable_if_t<viewable_range<R> &&
                !common_range<R> &&
                constructible_from<V, all_view<R>>, int> = 0>
    constexpr explicit common_view(R&& r)
        : base_(views::all(std::forward<R>(r)))
    {}

    constexpr V base() const { return base_; }

    template <typename VV = V, std::enable_if_t<sized_range<VV>, int> = 0>
    constexpr auto size() { return ranges::size(base_); }

    template <typename VV = V, std::enable_if_t<sized_range<const VV>, int> = 0>
    constexpr auto size() const { return ranges::size(base_); }

    constexpr auto begin()
    {
        return do_begin<V>(base_, random_and_sized_t<V>{});
    }

    template <typename VV = V, std::enable_if_t<range<const VV>, int> = 0>
    constexpr auto begin() const
    {
        return do_begin<const V>(base_, random_and_sized_t<const V>{});
    }

    constexpr auto end()
    {
        return do_end<V>(base_, random_and_sized_t<V>{});
    }

    template <typename VV = V, std::enable_if_t<range<const VV>, int> = 0>
    constexpr auto end() const
    {
        return do_end<const V>(base_, random_and_sized_t<const V>{});
    }

};

template <typename R>
common_view(R&&) -> common_view<all_view<R>>;

namespace detail {

struct common_view_fn {
private:
    template <typename T>
    static constexpr auto impl(T&& t, nano::detail::priority_tag<1>)
        noexcept(noexcept(views::all(std::forward<T>(t))))
        -> std::enable_if_t<common_range<T>,
            decltype(views::all(std::forward<T>(t)))>
    {
        return views::all(std::forward<T>(t));
    }

    template <typename T>
    static constexpr auto impl(T&& t, nano::detail::priority_tag<0>)
        -> common_view<all_view<T>>
    {
        return common_view<all_view<T>>{std::forward<T>(t)};
    }

public:
    template <typename T>
    constexpr auto operator()(T&& t) const
        -> std::enable_if_t<
        viewable_range<T>,
        decltype(common_view_fn::impl(std::forward<T>(t),
                                    nano::detail::priority_tag<1>{}))>
    {
        return common_view_fn::impl(std::forward<T>(t),
                               nano::detail::priority_tag<1>{});
    }
};

template <>
inline constexpr bool is_raco<common_view_fn> = true;

} // namespace detail

namespace views {

NANO_INLINE_VAR(::nano::detail::common_view_fn, common)

} // namespace views

NANO_END_NAMESPACE

#endif
