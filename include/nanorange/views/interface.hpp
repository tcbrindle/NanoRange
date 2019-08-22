// nanorange/views/interface.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_VIEWS_INTERFACE_HPP_INCLUDED
#define NANORANGE_VIEWS_INTERFACE_HPP_INCLUDED

#include <nanorange/detail/ranges/concepts.hpp>
#include <nanorange/iterator/common_iterator.hpp>
#include <nanorange/iterator/operations.hpp>

NANO_BEGIN_NAMESPACE

// [ranges.view_interface]

namespace detail {

template <typename, typename = void>
struct range_common_iterator_impl;

template <typename R>
struct range_common_iterator_impl<
    R, std::enable_if_t<range<R> && !common_range<R>>> {
    using type = common_iterator<iterator_t<R>, sentinel_t<R>>;
};

template <typename R>
struct range_common_iterator_impl<R, std::enable_if_t<common_range<R>>> {
    using type = iterator_t<R>;
};

template <typename R>
using range_common_iterator_t = typename range_common_iterator_impl<R>::type;

} // namespace detail

template <typename D>
class view_interface : public view_base {

    static_assert(std::is_class<D>::value, "");
    static_assert(same_as<D, std::remove_cv_t<D>>, "");

private:
    constexpr D& derived() noexcept { return static_cast<D&>(*this); }

    constexpr const D& derived() const noexcept
    {
        return static_cast<const D&>(*this);
    }

public:
    template <typename R = D>
    [[nodiscard]] constexpr auto empty()
        -> std::enable_if_t<forward_range<R>, bool>
    {
        return ranges::begin(derived()) == ranges::end(derived());
    }

    template <typename R = D>
    [[nodiscard]] constexpr auto empty() const
        -> std::enable_if_t<forward_range<const R>, bool>
    {
        return ranges::begin(derived()) == ranges::end(derived());
    }

    template <typename R = D, typename = decltype(ranges::empty(std::declval<R&>()))>
    constexpr explicit operator bool()
    {
        return !ranges::empty(derived());
    }

    template <typename R = D, typename = decltype(ranges::empty(std::declval<const R&>()))>
    constexpr explicit operator bool() const
    {
        return !ranges::empty(derived());
    }

    template <typename R = D, typename = std::enable_if_t<contiguous_iterator<iterator_t<R>>>>
    constexpr auto data()
    {
        return ranges::empty(derived()) ? nullptr : std::addressof(*ranges::begin(derived()));
    }

    template <typename R = D, typename = std::enable_if_t<
                  range<const R> && contiguous_iterator<iterator_t<const R>>>>
    constexpr auto data() const
    {
        return ranges::empty(derived()) ? nullptr : std::addressof(*ranges::begin(derived()));
    }

    template <typename R = D, typename = std::enable_if_t<
                  forward_range<R> &&
                  sized_sentinel_for<sentinel_t<R>, iterator_t<R>>>>
    constexpr auto size()
    {
        return ranges::end(derived()) - ranges::begin(derived());
    }

    template <typename R = D, typename = std::enable_if_t<
                  forward_range<const R> &&
                  sized_sentinel_for<sentinel_t<const R>, iterator_t<const R>>>>
    constexpr auto size() const
    {
        return ranges::end(derived()) - ranges::begin(derived());
    }

    template <typename R = D, typename = std::enable_if_t<forward_range<R>>>
    constexpr decltype(auto) front()
    {
        return *ranges::begin(derived());
    }

    template <typename R = D, typename = std::enable_if_t<forward_range<const R>>>
    constexpr decltype(auto) front() const
    {
        return *ranges::begin(derived());
    }

    template <typename R = D, typename = std::enable_if_t<
                                  bidirectional_range<R> && common_range<R>>>
    constexpr decltype(auto) back()
    {
        return *ranges::prev(ranges::end(derived()));
    }

    template <typename R = D, typename = std::enable_if_t<bidirectional_range<const R> &&
                                          common_range<const R>>>
    constexpr decltype(auto) back() const
    {
        return *ranges::prev(ranges::end(derived()));
    }

    template <typename R = D, typename = std::enable_if_t<random_access_range<R>>>
    constexpr decltype(auto) operator[](iter_difference_t<iterator_t<R>> n)
    {
        return ranges::begin(derived())[n];
    }

    template <typename R = D,  typename = std::enable_if_t<random_access_range<const R>>>
    constexpr decltype(auto) operator[](iter_difference_t<iterator_t<const R>> n) const
    {
        return ranges::begin(derived())[n];
    }
};

NANO_END_NAMESPACE

#endif
