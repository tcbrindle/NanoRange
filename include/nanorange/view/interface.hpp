// nanorange/view/interface.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_VIEW_INTERFACE_HPP_INCLUDED
#define NANORANGE_VIEW_INTERFACE_HPP_INCLUDED

#include <nanorange/detail/range/concepts.hpp>
#include <nanorange/iterator/common_iterator.hpp>

NANO_BEGIN_NAMESPACE

// [ranges.view_interface]

namespace detail {

template <typename, typename = void>
struct range_common_iterator_impl;

template <typename R>
struct range_common_iterator_impl<
    R, std::enable_if_t<Range<R> && !CommonRange<R>>> {
    using type = common_iterator<iterator_t<R>, sentinel_t<R>>;
};

template <typename R>
struct range_common_iterator_impl<R, std::enable_if_t<CommonRange<R>>> {
    using type = iterator_t<R>;
};

template <typename R>
using range_common_iterator_t = typename range_common_iterator_impl<R>::type;

} // namespace detail

template <typename D>
class view_interface {

    static_assert(std::is_class<D>::value, "");

private:
    constexpr D& derived() noexcept { return static_cast<D&>(*this); }

    constexpr const D& derived() const noexcept
    {
        return static_cast<const D&>(*this);
    }

public:
    template <typename R = D>
    constexpr auto empty() -> std::enable_if_t<ForwardRange<R>, bool>
    {
        return ranges::begin(derived()) == ranges::end(derived());
    }

    template <typename = decltype(ranges::empty(std::declval<const D&>()))>
    constexpr explicit operator bool() const
    {
        return ranges::begin(derived());
    }

    template <typename R = D>
    constexpr auto data() const
        -> std::enable_if_t<RandomAccessRange<R> &&
                                std::is_pointer<iterator_t<R>>::value,
                            decltype(ranges::begin(derived()))>
    {
        return ranges::begin(derived());
    }

    template <typename R = D>
    constexpr auto size() const -> std::enable_if_t<
        ForwardRange<const R> &&
            SizedSentinel<sentinel_t<const R>, iterator_t<const R>>,
        decltype(ranges::end(derived()) - ranges::begin(derived()))>
    {
        return ranges::end(derived()) - ranges::begin(derived());
    }

    template <typename R = D>
    constexpr auto front()
        -> std::enable_if_t<ForwardRange<R>,
                            decltype(*ranges::begin(derived()))>
    {
        return *ranges::begin(derived());
    }

    template <typename R = D>
    constexpr auto front() const
        -> std::enable_if_t<ForwardRange<const R>,
                            decltype(*ranges::begin(derived()))>
    {
        return *ranges::begin(derived());
    }

    template <typename R = D>
    constexpr auto back()
        -> std::enable_if_t<BidirectionalRange<R> && CommonRange<R>,
                            decltype(*prev(ranges::end(derived())))>
    {
        return *prev(ranges::end(derived()));
    }

    template <typename R = D>
    constexpr auto back()
        -> std::enable_if_t<BidirectionalRange<const R> && CommonRange<const R>,
                            decltype(*prev(ranges::end(derived())))>
    {
        return *prev(ranges::end(derived()));
    }

    template <typename R = D>
    constexpr auto operator[](difference_type_t<iterator_t<R>> n)
        -> std::enable_if_t<RandomAccessRange<R>,
                            decltype(ranges::begin(derived())[n])>
    {
        return ranges::begin(derived())[n];
    }

    template <typename R = const D>
    constexpr auto operator[](difference_type_t<iterator_t<R>> n) const
        -> std::enable_if_t<RandomAccessRange<R>,
                            decltype(ranges::begin(derived())[n])>
    {
        return ranges::begin(derived())[n];
    }

    template <typename R = D>
    constexpr auto at(difference_type_t<iterator_t<R>> n)
        -> std::enable_if_t<RandomAccessRange<R> && SizedRange<R>,
                            decltype(derived()[n])>
    {
        if (n < 0 || n >= ranges::size(derived())) {
            throw std::out_of_range{""};
        }

        return derived()[n];
    }

    template <typename R = const D>
    constexpr auto at(difference_type_t<iterator_t<R>> n) const
        -> std::enable_if_t<RandomAccessRange<R> && SizedRange<R>,
                            decltype(derived()[n])>
    {
        if (n < 0 || n >= ranges::size(derived())) {
            throw std::out_of_range{""};
        }

        return derived()[n];
    }

    template <typename C,
              typename = std::enable_if_t<
                  ForwardRange<C> && !View<C> &&
                  ConvertibleTo<reference_t<iterator_t<const D>>,
                                value_type_t<iterator_t<C>>> &&
                  Constructible<C, detail::range_common_iterator_t<const D>,
                                detail::range_common_iterator_t<const D>>>>
    operator C() const
    {
        using I = detail::range_common_iterator_t<D>;
        return C(I{ranges::begin(derived()), ranges::end(derived())});
    }
};

NANO_END_NAMESPACE

#endif
