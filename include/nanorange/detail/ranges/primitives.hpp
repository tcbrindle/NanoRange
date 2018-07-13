// nanorange/detail/ranges/primitives.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_DETAIL_RANGES_PRIMITIVES_HPP_INCLUDED
#define NANORANGE_DETAIL_RANGES_PRIMITIVES_HPP_INCLUDED

#include <nanorange/detail/ranges/access.hpp>

NANO_BEGIN_NAMESPACE

// [range.primitives.size]

template <typename T>
constexpr bool disable_sized_range = false;

namespace detail {
namespace size_ {

template <typename T>
void size(T&&) = delete;

// For some reason MSVC doesn't mind poison pills,
// as long as there are two
template <typename T>
void size(T&) = delete;

struct fn {
private:
    template <typename T, std::size_t N>
    static constexpr std::size_t impl(const T(&&)[N], priority_tag<3>) noexcept
    {
        return N;
    }

    template <typename T, std::size_t N>
    static constexpr std::size_t impl(const T (&)[N], priority_tag<3>) noexcept
    {
        return N;
    }

    template <typename T,
              typename I = decltype(decay_copy(std::declval<T>().size()))>
    static constexpr auto impl(T&& t, priority_tag<2>) noexcept(
        noexcept(decay_copy(std::forward<T>(t).size())))
        -> std::enable_if_t<
            Integral<I> && !disable_sized_range<remove_cvref_t<T>>, I>
    {
        return decay_copy(std::forward<T>(t).size());
    }

    template <typename T,
              typename I = decltype(decay_copy(size(std::declval<T>())))>
    static constexpr auto impl(T&& t, priority_tag<1>) noexcept(
        noexcept(decay_copy(size(std::forward<T>(t)))))
        -> std::enable_if_t<
            Integral<I> && !disable_sized_range<remove_cvref_t<T>>, I>
    {
        return decay_copy(size(std::forward<T>(t)));
    }

    template <typename T,
              typename I = decltype(ranges::begin(std::declval<T>())),
              typename S = decltype(ranges::end(std::declval<T>())),
              typename D = decltype(decay_copy(std::declval<S>() -
                                               std::declval<I>()))>
    static constexpr auto impl(T&& t, priority_tag<0>) noexcept(
        noexcept(decay_copy(ranges::end(t) - ranges::begin(t))))
        -> std::enable_if_t<
            !std::is_array<remove_cvref_t<T>>::value && // MSVC sillyness?
                SizedSentinel<S, I> && ForwardIterator<I>,
            D>
    {
        return decay_copy(ranges::end(t) - ranges::begin(t));
    }

public:
    template <typename T>
    constexpr auto operator()(T&& t) const
        noexcept(noexcept(fn::impl(std::forward<T>(t), priority_tag<3>{})))
            -> decltype(fn::impl(std::forward<T>(t), priority_tag<3>{}))
    {
        return fn::impl(std::forward<T>(t), priority_tag<3>{});
    }
};

} // namespace size_
} // namespace detail

NANO_INLINE_VAR(detail::size_::fn, size)

// [range.primitives.empty]

namespace detail {
namespace empty_ {

struct fn {
private:
    template <typename T>
    static constexpr auto
    impl(T&& t,
         priority_tag<2>) noexcept(noexcept((bool(std::forward<T>(t).empty()))))
        -> decltype((bool(std::forward<T>(t).empty())))
    {
        return bool((std::forward<T>(t).empty()));
    }

    template <typename T>
    static constexpr auto impl(T&& t, priority_tag<1>) noexcept(
        noexcept(ranges::size(std::forward<T>(t)) == 0))
        -> decltype(ranges::size(std::forward<T>(t)) == 0)
    {
        return ranges::size(std::forward<T>(t)) == 0;
    }

    template <typename T,
              typename I = decltype(ranges::begin(std::declval<T>()))>
    static constexpr auto
    impl(T&& t,
         priority_tag<0>) noexcept(noexcept(ranges::begin(t) == ranges::end(t)))
        -> std::enable_if_t<ForwardIterator<I>,
                            decltype(ranges::begin(t) == ranges::end(t))>
    {
        return ranges::begin(t) == ranges::end(t);
    }

public:
    template <typename T>
    constexpr auto operator()(T&& t) const
        noexcept(noexcept(fn::impl(std::forward<T>(t), priority_tag<2>{})))
            -> decltype(fn::impl(std::forward<T>(t), priority_tag<2>{}))
    {
        return fn::impl(std::forward<T>(t), priority_tag<2>{});
    }
};

} // namespace empty_
} // namespace detail

NANO_INLINE_VAR(detail::empty_::fn, empty)

namespace detail {

template <typename P>
constexpr bool is_object_pointer_v =
    std::is_pointer<P>::value && std::is_object<test_t<iter_value_t, P>>::value;

namespace data_ {

struct fn {
private:
    template <typename T, typename D = decltype(decay_copy(std::declval<T&>().data()))>
    static constexpr auto
    impl(T& t, priority_tag<1>) noexcept(noexcept(decay_copy(t.data())))
        -> std::enable_if_t<is_object_pointer_v<D>, D>
    {
        return t.data();
    }

    template <typename T>
    static constexpr auto
    impl(T&& t,
         priority_tag<0>) noexcept(noexcept(ranges::begin(std::forward<T>(t))))
        -> std::enable_if_t<
            is_object_pointer_v<decltype(ranges::begin(std::forward<T>(t)))>,
            decltype(ranges::begin(std::forward<T>(t)))>
    {
        return ranges::begin(std::forward<T>(t));
    }

public:
    template <typename T>
    constexpr auto operator()(T&& t) const
        noexcept(noexcept(fn::impl(std::forward<T>(t), priority_tag<1>{})))
            -> decltype(fn::impl(std::forward<T>(t), priority_tag<1>{}))
    {
        return fn::impl(std::forward<T>(t), priority_tag<1>{});
    }
};

} // namespace data_
} // namespace detail

NANO_INLINE_VAR(detail::data_::fn, data)

NANO_END_NAMESPACE

#endif