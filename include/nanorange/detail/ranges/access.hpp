// nanorange/detail/ranges/access.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_DETAIL_RANGES_ACCESS_HPP_INCLUDED
#define NANORANGE_DETAIL_RANGES_ACCESS_HPP_INCLUDED

#include <nanorange/detail/ranges/begin_end.hpp>
#include <nanorange/iterator/reverse_iterator.hpp>

NANO_BEGIN_NAMESPACE

namespace detail {
namespace rbegin_ {

template <typename T>
void rbegin(T&) = delete;

template <typename T>
void rbegin(const T&) = delete;

struct fn {
private:
    template <typename T,
              std::enable_if_t<
                  !std::is_lvalue_reference_v<T> &&
                  !enable_borrowed_range<std::remove_cv_t<T>>, int> = 0>
    static constexpr void impl(T&&, priority_tag<3>) = delete;

    template <typename T,
              typename I = decltype(decay_copy(std::declval<T>().rbegin()))>
    static constexpr auto
    impl(T&& t, priority_tag<2>) noexcept(noexcept(decay_copy(std::forward<T>(t).rbegin())))
        -> std::enable_if_t<input_or_output_iterator<I>, I>
    {
        return std::forward<T>(t).rbegin();
    }

    template <typename T,
              typename I = decltype(decay_copy(rbegin(std::declval<T>())))>
    static constexpr auto impl(T&& t, priority_tag<1>) noexcept(
        noexcept(decay_copy(rbegin(std::forward<T>(t)))))
        -> std::enable_if_t<input_or_output_iterator<I>, I>
    {
        return rbegin(std::forward<T>(t));
    }

    template <typename T,
              typename I = decltype(ranges::begin(std::declval<T>())),
              typename S = decltype(ranges::end(std::declval<T>()))>
    static constexpr auto impl(T&& t, priority_tag<0>) noexcept(
        noexcept(ranges::make_reverse_iterator(ranges::end(std::forward<T>(t)))))
        -> std::enable_if_t<same_as<I, S> && bidirectional_iterator<I>,
                            decltype(ranges::make_reverse_iterator(
                                ranges::end(std::forward<T>(t))))>
    {
        return ranges::make_reverse_iterator(ranges::end(std::forward<T>(t)));
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

} // namespace rbegin_
} // namespace detail

NANO_INLINE_VAR(detail::rbegin_::fn, rbegin)

namespace detail {
namespace rend_ {

template <typename T>
void rend(T&) = delete;

template <typename T>
void rend(const T&) = delete;

struct fn {
private:
    template <typename T,
              std::enable_if_t<
                  !std::is_lvalue_reference_v<T> &&
                  !enable_borrowed_range<std::remove_cv_t<T>>, int> = 0>
    static constexpr void impl(T&&, priority_tag<3>) = delete;

    template <typename T,
              typename I = decltype(ranges::rbegin(std::declval<T>())),
              typename S = decltype(decay_copy(std::declval<T>().rend()))>
    static constexpr auto
    impl(T&& t, priority_tag<2>) noexcept(noexcept(decay_copy(std::forward<T>(t).rend())))
        -> std::enable_if_t<sentinel_for<S, I>, S>
    {
        return std::forward<T>(t).rend();
    }

    template <typename T,
              typename I = decltype(ranges::rbegin(std::declval<T>())),
              typename S = decltype(decay_copy(rend(std::declval<T>())))>
    static constexpr auto impl(T&& t, priority_tag<1>) noexcept(
        noexcept(decay_copy(rend(std::forward<T>(t)))))
        -> std::enable_if_t<sentinel_for<S, I>, S>
    {
        return rend(std::forward<T>(t));
    }

    template <typename T,
              typename I = decltype(ranges::begin(std::declval<T>())),
              typename S = decltype(ranges::end(std::declval<T>()))>
    static constexpr auto impl(T&& t, priority_tag<0>) noexcept(
        noexcept(ranges::make_reverse_iterator(ranges::begin(std::forward<T>(t)))))
        -> std::enable_if_t<same_as<I, S> && bidirectional_iterator<I>,
                            decltype(ranges::make_reverse_iterator(
                                ranges::begin(std::forward<T>(t))))>
    {
        return ranges::make_reverse_iterator(ranges::begin(std::forward<T>(t)));
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

} // namespace rend_
} // namespace detail

NANO_INLINE_VAR(detail::rend_::fn, rend)

namespace detail {
namespace crbegin_ {

struct fn {
private:
    template <typename T, typename U = std::remove_reference_t<T>,
              std::enable_if_t<std::is_lvalue_reference_v<T>, int> = 0>
    static constexpr auto impl(T&& t)
        noexcept(noexcept(ranges::rbegin(static_cast<const U&>(t))))
        -> decltype(ranges::rbegin(static_cast<const U&>(t)))
    {
        return ranges::rbegin(static_cast<const U&>(t));
    }

    template <typename T,
              std::enable_if_t<!std::is_lvalue_reference_v<T>, int> = 0>
    static constexpr auto impl(T&& t)
        noexcept(noexcept(ranges::rbegin(static_cast<const T&&>(t))))
            -> decltype(ranges::rbegin(static_cast<const T&&>(t)))
    {
        return ranges::rbegin(static_cast<const T&&>(t));
    }

public:
    template <typename T>
    constexpr auto operator()(T&& t) const
        noexcept(noexcept(fn::impl(std::forward<T>(t))))
        -> decltype(fn::impl(std::forward<T>(t)))
    {
        return fn::impl(std::forward<T>(t));
    }
};

} // namespace crbegin_
} // namespace detail

NANO_INLINE_VAR(detail::crbegin_::fn, crbegin)

namespace detail {
namespace crend_ {

struct fn {
private:
    template <typename T, typename U = std::remove_reference_t<T>,
              std::enable_if_t<std::is_lvalue_reference_v<T>, int> = 0>
    static constexpr auto impl(T&& t)
        noexcept(noexcept(ranges::rend(static_cast<const U&>(t))))
        -> decltype(ranges::rend(static_cast<const U&>(t)))
    {
        return ranges::rend(static_cast<const U&>(t));
    }

    template <typename T,
              std::enable_if_t<!std::is_lvalue_reference_v<T>, int> = 0>
    static constexpr auto impl(T&& t)
        noexcept(noexcept(ranges::rend(static_cast<const T&&>(t))))
            -> decltype(ranges::rend(static_cast<const T&&>(t)))
    {
        return ranges::rend(static_cast<const T&&>(t));
    }

public:
    template <typename T>
    constexpr auto operator()(T&& t) const
        noexcept(noexcept(fn::impl(std::forward<T>(t))))
        -> decltype(fn::impl(std::forward<T>(t)))
    {
        return fn::impl(std::forward<T>(t));
    }
};

} // namespace crend_
} // namespace detail

NANO_INLINE_VAR(detail::crend_::fn, crend)

NANO_END_NAMESPACE

#endif
