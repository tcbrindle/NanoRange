// nanorange/detail/ranges/begin_end.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_DETAIL_RANGES_BEGIN_END_HPP_INCLUDED
#define NANORANGE_DETAIL_RANGES_BEGIN_END_HPP_INCLUDED

#include <nanorange/detail/functional/decay_copy.hpp>
#include <nanorange/detail/iterator/concepts.hpp>
#include <nanorange/detail/ranges/borrowed_range.hpp>

NANO_BEGIN_NAMESPACE

// [range.access.begin]

namespace detail {
namespace begin_ {

template <typename T>
void begin(T&) = delete;

template <typename T>
void begin(const T&) = delete;

struct fn {
private:
    template <typename T,
              std::enable_if_t<
                  !std::is_lvalue_reference_v<T> &&
                  !enable_borrowed_range<std::remove_cv_t<T>>, int> = 0>
    static constexpr void impl(T&&, priority_tag<3>) = delete;

    template <typename T,
              std::enable_if_t<std::is_array_v<remove_cvref_t<T>>, int> = 0>
    static constexpr auto impl(T&& t, priority_tag<2>) noexcept
        -> decltype(t + 0)
    {
        return t + 0;
    }

    template <typename T>
    static constexpr auto
    impl(T&& t, priority_tag<1>)
        noexcept(noexcept(decay_copy(std::forward<T>(t).begin())))
        -> std::enable_if_t<
            input_or_output_iterator<
                decltype(decay_copy(std::forward<T>(t).begin()))>,
                decltype(decay_copy(std::forward<T>(t).begin()))>
    {
        return decay_copy(t.begin());
    }

    template <typename T>
    static constexpr auto impl(T&& t, priority_tag<0>) noexcept(
        noexcept(decay_copy(begin(std::forward<T>(t)))))
        -> std::enable_if_t<input_or_output_iterator<decltype(decay_copy(begin(std::forward<T>(t))))>,
            decltype(decay_copy(begin(std::forward<T>(t))))>
    {
        return decay_copy(begin(std::forward<T>(t)));
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

} // namespace begin_
} // namespace detail

NANO_INLINE_VAR(detail::begin_::fn, begin)

namespace detail {
namespace end_ {

template <typename T>
void end(T&) = delete;

template <typename T>
void end(const T&) = delete;

struct fn {
private:
    template <typename T,
              std::enable_if_t<
                  !std::is_lvalue_reference_v<T> &&
                  !enable_borrowed_range<std::remove_cv_t<T>>, int> = 0>
    static constexpr void impl(T&&, priority_tag<3>) = delete;

    template <typename T,
              std::enable_if_t<std::is_array_v<remove_cvref_t<T>>, int> = 0>
    static constexpr auto impl(T&& t, priority_tag<2>) noexcept
        -> decltype(t + std::extent_v<remove_cvref_t<T>>)
    {
        return t + std::extent_v<remove_cvref_t<T>>;
    }

    template <typename T,
              typename S = decltype(decay_copy(std::declval<T>().end())),
              typename I = decltype(ranges::begin(std::declval<T>()))>
    static constexpr auto
    impl(T&& t, priority_tag<1>)
        noexcept(noexcept(decay_copy(std::forward<T>(t).end())))
        -> std::enable_if_t<sentinel_for<S, I>,
                            decltype(decay_copy(std::forward<T>(t).end()))>
    {
        return decay_copy(std::forward<T>(t).end());
    }

    template <typename T,
              typename S = decltype(decay_copy(end(std::declval<T>()))),
              typename I = decltype(ranges::begin(std::declval<T>()))>
    static constexpr auto impl(T&& t, priority_tag<0>) noexcept(
        noexcept(decay_copy(end(std::forward<T>(t)))))
        -> std::enable_if_t<sentinel_for<S, I>, S>
    {
        return decay_copy(end(std::forward<T>(t)));
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

} // namespace end_
} // namespace detail

NANO_INLINE_VAR(detail::end_::fn, end)

// [range.access.cbegin]

namespace detail {
namespace cbegin_ {

struct fn {
private:
    template <typename T, typename U = std::remove_reference_t<T>,
              std::enable_if_t<std::is_lvalue_reference_v<T>, int> = 0>
    static constexpr auto impl(T&& t)
        noexcept(noexcept(ranges::begin(static_cast<const U&>(t))))
        -> decltype(ranges::begin(static_cast<const U&>(t)))
    {
        return ranges::begin(static_cast<const U&>(t));
    }

    template <typename T,
              std::enable_if_t<!std::is_lvalue_reference_v<T>, int> = 0>
    static constexpr auto impl(T&& t)
        noexcept(noexcept(ranges::begin(static_cast<const T&&>(std::forward<T>(t)))))
        -> decltype(ranges::begin(static_cast<const T&&>(std::forward<T>(t))))
    {
        return ranges::begin(static_cast<const T&&>(std::forward<T>(t)));
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

} // namespace cbegin_
} // namespace detail

NANO_INLINE_VAR(detail::cbegin_::fn, cbegin)

// [ranges.access.cend]

namespace detail {
namespace cend_ {

struct fn {
private:
    template <typename T, typename U = std::remove_reference_t<T>,
              std::enable_if_t<std::is_lvalue_reference_v<T>, int> = 0>
    static constexpr auto impl(T&& t)
        noexcept(noexcept(ranges::end(static_cast<const U&>(t))))
        -> decltype(ranges::end(static_cast<const U&>(t)))
    {
        return ranges::end(static_cast<const U&>(t));
    }

    template <typename T,
              std::enable_if_t<!std::is_lvalue_reference_v<T>, int> = 0>
    static constexpr auto impl(T&& t)
        noexcept(noexcept(ranges::end(static_cast<const T&&>(std::forward<T>(t)))))
        -> decltype(ranges::end(static_cast<const T&&>(std::forward<T>(t))))
    {
        return ranges::end(static_cast<const T&&>(std::forward<T>(t)));
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

} // namespace cend_
} // namespace detail

NANO_INLINE_VAR(detail::cend_::fn, cend)

NANO_END_NAMESPACE

#endif
