// nanorange/detail/range/access.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_DETAIL_RANGE_ACCESS_HPP_INCLUDED
#define NANORANGE_DETAIL_RANGE_ACCESS_HPP_INCLUDED

#include <nanorange/detail/functional/decay_copy.hpp>
#include <nanorange/detail/iterator/concepts.hpp>

#include <iterator> // for make_reverse_iterator

NANO_BEGIN_NAMESPACE

// [range.access.begin]

namespace detail {
namespace begin_ {

// MSVC doesn't mind this for some reason
template <typename T>
void begin(T&&) = delete;

template <typename T>
void begin(std::initializer_list<T>&&) = delete;

struct fn {
private:
    template <typename T, std::size_t N>
    static constexpr auto impl(T (&t)[N], priority_tag<2>) noexcept
        -> decltype((t) + 0)
    {
        return (t) + 0;
    }

    template <typename T>
    static constexpr auto
    impl(T& t, priority_tag<1>) noexcept(noexcept(decay_copy(t.begin())))
        -> std::enable_if_t<Iterator<decltype(decay_copy(t.begin()))>,
                            decltype(decay_copy(t.begin()))>
    {
        return decay_copy(t.begin());
    }

    template <typename T>
    static constexpr auto impl(T&& t, priority_tag<0>) noexcept(
        noexcept(decay_copy(begin(std::forward<T>(t)))))
        -> std::enable_if_t<
            Iterator<decltype(decay_copy(begin(std::forward<T>(t))))>,
            decltype(decay_copy(begin(std::forward<T>(t))))>
    {
        return decay_copy(begin(std::forward<T>(t)));
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

} // namespace begin_
} // namespace detail

NANO_INLINE_VAR(detail::begin_::fn, begin)

namespace detail {
namespace end_ {

template <typename T>
void end(T&&) = delete;

template <typename T>
void end(std::initializer_list<T>&&) = delete;

struct fn {
private:
    template <typename T, std::size_t N>
    static constexpr auto impl(T (&t)[N], priority_tag<2>) noexcept
        -> decltype(t + N)
    {
        return t + N;
    }

    template <typename T,
              typename S = decltype(decay_copy(std::declval<T&>().end())),
              typename I = decltype(ranges::begin(std::declval<T&>()))>
    static constexpr auto
    impl(T& t, priority_tag<1>) noexcept(noexcept(decay_copy(t.end())))
        -> std::enable_if_t<Sentinel<S, I>, decltype(decay_copy(t.end()))>
    {
        return decay_copy(t.end());
    }

    template <typename T,
              typename S = decltype(decay_copy(end(std::declval<T>()))),
              typename I = decltype(ranges::begin(std::declval<T>()))>
    static constexpr auto impl(T&& t, priority_tag<0>) noexcept(
        noexcept(decay_copy(end(std::forward<T>(t)))))
        -> std::enable_if_t<Sentinel<S, I>, S>
    {
        return decay_copy(end(std::forward<T>(t)));
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

} // namespace end_
} // namespace detail

NANO_INLINE_VAR(detail::end_::fn, end)

// [range.access.cbegin]

namespace detail {
namespace cbegin_ {

struct fn {

    template <typename T>
    constexpr auto operator()(const T& t) const
        noexcept(noexcept(ranges::begin(t))) -> decltype(ranges::begin(t))
    {
        return ranges::begin(t);
    }

    template <typename T>
    constexpr auto operator()(const T&& t) const
        noexcept(noexcept(ranges::begin(static_cast<const T&&>(t))))
            -> decltype(ranges::begin(static_cast<const T&&>(t)))
    {
        return ranges::begin(static_cast<const T&&>(t));
    }
};

} // namespace cbegin_
} // namespace detail

NANO_INLINE_VAR(detail::cbegin_::fn, cbegin)

// [ranges.access.cend]

namespace detail {
namespace cend_ {

struct fn {

    template <typename T>
    constexpr auto operator()(const T& t) const
        noexcept(noexcept(ranges::end(t))) -> decltype(ranges::end(t))
    {
        return ranges::end(t);
    }

    template <typename T>
    constexpr auto operator()(const T&& t) const
        noexcept(noexcept(ranges::end(static_cast<const T&&>(t))))
            -> decltype(ranges::end(static_cast<const T&&>(t)))
    {
        return ranges::end(static_cast<const T&&>(t));
    }
};

} // namespace cend_
} // namespace detail

NANO_INLINE_VAR(detail::cend_::fn, cend)

namespace detail {
namespace rbegin_ {

template <typename T>
void rbegin(T&&) = delete;

// second poison pill needed for MSVC
template <typename T>
void rbegin(const T&&) = delete;

struct fn {
private:
    template <typename T,
              typename I = decltype(decay_copy(std::declval<T&>().rbegin()))>
    static constexpr auto
    impl(T& t, priority_tag<2>) noexcept(noexcept(decay_copy(t.rbegin())))
        -> std::enable_if_t<Iterator<I>, I>
    {
        return t.rbegin();
    }

    template <typename T,
              typename I = decltype(decay_copy(rbegin(std::declval<T&&>())))>
    static constexpr auto impl(T&& t, priority_tag<1>) noexcept(
        noexcept(decay_copy(rbegin(std::forward<T>(t)))))
        -> std::enable_if_t<Iterator<I>, I>
    {
        return rbegin(std::forward<T>(t));
    }

    template <typename T,
              typename I = decltype(ranges::begin(std::declval<T&&>())),
              typename S = decltype(ranges::end(std::declval<T&&>()))>
    static constexpr auto impl(T&& t, priority_tag<0>) noexcept(
        noexcept(std::make_reverse_iterator(ranges::end(std::forward<T>(t)))))
        -> std::enable_if_t<Same<I, S> && BidirectionalIterator<I>,
                            decltype(std::make_reverse_iterator(
                                ranges::end(std::forward<T>(t))))>
    {
        return std::make_reverse_iterator(ranges::end(std::forward<T>(t)));
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

} // namespace rbegin_
} // namespace detail

NANO_INLINE_VAR(detail::rbegin_::fn, rbegin)

namespace detail {
namespace rend_ {

template <typename T>
void rend(T&&) = delete;

template <typename T>
void rend(const T&&) = delete;

struct fn {
private:
    template <typename T,
              typename I = decltype(ranges::begin(std::declval<T&>())),
              typename S = decltype(decay_copy(std::declval<T&>().rend()))>
    static constexpr auto
    impl(T& t, priority_tag<2>) noexcept(noexcept(decay_copy(t.rend())))
        -> std::enable_if_t<Sentinel<S, I>, S>
    {
        return t.rend();
    }

    template <typename T,
              typename I = decltype(ranges::begin(std::declval<T&&>())),
              typename S = decltype(decay_copy(rend(std::declval<T&&>())))>
    static constexpr auto impl(T&& t, priority_tag<1>) noexcept(
        noexcept(decay_copy(rend(std::forward<T>(t)))))
        -> std::enable_if_t<Sentinel<S, I>, S>
    {
        return rend(std::forward<T>(t));
    }

    template <typename T,
              typename I = decltype(ranges::begin(std::declval<T&&>())),
              typename S = decltype(ranges::end(std::declval<T&&>()))>
    static constexpr auto impl(T&& t, priority_tag<0>) noexcept(
        noexcept(std::make_reverse_iterator(ranges::begin(std::forward<T>(t)))))
        -> std::enable_if_t<Same<I, S> && BidirectionalIterator<I>,
                            decltype(std::make_reverse_iterator(
                                ranges::begin(std::forward<T>(t))))>
    {
        return std::make_reverse_iterator(ranges::begin(std::forward<T>(t)));
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

} // namespace rend_
} // namespace detail

NANO_INLINE_VAR(detail::rend_::fn, rend)

namespace detail {
namespace crbegin_ {

struct fn {
    template <typename T>
    constexpr auto operator()(const T& t) const
        noexcept(noexcept(ranges::rbegin(t))) -> decltype(ranges::rbegin(t))
    {
        return ranges::rbegin(t);
    }

    template <typename T>
    constexpr auto operator()(const T&& t) const
        noexcept(noexcept(ranges::rbegin(static_cast<const T&&>(t))))
            -> decltype(ranges::rbegin(static_cast<const T&&>(t)))
    {
        return ranges::rbegin(static_cast<const T&&>(t));
    }
};

} // namespace crbegin_
} // namespace detail

NANO_INLINE_VAR(detail::crbegin_::fn, crbegin)

namespace detail {
namespace crend_ {

struct fn {
    template <typename T>
    constexpr auto operator()(const T& t) const
        noexcept(noexcept(ranges::rend(t))) -> decltype(ranges::rend(t))
    {
        return ranges::rend(t);
    }

    template <typename T>
    constexpr auto operator()(const T&& t) const
        noexcept(noexcept(ranges::rend(static_cast<const T&&>(t))))
            -> decltype(ranges::rend(static_cast<const T&&>(t)))
    {
        return ranges::rend(static_cast<const T&&>(t));
    }
};

} // namespace crend_
} // namespace detail

NANO_INLINE_VAR(detail::crend_::fn, crend)

NANO_END_NAMESPACE

#endif
