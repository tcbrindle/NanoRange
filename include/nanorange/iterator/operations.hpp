// nanorange/iterator/operations.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_ITERATOR_OPERATIONS_HPP_INCLUDED
#define NANORANGE_ITERATOR_OPERATIONS_HPP_INCLUDED

#include <nanorange/detail/iterator/concepts.hpp>
#include <nanorange/detail/ranges/begin_end.hpp>
#include <nanorange/detail/ranges/concepts.hpp>
#include <nanorange/detail/ranges/primitives.hpp>

NANO_BEGIN_NAMESPACE

namespace detail {
namespace advance_ {

struct fn {
private:
    template <typename T>
    static constexpr T abs(T t)
    {
        if (t < T{0}) {
            return -t;
        }
        return t;
    }

    template <typename R>
    static constexpr auto impl(R& r, iter_difference_t<R> n)
        -> std::enable_if_t<random_access_iterator<R>>
    {
        r += n;
    }

    template <typename I>
    static constexpr auto impl(I& i, iter_difference_t<I> n)
        -> std::enable_if_t<bidirectional_iterator<I> &&
                            !random_access_iterator<I>>
    {
        constexpr auto zero = iter_difference_t<I>{0};

        if (n > zero) {
            while (n-- > zero) {
                ++i;
            }
        } else {
            while (n++ < zero) {
                --i;
            }
        }
    }

    template <typename I>
    static constexpr auto impl(I& i, iter_difference_t<I> n)
        -> std::enable_if_t<!bidirectional_iterator<I>>
    {
        while (n-- > iter_difference_t<I>{0}) {
            ++i;
        }
    }

    template <typename I, typename S>
    static constexpr auto impl(I& i, S bound, priority_tag<2>)
        -> std::enable_if_t<assignable_from<I&, S>>
    {
        i = std::move(bound);
    }

    template <typename I, typename S>
    static constexpr auto impl(I& i, S bound, priority_tag<1>)
        -> std::enable_if_t<sized_sentinel_for<S, I>>
    {
        fn::impl(i, bound - i);
    }

    template <typename I, typename S>
    static constexpr void impl(I& i, S bound, priority_tag<0>)
    {
        while (i != bound) {
            ++i;
        }
    }

    template <typename I, typename S>
    static constexpr auto impl(I& i, iter_difference_t<I> n, S bound)
        -> std::enable_if_t<sized_sentinel_for<S, I>, iter_difference_t<I>>
    {
        if (fn::abs(n) >= fn::abs(bound - i)) {
            auto dist = bound - i;
            fn::impl(i, bound, priority_tag<2>{});
            return dist;
        } else {
            fn::impl(i, n);
            return n;
        }
    }

    template <typename I, typename S>
    static constexpr auto impl(I& i, iter_difference_t<I> n, S bound)
        -> std::enable_if_t<bidirectional_iterator<I> && !sized_sentinel_for<S, I>,
                            iter_difference_t<I>>
    {
        constexpr iter_difference_t<I> zero{0};
        iter_difference_t<I> counter{0};

        if (n < zero) {
            do {
                --i;
                --counter; // Yes, really
            } while (++n < zero && i != bound);
        } else {
            while (n-- > zero && i != bound) {
                ++i;
                ++counter;
            }
        }

        return counter;
    }

    template <typename I, typename S>
    static constexpr auto impl(I& i, iter_difference_t<I> n, S bound)
        -> std::enable_if_t<!bidirectional_iterator<I> && !sized_sentinel_for<S, I>,
                            iter_difference_t<I>>
    {
        constexpr iter_difference_t<I> zero{0};
        iter_difference_t<I> counter{0};

        while (n-- > zero && i != bound) {
            ++i;
            ++counter;
        }

        return counter;
    }

public:
    template <typename I>
    constexpr auto operator()(I& i, iter_difference_t<I> n) const
        -> std::enable_if_t<input_or_output_iterator<I>>
    {
        fn::impl(i, n);
    }

    template <typename I, typename S>
    constexpr auto operator()(I& i, S bound) const
        -> std::enable_if_t<input_or_output_iterator<I> && sentinel_for<S, I>>
    {
        fn::impl(i, bound, priority_tag<2>{});
    }

    template <typename I, typename S>
    constexpr auto operator()(I& i, iter_difference_t<I> n, S bound) const
        -> std::enable_if_t<input_or_output_iterator<I> && sentinel_for<S, I>, iter_difference_t<I>>
    {
        return n - fn::impl(i, n, bound);
    }
};

} // namespace advance_
} // namespace detail

NANO_INLINE_VAR(detail::advance_::fn, advance)

namespace detail {
namespace distance_ {

struct fn {
private:
    template <typename I, typename S>
    static constexpr auto impl(I i, S s)
        -> std::enable_if_t<sized_sentinel_for<S, I>, iter_difference_t<I>>
    {
        return s - i;
    }

    template <typename I, typename S>
    static constexpr auto impl(I i, S s)
        -> std::enable_if_t<!sized_sentinel_for<S, I>, iter_difference_t<I>>
    {
        iter_difference_t<I> counter{0};
        while (i != s) {
            ++i;
            ++counter;
        }
        return counter;
    }

    template <typename R>
    static constexpr auto impl(R&& r)
        -> std::enable_if_t<sized_range<R>, iter_difference_t<iterator_t<R>>>
    {
        return static_cast<iter_difference_t<iterator_t<R>>>(ranges::size(r));
    }

    template <typename R>
    static constexpr auto impl(R&& r)
        -> std::enable_if_t<!sized_range<R>, iter_difference_t<iterator_t<R>>>
    {
        return fn::impl(ranges::begin(r), ranges::end(r));
    }

public:
    template <typename I, typename S>
    constexpr auto operator()(I first, S last) const
        -> std::enable_if_t<input_or_output_iterator<I> && sentinel_for<S, I>, iter_difference_t<I>>
    {
        return fn::impl(std::move(first), std::move(last));
    }

    template <typename R>
    constexpr auto operator()(R&& r) const
        -> std::enable_if_t<range<R>, iter_difference_t<iterator_t<R>>>
    {
        return fn::impl(std::forward<R>(r));
    }
};

} // namespace distance_
} // namespace detail

NANO_INLINE_VAR(detail::distance_::fn, distance)

namespace detail {
namespace next_ {

struct fn {
    template <typename I>
    constexpr auto operator()(I x) const -> std::enable_if_t<input_or_output_iterator<I>, I>
    {
        ++x;
        return x;
    }

    template <typename I>
    constexpr auto operator()(I x, iter_difference_t<I> n) const
        -> std::enable_if_t<input_or_output_iterator<I>, I>
    {
        ranges::advance(x, n);
        return x;
    }

    template <typename I, typename S>
    constexpr auto operator()(I x, S bound) const
        -> std::enable_if_t<input_or_output_iterator<I> && sentinel_for<S, I>, I>
    {
        ranges::advance(x, bound);
        return x;
    }

    template <typename I, typename S>
    constexpr auto operator()(I x, iter_difference_t<I> n, S bound) const
        -> std::enable_if_t<input_or_output_iterator<I> && sentinel_for<S, I>, I>
    {
        ranges::advance(x, n, bound);
        return x;
    }
};

} // namespace next_
} // namespace detail

NANO_INLINE_VAR(detail::next_::fn, next)

namespace detail {
namespace prev_ {

struct fn {
    template <typename I>
    constexpr auto operator()(I x) const
        -> std::enable_if_t<bidirectional_iterator<I>, I>
    {
        --x;
        return x;
    }

    template <typename I>
    constexpr auto operator()(I x, iter_difference_t<I> n) const
        -> std::enable_if_t<bidirectional_iterator<I>, I>
    {
        ranges::advance(x, -n);
        return x;
    }

    template <typename I, typename S>
    constexpr auto operator()(I x, iter_difference_t<I> n, S bound) const
        -> std::enable_if_t<bidirectional_iterator<I> && sentinel_for<S, I>, I>
    {
        ranges::advance(x, -n, bound);
        return x;
    }
};

} // namespace prev_
} // namespace detail

NANO_INLINE_VAR(detail::prev_::fn, prev)

NANO_END_NAMESPACE

#endif
