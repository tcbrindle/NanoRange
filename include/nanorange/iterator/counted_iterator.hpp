// nanorange/iterator/counted_iterator.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_ITERATOR_COUNTED_ITERATOR_HPP_INCLUDED
#define NANORANGE_ITERATOR_COUNTED_ITERATOR_HPP_INCLUDED

#include <nanorange/detail/iterator/algorithm_requirements.hpp>
#include <nanorange/detail/iterator/concepts.hpp>
#include <nanorange/detail/iterator/iter_move.hpp>
#include <nanorange/detail/iterator/iter_swap.hpp>
#include <nanorange/iterator/default_sentinel.hpp>

NANO_BEGIN_NAMESPACE

namespace counted_iterator_ {

template <typename I>
class counted_iterator {
    static_assert(input_or_output_iterator<I>, "");

    template <typename I2>
    friend class counted_iterator;

public:
    using iterator = I;
    using difference_type = iter_difference_t<I>;

    constexpr counted_iterator() = default;

    constexpr counted_iterator(I x, iter_difference_t<I> n)
        : current_(x), cnt_(n)
    {}

    template <typename I2, std::enable_if_t<convertible_to<I2, I>, int> = 0>
    constexpr counted_iterator(const counted_iterator<I2>& i)
        : current_(i.current_), cnt_(i.cnt_)
    {}

    template <typename I2>
    constexpr auto operator=(const counted_iterator<I2>& i)
        -> std::enable_if_t<convertible_to<I2, I>, counted_iterator&>
    {
        current_ = i.current_;
        cnt_ = i.cnt_;
        return *this;
    }

    constexpr I base() const { return current_; }

    constexpr iter_difference_t<I> count() const { return cnt_; }

    constexpr decltype(auto) operator*() { return *current_; }

    template <typename II = I,
              std::enable_if_t<detail::dereferenceable<const II>, int> = 0>
    constexpr decltype(auto) operator*() const
    {
        return *current_;
    }

    constexpr counted_iterator& operator++()
    {
        ++current_;
        --cnt_;
        return *this;
    }

    template <typename II = I, std::enable_if_t<!forward_iterator<II>, int> = 0>
    decltype(auto) operator++(int)
    {
        --cnt_;
        try {
            return current_++;
        } catch (...) {
            ++cnt_;
            throw;
        }
    }

    template <typename II = I>
    constexpr auto
    operator++(int) -> std::enable_if_t<forward_iterator<II>, counted_iterator>
    {
        auto tmp = *this;
        ++*this;
        return tmp;
    }

    template <typename II = I>
    constexpr auto operator--()
        -> std::enable_if_t<bidirectional_iterator<II>, counted_iterator&>
    {
        --current_;
        ++cnt_;
        return *this;
    }

    template <typename II = I>
    constexpr auto operator--(int)
        -> std::enable_if_t<bidirectional_iterator<II>, counted_iterator>
    {
        auto tmp = *this;
        --*this;
        return tmp;
    }

    template <typename II = I>
    constexpr auto operator+(difference_type n) const
        -> std::enable_if_t<random_access_iterator<II>, counted_iterator>
    {
        return counted_iterator(current_ + n, cnt_ - n);
    }

    template <typename II = I, std::enable_if_t<random_access_iterator<II>, int> = 0>
    friend constexpr counted_iterator operator+(iter_difference_t<II> n,
                                                const counted_iterator<II>& x)
    {
        return x + n;
    }

    template <typename II = I>
    constexpr auto operator+=(difference_type n)
        -> std::enable_if_t<random_access_iterator<II>, counted_iterator&>
    {
        current_ += n;
        cnt_ -= n;
        return *this;
    }

    template <typename II = I>
    constexpr auto operator-(difference_type n) const
        -> std::enable_if_t<random_access_iterator<II>, counted_iterator>
    {
        return counted_iterator(current_ - n, cnt_ + n);
    }

    template <typename II = I,
              std::enable_if_t<random_access_iterator<II>, int> = 0>
    constexpr decltype(auto) operator[](difference_type n) const
    {
        return current_[n];
    }

    template <typename I2>
    friend constexpr auto operator==(const counted_iterator& x,
                                     const counted_iterator<I2>& y)
        -> std::enable_if_t<common_with<I2, I>, bool>
    {
        return x.count() == y.count();
    }

    friend constexpr bool operator==(const counted_iterator& x, default_sentinel_t)
    {
        return x.count() == 0;
    }

    friend constexpr bool operator==(default_sentinel_t, const counted_iterator& x)
    {
        return x.count() == 0;
    }

    template <typename I2>
    friend constexpr auto operator!=(const counted_iterator& x,
                                     const counted_iterator<I2>& y)
        -> std::enable_if_t<common_with<I2, I>, bool>
    {
        return !(x == y);
    }

    friend constexpr bool operator!=(const counted_iterator& x, default_sentinel_t y)
    {
        return !(x == y);
    }

    friend constexpr bool operator!=(default_sentinel_t x, const counted_iterator& y)
    {
        return !(x == y);
    }

    template <typename I2>
    friend constexpr auto operator<(const counted_iterator& x,
                                    const counted_iterator<I2>& y)
        -> std::enable_if_t<common_with<I2, I>, bool>
    {
        return y.count() < x.count();
    }

    template <typename I2>
    friend constexpr auto operator>(const counted_iterator& x,
                                    const counted_iterator<I2>& y)
        -> std::enable_if_t<common_with<I2, I>, bool>
    {
        return y < x;
    }

    template <typename I2>
    friend constexpr auto operator<=(const counted_iterator& x,
                                     const counted_iterator<I2>& y)
         -> std::enable_if_t<common_with<I2, I>, bool>
    {
        return !(y < x);
    }

    template <typename I2>
    friend constexpr auto operator>=(const counted_iterator& x,
                                     const counted_iterator<I2>& y)
        -> std::enable_if_t<common_with<I2, I>, bool>
    {
        return !(x < y);
    }

    template <typename I2>
    friend constexpr auto operator-(const counted_iterator& x,
                                    const counted_iterator<I2>& y)
        -> std::enable_if_t<common_with<I2, I>, iter_difference_t<I2>>
    {
        return y.count() - x.count();
    }

    friend constexpr iter_difference_t<I>
    operator-(const counted_iterator& x, default_sentinel_t)
    {
        return -x.cnt_;
    }

    friend constexpr iter_difference_t<I>
    operator-(default_sentinel_t, const counted_iterator& y)
    {
        return y.cnt_;
    }

    template <typename II = I>
    constexpr auto operator-=(difference_type n)
        -> std::enable_if_t<random_access_iterator<II>, counted_iterator&>
    {
        current_ -= n;
        cnt_ += n;
        return *this;
    }

#ifndef _MSC_VER
    // FIXME MSVC: If this is a template, MSVC can't find it via ADL for some reason
    // Making it a non-template doesn't lose much other than the InputIterator guard
    template <typename II = I, std::enable_if_t<input_iterator<II>, int> = 0>
#endif
    friend constexpr iter_rvalue_reference_t<I>
    iter_move(const counted_iterator& i) noexcept(
        noexcept(ranges::iter_move(i.current_)))
    {
        return ranges::iter_move(i.current_);
    }

    template <typename I2>
    friend constexpr auto iter_swap(
        const counted_iterator<I>& x,
        const counted_iterator<I2>&
            y) noexcept(noexcept(ranges::iter_swap(x.current_, y.current_)))
        -> std::enable_if_t<indirectly_swappable<I2, I>>
    {
        ranges::iter_swap(x.current_, y.current_);
    }

private:
    I current_{};
    iter_difference_t<I> cnt_{0};
};

}

using counted_iterator_::counted_iterator;

namespace detail {

template <typename I, typename = void>
struct counted_iterator_readable_traits_helper {
};

template <typename I>
struct counted_iterator_readable_traits_helper<I, std::enable_if_t<readable<I>>> {
    using value_type = iter_value_t<I>;
};

template <typename I, typename = void>
struct counted_iterator_category_helper {
};

template <typename I>
struct counted_iterator_category_helper<I, std::enable_if_t<input_iterator<I>>> {
    using type = iterator_category_t<I>;
};

} // namespace detail

template <typename I>
struct readable_traits<counted_iterator<I>>
        : detail::counted_iterator_readable_traits_helper<I> {
};

template <typename I>
struct iterator_category<counted_iterator<I>>
        : detail::counted_iterator_category_helper<I> {
};

template <typename I>
constexpr auto make_counted_iterator(I i, iter_difference_t<I> n)
    -> std::enable_if_t<input_or_output_iterator<I>, counted_iterator<I>>
{
    return counted_iterator<I>(std::move(i), n);
}

NANO_END_NAMESPACE

#endif
