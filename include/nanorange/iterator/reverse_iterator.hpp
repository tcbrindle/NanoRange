// nanorange/iterator/reverse_iterator.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_ITERATOR_REVERSE_ITERATOR_HPP_INCLUDED
#define NANORANGE_ITERATOR_REVERSE_ITERATOR_HPP_INCLUDED

#include <nanorange/detail/iterator/algorithm_requirements.hpp>
#include <nanorange/detail/iterator/concepts.hpp>
#include <nanorange/detail/iterator/iter_move.hpp>
#include <nanorange/detail/iterator/iter_swap.hpp>
#include <nanorange/iterator/operations.hpp>

NANO_BEGIN_NAMESPACE

namespace reverse_iterator_ {

template <typename I>
class reverse_iterator {

    static_assert(BidirectionalIterator<I>,
                  "Template argument to reverse_iterator must model "
                  "BidirectionalIterator");

public:
    using iterator_type = I;
    using difference_type = difference_type_t<I>;
    using value_type = value_type_t<I>;
    using iterator_category = detail::legacy_iterator_category_t<I>;
    using reference = reference_t<I>;
    using pointer = I;

    constexpr reverse_iterator() = default;

    explicit constexpr reverse_iterator(I x) : current_(std::move(x)) {}

    template <typename U, std::enable_if_t<ConvertibleTo<U, I>, int> = 0>

    constexpr reverse_iterator(const reverse_iterator<U>& i)
        : current_(i.base())
    {}

    template <typename U, std::enable_if_t<ConvertibleTo<U, I>, int> = 0>

    constexpr reverse_iterator& operator=(const reverse_iterator<U>& i)
    {
        current_ = i.base();
        return *this;
    }

    constexpr I base() const { return current_; }

    constexpr reference operator*() const { return *prev(current_); }

    constexpr pointer operator->() const { return prev(current_); }

    constexpr reverse_iterator& operator++()
    {
        --current_;
        return *this;
    }

    constexpr reverse_iterator operator++(int)
    {
        reverse_iterator tmp = *this;
        --current_;
        return tmp;
    }

    constexpr reverse_iterator& operator--()
    {
        ++current_;
        return *this;
    }

    constexpr reverse_iterator operator--(int)
    {
        reverse_iterator tmp = *this;
        ++current_;
        return tmp;
    }

    template <typename II = I>
    constexpr std::enable_if_t<RandomAccessIterator<II>, reverse_iterator>
    operator+(difference_type n) const
    {
        return reverse_iterator(current_ - n);
    }

    template <typename II = I>
    constexpr std::enable_if_t<RandomAccessIterator<II>, reverse_iterator&>
    operator+=(difference_type n)
    {
        current_ -= n;
        return *this;
    }

    template <typename II = I>
    constexpr std::enable_if_t<RandomAccessIterator<II>, reverse_iterator>
    operator-(difference_type n) const
    {
        return reverse_iterator(current_ + n);
    }

    template <typename II = I>
    constexpr std::enable_if_t<RandomAccessIterator<II>, reverse_iterator&>
    operator-=(difference_type n)
    {
        current_ += n;
        return *this;
    }

    template <typename II = I>
    constexpr std::enable_if_t<RandomAccessIterator<II>, reference>
    operator[](difference_type n) const
    {
        return current_[-n - 1];
    }

    // FIXME: Why doesn't GCC like rvalue_reference_t<I> return?
    friend constexpr decltype(auto) //rvalue_reference_t<I>
    iter_move(const reverse_iterator& i) noexcept(
        noexcept(ranges::iter_move(std::declval<I&>())) &&
        noexcept(--std::declval<I&>()) &&
        std::is_nothrow_copy_constructible<I>::value)
    {
        return ranges::iter_move(prev(i.current_));
    }

    template <typename I2>
    friend constexpr auto
    iter_swap(const reverse_iterator& x,
              const reverse_iterator<I2>&
                  y) noexcept(noexcept(ranges::iter_swap(std::declval<I>(),
                                                         std::declval<I>())) &&
                              noexcept(--std::declval<I&>()))
        -> std::enable_if_t<IndirectlySwappable<I2, I>>

    {
        ranges::iter_swap(prev(x.current_), prev(y.base()));
    }

private:
    I current_{};
};

template <typename I1, typename I2>
constexpr std::enable_if_t<EqualityComparableWith<I1, I2>, bool>
operator==(const reverse_iterator<I1>& x, const reverse_iterator<I2>& y)
{
    return x.base() == y.base();
}

template <typename I1, typename I2>
constexpr std::enable_if_t<EqualityComparableWith<I1, I2>, bool>
operator!=(const reverse_iterator<I1>& x, const reverse_iterator<I2>& y)
{
    return x.base() != y.base();
}

template <typename I1, typename I2>
constexpr std::enable_if_t<StrictTotallyOrderedWith<I1, I2>, bool>
operator<(const reverse_iterator<I1>& x, const reverse_iterator<I2>& y)
{
    return x.base() > y.base();
}

template <typename I1, typename I2>
constexpr std::enable_if_t<StrictTotallyOrderedWith<I1, I2>, bool>
operator>(const reverse_iterator<I1>& x, const reverse_iterator<I2>& y)
{
    return x.base() < y.base();
}

template <typename I1, typename I2>
constexpr std::enable_if_t<StrictTotallyOrderedWith<I1, I2>, bool>
operator>=(const reverse_iterator<I1>& x, const reverse_iterator<I2>& y)
{
    return x.base() <= y.base();
}

template <typename I1, typename I2>
constexpr std::enable_if_t<StrictTotallyOrderedWith<I1, I2>, bool>
operator<=(const reverse_iterator<I1>& x, const reverse_iterator<I2>& y)
{
    return x.base() >= y.base();
}

template <typename I1, typename I2>
constexpr std::enable_if_t<SizedSentinel<I1, I2>, difference_type_t<I2>>
operator-(const reverse_iterator<I1>& x, const reverse_iterator<I2>& y)
{
    return y.base() - x.base();
}

template <typename I>
constexpr std::enable_if_t<RandomAccessIterator<I>, reverse_iterator<I>>
operator+(difference_type_t<I> n, const reverse_iterator<I>& x)
{
    return reverse_iterator<I>(x.base() - n);
}

} // namespace reverse_iterator_

using reverse_iterator_::reverse_iterator;

template <typename I>
constexpr std::enable_if_t<BidirectionalIterator<I>, reverse_iterator<I>>
make_reverse_iterator(I i)
{
    return reverse_iterator<I>(std::move(i));
}

NANO_END_NAMESPACE

#endif
