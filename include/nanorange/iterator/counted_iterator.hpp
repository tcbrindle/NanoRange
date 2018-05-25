// nanorange/iterator/counted_iterator.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_ITERATOR_COUNTED_ITERATOR_HPP_INCLUDED
#define NANORANGE_ITERATOR_COUNTED_ITERATOR_HPP_INCLUDED

#include <nanorange/detail/iterator/concepts.hpp>
#include <nanorange/iterator/default_sentinel.hpp>

NANO_BEGIN_NAMESPACE

namespace counted_iterator_ {

template <typename I>
class counted_iterator {
    static_assert(Iterator<I>, "");

    template <typename I2>
    friend class counted_iterator;

public:
    using iterator = I;
    using difference_type = difference_type_t<I>;

    constexpr counted_iterator() = default;

    constexpr counted_iterator(I x, difference_type_t<I> n)
        : current_(x), cnt_(n)
    {}

    template <typename I2, std::enable_if_t<ConvertibleTo<I2, I>, int> = 0>
    constexpr counted_iterator(const counted_iterator<I2>& i)
        : current_(i.current_), cnt_(i.cnt_)
    {}

    template <typename I2>
    constexpr auto operator=(const counted_iterator<I2>& i)
        -> std::enable_if_t<ConvertibleTo<I2, I>, counted_iterator&>
    {
        current_ = i.current_;
        cnt_ = i.cnt_;
        return *this;
    }

    constexpr I base() const { return current_; }

    constexpr difference_type_t<I> count() const { return cnt_; }

    constexpr decltype(auto) operator*() { return *current_; }

    template <typename II = I,
              std::enable_if_t<detail::Dereferenceable<const II>, int> = 0>
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

    template <typename II = I, std::enable_if_t<!ForwardIterator<II>, int> = 0>
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
    operator++(int) -> std::enable_if_t<ForwardIterator<II>, counted_iterator>
    {
        auto tmp = *this;
        ++*this;
        return tmp;
    }

    template <typename II = I>
    constexpr auto operator--()
        -> std::enable_if_t<BidirectionalIterator<II>, counted_iterator&>
    {
        --current_;
        ++cnt_;
        return *this;
    }

    template <typename II = I>
    constexpr auto operator--(int)
        -> std::enable_if_t<BidirectionalIterator<II>, counted_iterator>
    {
        auto tmp = *this;
        --*this;
        return tmp;
    }

    template <typename II = I>
    constexpr auto operator+(difference_type n) const
        -> std::enable_if_t<RandomAccessIterator<II>, counted_iterator>
    {
        return counted_iterator(current_ + n, cnt_ - n);
    }

    template <typename II = I>
    constexpr auto operator+=(difference_type n)
        -> std::enable_if_t<RandomAccessIterator<II>, counted_iterator&>
    {
        current_ += n;
        cnt_ -= n;
        return *this;
    }

    template <typename II = I>
    constexpr auto operator-(difference_type n) const
        -> std::enable_if_t<RandomAccessIterator<II>, counted_iterator>
    {
        return counted_iterator(current_ - n, cnt_ + n);
    }

    template <typename II = I>
    constexpr auto operator-=(difference_type n)
        -> std::enable_if_t<RandomAccessIterator<II>, counted_iterator&>
    {
        current_ -= n;
        cnt_ += n;
        return *this;
    }

    template <typename II = I,
              std::enable_if_t<RandomAccessIterator<II>, int> = 0>
    constexpr decltype(auto) operator[](difference_type n) const
    {
        return current_[n];
    }

    template <typename II = I, std::enable_if_t<InputIterator<II>, int> = 0>
    friend constexpr rvalue_reference_t<I>
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
        -> std::enable_if_t<IndirectlySwappable<I2, I>>
    {
        ranges::iter_swap(x.current_, y.current_);
    }

private:
    I current_{};
    difference_type_t<I> cnt_{0};
};

template <typename I1, typename I2>
constexpr auto operator==(const counted_iterator<I1>& x,
                          const counted_iterator<I2>& y)
    -> std::enable_if_t<Common<I1, I2>, bool>
{
    return x.count() == y.count();
}

template <typename I>
constexpr bool operator==(const counted_iterator<I>& x, default_sentinel)
{
    return x.count() == 0;
}

template <typename I>
constexpr bool operator==(default_sentinel, const counted_iterator<I>& x)
{
    return x.count() == 0;
}

template <typename I1, typename I2>
constexpr auto operator!=(const counted_iterator<I1>& x,
                          const counted_iterator<I2>& y)
    -> std::enable_if_t<Common<I1, I2>, bool>
{
    return !(x == y);
}

template <typename I>
constexpr bool operator!=(const counted_iterator<I>& x, default_sentinel y)
{
    return !(x == y);
}

template <typename I>
constexpr bool operator!=(default_sentinel x, const counted_iterator<I>& y)
{
    return !(x == y);
}

template <typename I1, typename I2>
constexpr auto operator<(const counted_iterator<I1>& x,
                         const counted_iterator<I2>& y)
    -> std::enable_if_t<Common<I1, I2>, bool>
{
    return y.count() < x.count();
}

template <typename I1, typename I2>
constexpr auto operator<=(const counted_iterator<I1>& x,
                          const counted_iterator<I2>& y)
    -> std::enable_if_t<Common<I1, I2>, bool>
{
    return !(y < x);
}

template <typename I1, typename I2>
constexpr auto operator>(const counted_iterator<I1>& x,
                         const counted_iterator<I2>& y)
    -> std::enable_if_t<Common<I1, I2>, bool>
{
    return y < x;
}

template <typename I1, typename I2>
constexpr auto operator>=(const counted_iterator<I1>& x,
                          const counted_iterator<I2>& y)
    -> std::enable_if_t<Common<I1, I2>, bool>
{
    return !(x < y);
}

template <typename I1, typename I2>
constexpr auto operator-(const counted_iterator<I1>& x,
                         const counted_iterator<I2>& y)
    -> std::enable_if_t<Common<I1, I2>, difference_type_t<I2>>
{
    return y.count() - x.count();
}

template <typename I>
constexpr difference_type_t<I> operator-(const counted_iterator<I>& x,
                                         default_sentinel)
{
    return -x.count();
}

template <typename I>
constexpr difference_type_t<I> operator-(default_sentinel,
                                         const counted_iterator<I>& y)
{
    return y.count();
}

template <typename I>
constexpr auto operator+(difference_type_t<I> n, const counted_iterator<I>& x)
    -> std::enable_if_t<RandomAccessIterator<I>, counted_iterator<I>>
{
    return x + n;
}

}

using counted_iterator_::counted_iterator;

namespace detail {

template <typename I, typename = void>
struct counted_iterator_value_type_helper {
};

template <typename I>
struct counted_iterator_value_type_helper<I, std::enable_if_t<Readable<I>>> {
    using type = value_type_t<I>;
};

template <typename I, typename = void>
struct counted_iterator_category_helper {
};

template <typename I>
struct counted_iterator_category_helper<I, std::enable_if_t<InputIterator<I>>> {
    using type = iterator_category_t<I>;
};

} // namespace detail

template <typename I>
struct value_type<counted_iterator<I>>
        : detail::counted_iterator_value_type_helper<I> {
};

template <typename I>
struct iterator_category<counted_iterator<I>>
        : detail::counted_iterator_category_helper<I> {
};

template <typename I>
constexpr auto make_counted_iterator(I i, difference_type_t<I> n)
    -> std::enable_if_t<Iterator<I>, counted_iterator<I>>
{
    return counted_iterator<I>(std::move(i), n);
}

NANO_END_NAMESPACE

#endif
