// nanorange/views/iota.hpp
//
// Copyright (c) 2019 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_VIEWS_IOTA_HPP_INCLUDED
#define NANORANGE_VIEWS_IOTA_HPP_INCLUDED

#include <nanorange/detail/iterator/concepts.hpp>
#include <nanorange/iterator/unreachable.hpp>
#include <nanorange/views/interface.hpp>

NANO_BEGIN_NAMESPACE

namespace detail {

struct Decrementable_req {
    template <typename I>
    auto requires_(I i) -> decltype(
        same_lv<I>(--i),
        requires_expr<same_as<I, decltype(i--)>>{}
    );
};

template <typename I>
NANO_CONCEPT Decrementable =
    Incrementable<I> && requires_<Decrementable_req, I>;

struct Advanceable_req {
    // FIXME: Nasty IOTA-DIFF-T stuff
    template <typename I, typename DiffT = iter_difference_t<I>>
    auto requires_(I i, const I j, const DiffT n) -> decltype(
        same_lv<I>(i += n),
        same_lv<I>(i -= n),
        I(j + n),
        I(n + j),
        I(j - n),
        convertible_to_helper<DiffT>(j - j)
    );
};

template <typename I>
NANO_CONCEPT Advanceable = Decrementable<I> &&
    StrictTotallyOrdered<I> &&
    requires_<Advanceable_req, I>;

template <typename W>
constexpr auto iota_view_iter_cat_helper()
{
    if constexpr (detail::Advanceable<W>) {
        return random_access_iterator_tag{};
    } else if constexpr (detail::Decrementable<W>) {
        return bidirectional_iterator_tag{};
    } else if constexpr (Incrementable<W>) {
        return forward_iterator_tag{};
    } else {
        return input_iterator_tag{};
    }
}

} // namespace detail

template <typename W, typename Bound = unreachable_sentinel_t>
struct iota_view : view_interface<iota_view<W, Bound>> {
    static_assert(WeaklyIncrementable<W>);
    static_assert(Semiregular<Bound>);
    static_assert(detail::WeaklyEqualityComparableWith<W, Bound>);

private:
    struct sentinel;

    struct iterator {
    private:
        friend struct sentinel;
        W value_ = W();

    public:
        using iterator_category = decltype(detail::iota_view_iter_cat_helper<W>());
        
        using value_type = W;
        // FIXME: IOTA_DIFF_T urgh
        using difference_type = iter_difference_t<W>;

        iterator() = default;

        constexpr explicit iterator(W value) : value_(value) {}

        constexpr W operator*() const
            noexcept(std::is_nothrow_copy_constructible_v<W>)
        {
            return value_;
        }

        constexpr iterator& operator++()
        {
            ++value_;
            return *this;
        }

        constexpr auto operator++(int)
        {
            if constexpr (Incrementable<W>) {
                auto tmp = *this;
                ++*this;
                return tmp;
            } else {
                ++*this;
            }
        }

        template <typename WW = W>
        constexpr auto operator--()
            -> std::enable_if_t<detail::Decrementable<WW>, iterator&>
        {
            --value_;
            return *this;
        }

        constexpr iterator operator--(int)
        {
            auto tmp = *this;
            ++*this;
            return tmp;
        }

        template <typename WW = W>
        constexpr auto operator+=(difference_type n)
            -> std::enable_if_t<detail::Advanceable<WW>, iterator&>
        {
            if constexpr (integral<W> && !signed_integral<W>) {
                if (n >= difference_type(0)) {
                    value_ += static_cast<W>(n);
                } else {
                    value_ -= static_cast<W>(-n);
                }
            } else {
                value_ += n;
            }
            return *this;
        }

        template <typename WW = W>
        constexpr auto operator-=(difference_type n)
            -> std::enable_if_t<detail::Advanceable<WW>, iterator&>
        {
            if constexpr (integral<W> && !signed_integral<W>) {
                if (n >= difference_type(0)) {
                    value_ -= static_cast<W>(n);
                } else {
                    value_ += static_cast<W>(-n);
                }
            } else {
                value_ -= n;
            }
            return *this;
        }

        template <typename WW = W>
        constexpr auto operator[](difference_type n) const
            -> std::enable_if_t<detail::Advanceable<WW>, W>
        {
            return W(value_ + n);
        }

        template <typename WW = W>
        friend constexpr auto operator==(const iterator& x, const iterator& y)
            -> std::enable_if_t<EqualityComparable<WW>, bool>
        {
            return x.value_ == y.value_;
        }

        template <typename WW = W>
        friend constexpr auto operator!=(const iterator& x, const iterator& y)
            -> std::enable_if_t<EqualityComparable<WW>, bool>
        {
            return !(x == y);
        }

        template <typename WW = W>
        friend constexpr auto operator<(const iterator& x, const iterator& y)
            -> std::enable_if_t<StrictTotallyOrdered<WW>, bool>
        {
            return x.value_ < y.value_;
        }

        template <typename WW = W>
        friend constexpr auto operator>(const iterator& x, const iterator& y)
            -> std::enable_if_t<StrictTotallyOrdered<WW>, bool>
        {
            return y < x;
        }

        template <typename WW = W>
        friend constexpr auto operator<=(const iterator& x, const iterator& y)
            -> std::enable_if_t<StrictTotallyOrdered<WW>, bool>
        {
            return !(y < x);
        }

        template <typename WW = W>
        friend constexpr auto operator>=(const iterator& x, const iterator& y)
            -> std::enable_if_t<StrictTotallyOrdered<WW>, bool>
        {
            return !(x < y);
        }

        template <typename WW = W>
        friend constexpr auto operator+(iterator i, difference_type n)
            -> std::enable_if_t<detail::Advanceable<WW>, iterator>
        {
            return i += n;
        }

        template <typename WW = W>
        friend constexpr auto operator+(difference_type n, iterator i)
            -> std::enable_if_t<detail::Advanceable<WW>, iterator>
        {
            return i + n;
        }

        template <typename WW = W>
        friend constexpr auto operator-(iterator i, difference_type n)
            -> std::enable_if_t<detail::Advanceable<WW>, iterator>
        {
            return i -= n;
        }

        template <typename WW = W>
        friend constexpr auto operator-(const iterator& x, const iterator& y)
            -> std::enable_if_t<detail::Advanceable<WW>, difference_type>
        {
            using D = difference_type;
            if constexpr (integral<D>) {
                if constexpr (signed_integral<D>) {
                    return D(D(x.value_) - D(y.value_));
                } else {
                    return (y.value_ > x.value)
                        ? D(-D(y.value_ - x.value_))
                        : D(x.value_ - y.value_);
                }
            } else {
                return x.value_ - y.value_;
            }
        }
    };

    struct sentinel {
    private:
        Bound bound_ = Bound();

    public:
        sentinel() = default;
        constexpr explicit sentinel(Bound bound) : bound_(bound) {}

        friend constexpr bool operator==(const iterator& i, const sentinel& s)
        {
            return i.value_ == s.bound_;
        }

        friend constexpr bool operator==(const sentinel& s, const iterator& i)
        {
            return i == s;
        }

        friend constexpr bool operator!=(const iterator& i, const sentinel& s)
        {
            return !(i == s);
        }

        friend constexpr bool operator!=(const sentinel& s, const iterator& i)
        {
            return !(i == s);
        }

        template <typename WW = W>
        friend constexpr auto operator-(const iterator& i, const sentinel& s)
            -> std::enable_if_t<SizedSentinel<Bound, WW>, iter_difference_t<WW>>
        {
            return i.value_ - s.bound_;
        }

        template <typename WW = W>
        friend constexpr auto operator-(const sentinel& s, const iterator& i)
            -> std::enable_if_t<SizedSentinel<Bound, WW>, iter_difference_t<WW>>
        {
            return -(i - s);
        }
    };

    W value_ = W();
    Bound bound_ = Bound();

public:
    iota_view() = default;

    constexpr explicit iota_view(W value) : value_(value) {}

    constexpr iota_view(detail::type_identity_t<W> value,
                        detail::type_identity_t<Bound> bound)
        : value_(value),
          bound_(bound)
    {}

    constexpr iterator begin() const
    {
        return iterator{value_};
    }

    template <typename WW = W, std::enable_if_t<!same_as<WW, Bound>, int> = 0>
    constexpr auto end() const
    {
        if constexpr (same_as<Bound, unreachable_sentinel_t>) {
            return unreachable_sentinel;
        } else {
            return sentinel{bound_};
        }
    }

    template <typename WW = W, std::enable_if_t<same_as<WW, Bound>, int> = 0>
    constexpr iterator end() const
    {
        return iterator{bound_};
    }

    template <typename WW = W, typename BB = Bound, std::enable_if_t<
              (same_as<WW, BB> && detail::Advanceable<W>) ||
              (integral<WW> && integral<BB>) ||
              SizedSentinel<BB, WW>, int> = 0>
    constexpr auto size() const
    {
        constexpr auto make_unsigned_like = [](auto i) {
            return std::make_unsigned_t<decltype(i)>(i);
        };

        if constexpr (integral<W> && integral<Bound>) {
            return (value_ < 0)
                ? ((bound_ < 0)
                  ? make_unsigned_like(-value_) - make_unsigned_like(-bound_)
                  : make_unsigned_like(bound_) - make_unsigned_like(-value_))
                : make_unsigned_like(bound_) - make_unsigned_like(value_);
        } else {
            make_unsigned_like(bound_ - value_);
        }
    }
};

template <typename W, typename Bound, std::enable_if_t<
    !integral<W> || !integral<Bound> ||
        (signed_integral<W> == signed_integral<Bound>), int> = 0>
iota_view(W, Bound) -> iota_view<W, Bound>;

namespace views {

namespace detail {

struct iota_view_fn {
    template <typename W>
    constexpr auto operator()(W&& value) const
        noexcept(noexcept(iota_view{std::forward<W>(value)}))
        -> decltype(iota_view(std::forward<W>(value)))
    {
        return iota_view(std::forward<W>(value));
    }

    template <typename W, typename Bound>
    constexpr auto operator()(W&& value, Bound&& bound) const
        noexcept(noexcept(iota_view{std::forward<W>(value), std::forward<Bound>(bound)}))
        -> decltype(iota_view(std::forward<W>(value), std::forward<Bound>(bound)))
    {
        return iota_view{std::forward<W>(value), std::forward<Bound>(bound)};
    }
};

} // namespace detail

NANO_INLINE_VAR(detail::iota_view_fn, iota)

}

NANO_END_NAMESPACE

#endif
