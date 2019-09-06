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

struct decrementable_concept {
    template <typename I>
    auto requires_(I i) -> decltype(
        requires_expr<same_as<decltype(--i), I&>>{},
        requires_expr<same_as<decltype(i--), I>>{}
    );
};

template <typename I>
NANO_CONCEPT decrementable = incrementable<I> && requires_<decrementable_concept, I>;

template <typename W>
using iota_diff_t = iter_difference_t<W>;

struct advanceable_concept {
    template <typename I>
    auto requires_(I i, const I j, const iota_diff_t<I> n) -> decltype(
        requires_expr<same_as<decltype(i += n), I&>>{},
        requires_expr<same_as<decltype(i -= n), I&>>{},
        I(j + n),
        I(n + j),
        I(j - n),
        requires_expr<convertible_to<decltype(j - j), iota_diff_t<I>>>{}
    );
};

template <typename I>
NANO_CONCEPT advanceable =
    decrementable<I> && totally_ordered<I> &&
    requires_<advanceable_concept, I>;

template <typename W>
constexpr auto iota_view_iter_cat_helper()
{
    if constexpr (detail::advanceable<W>) {
        return random_access_iterator_tag{};
    } else if constexpr (detail::decrementable<W>) {
        return bidirectional_iterator_tag{};
    } else if constexpr (incrementable<W>) {
        return forward_iterator_tag{};
    } else {
        return input_iterator_tag{};
    }
}

} // namespace detail

template <typename W, typename Bound = unreachable_sentinel_t>
struct iota_view : view_interface<iota_view<W, Bound>> {
    static_assert(weakly_incrementable<W>);
    static_assert(semiregular<Bound>);
    static_assert(detail::weakly_equality_comparable_with<W, Bound>);

private:
    struct sentinel;

    struct iterator {
    private:
        friend struct sentinel;
        W value_ = W();

    public:
        using iterator_category = decltype(detail::iota_view_iter_cat_helper<W>());
        using value_type = W;
        using difference_type = detail::iota_diff_t<W>;
        // Extension: legacy typedefs
        using pointer = void;
        using reference = W;

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
            if constexpr (incrementable<W>) {
                auto tmp = *this;
                ++*this;
                return tmp;
            } else {
                ++*this;
            }
        }

        template <typename WW = W>
        constexpr auto operator--()
            -> std::enable_if_t<detail::decrementable<WW>, iterator&>
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
            -> std::enable_if_t<detail::advanceable<WW>, iterator&>
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
            -> std::enable_if_t<detail::advanceable<WW>, iterator&>
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
            -> std::enable_if_t<detail::advanceable<WW>, W>
        {
            return W(value_ + n);
        }

        template <typename WW = W>
        friend constexpr auto operator==(const iterator& x, const iterator& y)
            -> std::enable_if_t<equality_comparable<WW>, bool>
        {
            return x.value_ == y.value_;
        }

        template <typename WW = W>
        friend constexpr auto operator!=(const iterator& x, const iterator& y)
            -> std::enable_if_t<equality_comparable<WW>, bool>
        {
            return !(x == y);
        }

        template <typename WW = W>
        friend constexpr auto operator<(const iterator& x, const iterator& y)
            -> std::enable_if_t<totally_ordered<WW>, bool>
        {
            return x.value_ < y.value_;
        }

        template <typename WW = W>
        friend constexpr auto operator>(const iterator& x, const iterator& y)
            -> std::enable_if_t<totally_ordered<WW>, bool>
        {
            return y < x;
        }

        template <typename WW = W>
        friend constexpr auto operator<=(const iterator& x, const iterator& y)
            -> std::enable_if_t<totally_ordered<WW>, bool>
        {
            return !(y < x);
        }

        template <typename WW = W>
        friend constexpr auto operator>=(const iterator& x, const iterator& y)
            -> std::enable_if_t<totally_ordered<WW>, bool>
        {
            return !(x < y);
        }

        template <typename WW = W>
        friend constexpr auto operator+(iterator i, difference_type n)
            -> std::enable_if_t<detail::advanceable<WW>, iterator>
        {
            return i += n;
        }

        template <typename WW = W>
        friend constexpr auto operator+(difference_type n, iterator i)
            -> std::enable_if_t<detail::advanceable<WW>, iterator>
        {
            return i + n;
        }

        template <typename WW = W>
        friend constexpr auto operator-(iterator i, difference_type n)
            -> std::enable_if_t<detail::advanceable<WW>, iterator>
        {
            return i -= n;
        }

        template <typename WW = W>
        friend constexpr auto operator-(const iterator& x, const iterator& y)
            -> std::enable_if_t<detail::advanceable<WW>, difference_type>
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
            -> std::enable_if_t<sized_sentinel_for<Bound, WW>, iter_difference_t<WW>>
        {
            return i.value_ - s.bound_;
        }

        template <typename WW = W>
        friend constexpr auto operator-(const sentinel& s, const iterator& i)
            -> std::enable_if_t<sized_sentinel_for<Bound, WW>, iter_difference_t<WW>>
        {
            return -(i - s);
        }
    };

    W value_ = W();
    Bound bound_ = Bound();

public:
    iota_view() = default;

    constexpr explicit iota_view(W value) : value_(value) {}

    constexpr iota_view(type_identity_t<W> value, type_identity_t<Bound> bound)
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
              (same_as<WW, BB> && detail::advanceable<W>) ||
              (integral<WW> && integral<BB>) ||
                                   sized_sentinel_for<BB, WW>, int> = 0>
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
