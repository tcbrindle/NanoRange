// nanorange/views/elements.hpp
//
// Copyright (c) 2019 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_VIEWS_ELEMENTS_HPP_INCLUDED
#define NANORANGE_VIEWS_ELEMENTS_HPP_INCLUDED

#include <nanorange/detail/views/range_adaptors.hpp>
#include <nanorange/views/all.hpp>
#include <nanorange/views/interface.hpp>

NANO_BEGIN_NAMESPACE

namespace detail {

struct has_tuple_element_concept {
    template <typename T, typename I,
              std::size_t N = I::value,
              typename = typename std::tuple_size<T>::type>
    auto requires_(T t) -> decltype(
        requires_expr<(N < std::tuple_size_v<T>)>{},
        std::declval<std::tuple_element_t<N, T>>(),
        requires_expr<convertible_to<decltype(std::get<N>(t)), std::tuple_element_t<N, T>>>{}
    );
};

template <typename T, std::size_t N>
NANO_CONCEPT has_tuple_element =
    detail::requires_<has_tuple_element_concept, T,
                      std::integral_constant<std::size_t, N>>;

} // namespace detail

template <typename R, std::size_t N>
struct elements_view : view_interface<elements_view<R, N>> {

    static_assert(input_range<R>);
    static_assert(view<R>);
    static_assert(detail::has_tuple_element<range_value_t<R>, N>);
    static_assert(detail::has_tuple_element<
        std::remove_reference_t<range_reference_t<R>>, N>);

    elements_view() = default;

    constexpr explicit elements_view(R base)
        : base_(std::move(base))
    {}

    template <typename RR = R, std::enable_if_t<!detail::simple_view<RR>, int> = 0>
    constexpr auto begin()
    {
        return iterator<false>(ranges::begin(base_));
    }

    template <typename RR = R, std::enable_if_t<detail::simple_view<RR>, int> = 0>
    constexpr auto begin() const
    {
        return iterator<true>(ranges::begin(base_));
    }

    template <typename RR = R, std::enable_if_t<!detail::simple_view<RR>, int> = 0>
    constexpr auto end()
    {
        return ranges::end(base_);
    }

    template <typename RR = R, std::enable_if_t<detail::simple_view<RR>, int> = 0>
    constexpr auto end() const
    {
        return ranges::end(base_);
    }

    template <typename RR = R, std::enable_if_t<sized_range<RR>, int> = 0>
    constexpr auto size()
    {
        return ranges::size(base_);
    }

    template <typename RR = R, std::enable_if_t<sized_range<const RR>, int> = 0>
    constexpr auto size() const
    {
        return ranges::size(base_);
    }

private:
    template <bool Const>
    struct iterator
    {
    private:
        using base_t = detail::conditional_t<Const, const R, R>;
        friend iterator<!Const>;

        iterator_t<base_t> current_;

    public:
        using iterator_category = iterator_category_t<iterator_t<base_t>>;
        using value_type =
            remove_cvref_t<std::tuple_element_t<N, range_value_t<base_t>>>;
        using difference_type = range_difference_t<base_t>;

        iterator() = default;

        constexpr explicit iterator(iterator_t<base_t> current)
            : current_(std::move(current))
        {}

        template <typename I,
                  std::enable_if_t<same_as<I, iterator<!Const>>, int> = 0,
                  bool C = Const, typename B = base_t,
                  std::enable_if_t<C &&
                      convertible_to<iterator_t<R>, iterator_t<B>>, int> = 0>
        constexpr iterator(I i)
            : current_(std::move(i.current_))
        {}

        constexpr iterator_t<base_t> base() const { return current_; }

        constexpr decltype(auto) operator*() const
        {
            return std::get<N>(*current_);
        }

        constexpr iterator& operator++() { ++current_; return *this; }

        constexpr auto operator++(int)
        {
            if constexpr (forward_range<base_t>) {
                auto temp = *this;
                ++*this;
                return temp;
            } else {
                ++*this;
            }
        }

        template <typename B = base_t>
        constexpr auto operator--()
            -> std::enable_if_t<bidirectional_range<B>, iterator&>
        {
            --current_;
            return *this;
        }

        template <typename B = base_t>
        constexpr auto operator--(int)
            -> std::enable_if_t<bidirectional_range<B>, iterator>
        {
            auto temp = *this;
            ++*this;
            return temp;
        }

        template <typename B = base_t>
        constexpr auto operator+=(difference_type x)
            -> std::enable_if_t<random_access_range<B>, iterator&>
        {
            current_ += x;
            return *this;
        }

        template <typename B = base_t>
        constexpr auto operator-=(difference_type x)
            -> std::enable_if_t<random_access_range<B>, iterator&>
        {
            current_ -= x;
            return *this;
        }

        template <typename B = base_t,
                  std::enable_if_t<random_access_range<B>, int> = 0>
        constexpr decltype(auto) operator[](difference_type n) const
        {
            return std::get<N>(*(current_ + n));
        }

        template <typename B = base_t>
        friend constexpr auto operator==(const iterator& x, const iterator& y)
            -> std::enable_if_t<equality_comparable<iterator_t<B>>, bool>
        {
            return x.current_ == y.current_;
        }

        template <typename B = base_t>
        friend constexpr auto operator!=(const iterator& x, const iterator& y)
            -> std::enable_if_t<equality_comparable<iterator_t<B>>, bool>
        {
            return !(x == y);
        }

        // Make these friend functions templates to keep MSVC happy
#if (defined(_MSC_VER) && _MSC_VER < 1922)
        template <typename = void>
#endif
        friend constexpr bool operator==(const iterator& x, const sentinel_t<base_t>& y)
        {
            return x.current_ == y;
        }

#if (defined(_MSC_VER) && _MSC_VER < 1922)
        template <typename = void>
#endif
        friend constexpr bool operator==(const sentinel_t<base_t>& y, const iterator& x)
        {
            return x.current_ == y;
        }

#if (defined(_MSC_VER) && _MSC_VER < 1922)
        template <typename = void>
#endif
        friend constexpr bool operator!=(const iterator& x, const sentinel_t<base_t>& y)
        {
            return !(x == y);
        }

#if (defined(_MSC_VER) && _MSC_VER < 1922)
        template <typename = void>
#endif
        friend constexpr bool operator!=(const sentinel_t<base_t>& y, const iterator& x)
        {
            return !(x == y);
        }

        template <typename B = base_t>
        friend constexpr auto operator<(const iterator& x, const iterator& y)
            -> std::enable_if_t<random_access_range<B>, bool>
        {
            return x.current_ < y.current_;
        }

        template <typename B = base_t>
        friend constexpr auto operator>(const iterator& x, const iterator& y)
            -> std::enable_if_t<random_access_range<B>, bool>
        {
            return (y < x);
        }

        template <typename B = base_t>
        friend constexpr auto operator<=(const iterator& x, const iterator& y)
            -> std::enable_if_t<random_access_range<B>, bool>
        {
            return !(y < x);
        }

        template <typename B = base_t>
        friend constexpr auto operator>=(const iterator& x, const iterator& y)
            -> std::enable_if_t<random_access_range<B>, bool>
        {
            return !(x < y);
        }

        template <typename B = base_t>
        friend constexpr auto operator+(const iterator& x, difference_type y)
            -> std::enable_if_t<random_access_range<B>, iterator>
        {
            return iterator{x} += y;
        }

        template <typename B = base_t>
        friend constexpr auto operator+(difference_type x, const iterator& y)
            -> std::enable_if_t<random_access_range<B>, iterator>
        {
            return y + x;
        }

        template <typename B = base_t>
        friend constexpr auto operator-(const iterator& x, difference_type y)
            -> std::enable_if_t<random_access_range<B>, iterator>
        {
            return iterator{x} -= y;
        }


        template <typename B = base_t>
        friend constexpr auto operator-(const iterator& x, const iterator& y)
            -> std::enable_if_t<random_access_range<B>, difference_type>
        {
            return x.current_ - y.current_;
        }

        template <typename B = base_t>
        friend constexpr auto operator-(const iterator& x, const sentinel_t<base_t>& y)
        -> std::enable_if_t<sized_sentinel_for<sentinel_t<B>, iterator_t<B>>,
            difference_type>
        {
            return x.current_ - y;
        }

        template <typename B = base_t>
        friend constexpr auto operator-(const sentinel_t<base_t>& x, const iterator& y)
            -> std::enable_if_t<sized_sentinel_for<sentinel_t<B>, iterator_t<B>>,
                                difference_type>
        {
            return -(y - x);
        }
    };

    R base_ = R();
};


template <typename R>
using keys_view = elements_view<all_view<R>, 0>;

template <typename R>
using values_view = elements_view<all_view<R>, 1>;

namespace detail {

template <std::size_t N>
struct elements_view_fn {
    template <typename E>
    constexpr auto operator()(E&& e) const
        -> decltype(elements_view<all_view<decltype(std::forward<E>(e))>, N>{std::forward<E>(e)})
    {
        return elements_view<all_view<decltype(std::forward<E>(e))>, N>{std::forward<E>(e)};
    }
};

template <std::size_t N>
inline constexpr bool is_raco<elements_view_fn<N>> = true;

} // namespace detail

namespace views {

inline namespace function_objects {

template <std::size_t N>
inline constexpr nano::detail::elements_view_fn<N> elements{};

inline constexpr nano::detail::elements_view_fn<0> keys{};

inline constexpr nano::detail::elements_view_fn<1> values{};

}

}


NANO_END_NAMESPACE

#endif
