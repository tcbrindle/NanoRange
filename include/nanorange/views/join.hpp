// nanorange/views/join.hpp
//
// Copyright (c) 2019 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_VIEWS_JOIN_HPP_INCLUDED
#define NANORANGE_VIEWS_JOIN_HPP_INCLUDED

#include <nanorange/detail/views/range_adaptors.hpp>
#include <nanorange/views/all.hpp>
#include <nanorange/views/interface.hpp>

NANO_BEGIN_NAMESPACE

namespace detail {

template <typename V, bool InnerIsReference>
struct join_view_data {
    V base_ = V();
};

template <typename V>
struct join_view_data<V, false> {
    V base_ = V();
    all_view<range_reference_t<V>> inner_ = all_view<range_reference_t<V>>();
};

template <typename B>
constexpr auto join_view_iter_cat_helper()
{
    constexpr bool ref_is_glvalue = std::is_reference_v<range_reference_t<B>>;

    if constexpr (ref_is_glvalue) {
        using OuterCat = iterator_category_t<iterator_t<B>>;
        using InnerCat = iterator_category_t<iterator_t<range_reference_t<B>>>;

        if constexpr (derived_from<OuterCat, bidirectional_iterator_tag> &&
                      derived_from<InnerCat, bidirectional_iterator_tag>) {
            return bidirectional_iterator_tag{};
        } else if constexpr(derived_from<OuterCat, forward_iterator_tag> &&
                            derived_from<InnerCat, forward_iterator_tag>) {
            return forward_iterator_tag{};
        } else {
            return input_iterator_tag{};
        }
    } else {
        return input_iterator_tag{};
    }
}

}

namespace join_view_ {

template <typename V>
struct join_view : view_interface<join_view<V>> {
private:

    static_assert(input_range<V>);
    static_assert(view<V>);
    static_assert(input_range<range_reference_t<V>>);
    static_assert(std::is_reference_v<range_reference_t<V>> ||
                  view<range_value_t<V>>);

    using InnerRng = range_reference_t<V>;

    template<bool> struct sentinel;

    template <bool Const>
    struct iterator {

        friend struct sentinel<Const>;

        template <typename B>
        static constexpr bool ref_is_glvalue =
            std::is_reference_v<range_reference_t<B>>;

        using Base = detail::conditional_t<Const, const V, V>;

        // https://github.com/ericniebler/stl2/issues/604
        using Parent = detail::conditional_t<Const && ref_is_glvalue<Base>,
            const join_view, join_view>;

        iterator_t<Base> outer_ = iterator_t<Base>();
        iterator_t<range_reference_t<Base>> inner_
            = iterator_t<range_reference_t<Base>>();
        Parent* parent_ = nullptr;

        template <typename B = Base>
        constexpr decltype(auto) update_inner(range_reference_t<Base> x)
        {
            if constexpr (ref_is_glvalue<B>) {
                return (x);
            } else {
                return (parent_->data_.inner_ = views::all(x));
            }
        }

        constexpr void satisfy()
        {
            for (; outer_ !=  ranges::end(parent_->data_.base_); ++outer_) {
                auto& inner = update_inner(*outer_);
                inner_ = ranges::begin(inner);
                if (inner_ != ranges::end(inner)) {
                    return;
                }
            }

            if constexpr (ref_is_glvalue<Base>) {
                inner_ = iterator_t<range_reference_t<Base>>();
            }
        }

    public:
        //using iterator_concept = ...
        using iterator_category = decltype(detail::join_view_iter_cat_helper<Base>());
        using value_type = range_value_t<range_reference_t<Base>>;
        using difference_type = nano::common_type_t<
            range_difference_t<Base>,
            range_difference_t<range_reference_t<Base>>>;
        // Extension: legacy typedefs
        using pointer = iterator_t<Base>;
        using reference = range_reference_t<range_reference_t<Base>>;

        iterator() = default;

        constexpr iterator(Parent& parent, iterator_t<Base> outer)
            : outer_(std::move(outer)),
              parent_(std::addressof(parent))
        {
            satisfy();
        }

        // constexpr iterator(iterator<!Const>);

        constexpr decltype(auto) operator*() const { return *inner_; }

        template <typename B = Base>
        constexpr auto operator->() const
            -> std::enable_if_t<detail::has_arrow<B>, iterator_t<Base>>
        {
            return inner_;
        }

    private:
        template <typename B = Base>
        constexpr decltype(auto) get_inner_rng()
        {
            if constexpr (ref_is_glvalue<B>) {
                return (*outer_);
            } else {
                return (parent_->data_.inner_);
            }
        }

    public:
        template <typename B = Base>
        constexpr iterator& operator++()
        {
            auto&& inner_rng = get_inner_rng();

            if (++inner_ == ranges::end(inner_rng)) {
                ++outer_;
                satisfy();
            }

            return *this;
        }

        template <typename B = Base>
        constexpr auto operator++(int)
            -> std::enable_if_t<!(ref_is_glvalue<B> &&
                                  forward_range<B> &&
                                  forward_range<range_reference_t<B>>)>
        {
            ++*this;
        }

        template <typename B = Base>
        constexpr auto operator++(int)
            -> std::enable_if_t<ref_is_glvalue<B> &&
                                forward_range<B> &&
                                forward_range<range_reference_t<B>>,
                                iterator>
        {
            auto tmp = *this;
            ++*this;
            return tmp;
        }

        template <typename B = Base>
        constexpr auto operator--()
            -> std::enable_if_t<ref_is_glvalue<B> &&
                                bidirectional_range<B> &&
                                bidirectional_range<range_reference_t<B>>,
                                iterator&>
        {
            if (outer_ == ranges::end(parent_->data_.base_)) {
                inner_ = ranges::end(*outer_);
            }

            while (inner_ == ranges::begin(*outer_)) {
                inner_ = ranges::end(*--outer_);
            }
            --inner_;
            return *this;
        }

        template <typename B = Base>
        constexpr auto operator--(int)
            -> std::enable_if_t<ref_is_glvalue<B> &&
                                bidirectional_range<B> &&
                                bidirectional_range<range_reference_t<B>>,
                                iterator>
        {
            auto tmp = *this;
            --*this;
            return *tmp;
        }

        template <typename B = Base>
        friend constexpr auto operator==(const iterator& x, const iterator& y)
            -> std::enable_if_t<ref_is_glvalue<B> &&
                                equality_comparable<iterator_t<B>> &&
                                equality_comparable<iterator_t<range_reference_t<B>>>,
                                bool>
        {
            return x.outer_ == y.outer_ && x.inner_ == y.inner_;
        }

        template <typename B = Base>
        friend constexpr auto operator!=(const iterator& x, const iterator& y)
            -> std::enable_if_t<ref_is_glvalue<B> &&
                                equality_comparable<iterator_t<B>> &&
                                equality_comparable<iterator_t<range_reference_t<B>>>,
                                bool>
        {
            return !(x == y);
        }

        friend constexpr decltype(auto) iter_move(const iterator& i)
            noexcept(noexcept(ranges::iter_move(i.inner_)))
        {
            return ranges::iter_move(i.inner_);
        }

        friend constexpr void iter_swap(const iterator& x, const iterator& y)
            noexcept(noexcept(ranges::iter_swap(x.inner_, y.inner_)))
        {
            ranges::iter_swap(x.inner_, y.inner_);
        }
    };

    template <bool Const>
    struct sentinel {
    private:
        using Parent = detail::conditional_t<Const, const join_view, join_view>;
        using Base = detail::conditional_t<Const, const V, V>;

        sentinel_t<Base> end_ = sentinel_t<Base>();

    public:
        sentinel() = default;

        constexpr explicit sentinel(Parent& parent)
            : end_(ranges::end(parent.data_.base_))
        {}

        // constexpr sentinel(sentinel<!Const> s);

        friend constexpr bool operator==(const iterator<Const>& x, const sentinel& y)
        {
            return x.outer_ == y.end_;
        }

        friend constexpr bool operator!=(const iterator<Const>& x, const sentinel& y)
        {
            return !(x == y);
        }

        friend constexpr bool operator==(const sentinel& x, const iterator<Const>& y)
        {
            return y == x;
        }

        friend constexpr bool operator!=(const sentinel& x, const iterator<Const>& y)
        {
            return !(y == x);
        }
    };

    detail::join_view_data<V, std::is_reference_v<InnerRng>> data_{};

public:

    join_view() = default;

    constexpr explicit join_view(V base)
        : data_{std::move(base)}
    {}

    template <typename R,
              std::enable_if_t<detail::not_same_as<R, join_view>, int> = 0,
              std::enable_if_t<input_range<R>, int> = 0,
              std::enable_if_t<viewable_range<R>, int> = 0,
              std::enable_if_t<constructible_from<V, all_view<R>>, int> = 0>
    constexpr join_view(R&& r)
        : data_{views::all(std::forward<R>(r))}
    {}

    constexpr auto begin()
    {
        return iterator<detail::simple_view<V>>{*this, ranges::begin(data_.base_)};
    }

    template <typename VV = V,
              std::enable_if_t<input_range<const VV> &&
                               std::is_reference_v<range_reference_t<const VV>>, int> = 0>
    constexpr auto begin() const
    {
        return iterator<true>{*this, ranges::begin(data_.base_)};
    }

    constexpr auto end()
    {
        if constexpr (forward_range<V> &&
                      std::is_reference_v<InnerRng> && forward_range<InnerRng> &&
                      common_range<V> && common_range<InnerRng>) {
            return iterator<detail::simple_view<V>>{*this, ranges::end(data_.base_)};
        } else {
            return sentinel<detail::simple_view<V>>{*this};
        }
    }

    template <typename VV = V,
              std::enable_if_t<input_range<const VV> &&
                               std::is_reference_v<range_reference_t<const VV>>, int> = 0>
    constexpr auto end() const
    {
        if constexpr (forward_range<const V> &&
                      std::is_reference_v<range_reference_t<const V>> &&
                      forward_range<range_reference_t<const V>> &&
                      common_range<const V> &&
                      common_range<range_reference_t<const V>>) {
            return iterator<true>{*this, ranges::end(data_.base_)};
        } else {
            return sentinel<true>{*this};
        }
    }
};

template <typename R>
explicit join_view(R&&) -> join_view<all_view<R>>;

} // namespace join_view_

using join_view_::join_view;

namespace detail {

struct join_view_fn {

    template <typename E>
    constexpr auto operator()(E&& e) const
        -> decltype(join_view{std::forward<E>(e)})
    {
        return join_view{std::forward<E>(e)};
    }

};

template <>
inline constexpr bool is_raco<join_view_fn> = true;

}

namespace views {

NANO_INLINE_VAR(nano::detail::join_view_fn, join)

}

NANO_END_NAMESPACE

#endif
