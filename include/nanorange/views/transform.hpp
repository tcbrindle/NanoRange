// nanorange/views/transform.hpp
//
// Copyright (c) 2019 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_VIEWS_TRANSFORM_HPP_INCLUDED
#define NANORANGE_VIEWS_TRANSFORM_HPP_INCLUDED

#include <nanorange/detail/views/range_adaptors.hpp>
#include <nanorange/detail/views/semiregular_box.hpp>
#include <nanorange/views/all.hpp>
#include <nanorange/views/interface.hpp>

NANO_BEGIN_NAMESPACE

namespace transform_view_ {

template <typename V, typename F>
struct transform_view : view_interface<transform_view<V, F>>
{
private:
    // FIXME: Weird GCC9 bug strikes again!
    // These two conditions are equivalent to input_range<V>, but if we test
    // that directly with GCC9 then it fails?!
    static_assert(range<V>);
    static_assert(input_iterator<iterator_t<V>>);
    static_assert(view<V>);
    static_assert(copy_constructible<F>);
    static_assert(std::is_object_v<F>);
    static_assert(regular_invocable<F&, range_reference_t<V>>);

    template <bool> struct sentinel;

    template <bool Const>
    struct iterator {
    private:
        friend struct iterator<!Const>;
        friend struct sentinel<Const>;

        using Parent =
            detail::conditional_t<Const, const transform_view, transform_view>;
        using Base = detail::conditional_t<Const, const V, V>;

        iterator_t<Base> current_ = iterator_t<Base>();
        Parent* parent_ = nullptr;

        static constexpr bool iter_move_noexcept_helper =
            noexcept(nano::invoke(*parent_->fun_, *current_));

    public:
        using iterator_category = detail::conditional_t<
            derived_from<iterator_category_t<iterator_t<Base>>, contiguous_iterator_tag>,
            random_access_iterator_tag,
            iterator_category_t<iterator_t<Base>>>;

        using value_type =
            remove_cvref_t<invoke_result_t<F&, range_reference_t<Base>>>;
        using difference_type = range_difference_t<Base>;
        // Extension: legacy typedefs
        using pointer = void;
        using reference = invoke_result_t<F&, range_reference_t<Base>>;

        iterator() = default;

        constexpr iterator(Parent& parent, iterator_t<Base> current)
            : current_(std::move(current)),
              parent_(std::addressof(parent))
        {}

        template <typename I,
            std::enable_if_t<same_as<I, iterator<!Const>>, int> = 0,
            bool C = Const, typename VV = V, std::enable_if_t<
            C && convertible_to<iterator_t<VV>, iterator_t<Base>>, int> = 0>
        constexpr iterator(I i)
            : current_(std::move(i.current_)),
              parent_(i.parent_)
        {}

        constexpr iterator_t<Base> base() const { return current_; }

        constexpr decltype(auto) operator*() const
        {
            return nano::invoke(*parent_->fun_, *current_);
        }

        constexpr iterator& operator++() { ++current_; return *this; }

        constexpr auto operator++(int)
        {
            if constexpr (forward_range<Base>) {
                auto tmp = *this;
                ++*this;
                return tmp;
            } else {
                ++current_;
            }
        }

        template <typename B = Base>
        constexpr auto operator--()
            -> std::enable_if_t<bidirectional_range<B>, iterator&>
        {
            --current_;
            return *this;
        }

        template <typename B = Base>
        constexpr auto operator--(int)
            -> std::enable_if_t<bidirectional_range<B>, iterator>
        {
            auto tmp = *this;
            --*this;
            return tmp;
        }

        template <typename B = Base>
        constexpr auto operator+=(difference_type n)
            -> std::enable_if_t<random_access_range<B>, iterator&>
        {
            current_ += n;
            return *this;
        }

        template <typename B = Base>
        constexpr auto operator-=(difference_type n)
            -> std::enable_if_t<random_access_range<B>, iterator&>
        {
            current_ -= n;
            return *this;
        }

        template <typename B = Base, typename = std::enable_if_t<random_access_range<B>>>
        constexpr decltype(auto) operator[](difference_type n) const
        {
            return nano::invoke(*parent_->fun_, current_[n]);
        }

        template <typename B = Base>
        friend constexpr auto operator==(const iterator& x, const iterator& y)
            -> std::enable_if_t<equality_comparable<iterator_t<B>>, bool>
        {
            return x.current_ == y.current_;
        }

        template <typename B = Base>
        friend constexpr auto operator!=(const iterator& x, const iterator& y)
            -> std::enable_if_t<equality_comparable<iterator_t<B>>, bool>
        {
            return !(x == y);
        }

        template <typename B = Base>
        friend constexpr auto operator<(const iterator& x, const iterator& y)
            -> std::enable_if_t<random_access_range<B>, bool>
        {
            return x.current_ < y.current_;
        }

        template <typename B = Base>
        friend constexpr auto operator>(const iterator& x, const iterator& y)
            -> std::enable_if_t<random_access_range<B>, bool>
        {
            return y < x;
        }

        template <typename B = Base>
        friend constexpr auto operator<=(const iterator& x, const iterator& y)
            -> std::enable_if_t<random_access_range<B>, bool>
        {
            return !(y < x);
        }

        template <typename B = Base>
        friend constexpr auto operator>=(const iterator& x, const iterator& y)
            -> std::enable_if_t<random_access_range<B>, bool>
        {
            return !(x < y);
        }

        template <typename B = Base>
        friend constexpr auto operator+(iterator i, difference_type n)
            -> std::enable_if_t<random_access_range<B>, iterator>
        {
            return iterator{*i.parent_, i.current_ + n};
        }

        template <typename B = Base>
        friend constexpr auto operator+(difference_type n, iterator i)
            -> std::enable_if_t<random_access_range<B>, iterator>
        {
            return iterator{*i.parent_, i.current_ + n};
        }

        template <typename B = Base>
        friend constexpr auto operator-(iterator i, difference_type n)
            -> std::enable_if_t<random_access_range<B>, iterator>
        {
            return iterator{*i.parent_, i.current_ - n};
        }

        template <typename B = Base>
        friend constexpr auto operator-(const iterator& x, const iterator& y)
            -> std::enable_if_t<random_access_range<B>, difference_type>
        {
            return x.current_ - y.current_;
        }

        friend constexpr decltype(auto) iter_move(const iterator& i)
            noexcept(iter_move_noexcept_helper)
        {
            if constexpr (std::is_lvalue_reference_v<decltype(*i)>) {
                return std::move(*i);
            } else {
                return *i;
            }
        }

        template <typename B = Base>
        friend constexpr auto iter_swap(const iterator& x, const iterator& y)
            noexcept(noexcept(ranges::iter_swap(x.current_, y.current_)))
            -> std::enable_if_t<indirectly_swappable<iterator_t<B>>>
        {
            return ranges::iter_swap(x.current_, y.current_);
        }
    };

    template <bool Const>
    struct sentinel
    {
    private:
        friend struct sentinel<!Const>;

        using Parent =
            detail::conditional_t<Const, const transform_view, transform_view>;
        using Base = detail::conditional_t<Const, const V, V>;
        sentinel_t<Base> end_ = sentinel_t<Base>();

    public:
        sentinel() = default;

        constexpr explicit sentinel(sentinel_t<Base> end)
            : end_(std::move(end))
        {}

        template <typename S,
            std::enable_if_t<same_as<S, sentinel<!Const>>, int> = 0,
            bool C = Const, typename VV = V, std::enable_if_t<
            C && convertible_to<sentinel_t<VV>, sentinel_t<Base>>, int> = 0>
        constexpr sentinel(S i)
            : end_(std::move(i.end_))
        {}

        constexpr sentinel_t<Base> base() { return end_; }

        friend constexpr bool operator==(const iterator<Const>& x, const sentinel& y)
        {
            return x.base() == y.end_;
        }

        friend constexpr bool operator==(const sentinel& x, const iterator<Const>& y)
        {
            return x.end_ == y.base();
        }

        friend constexpr bool operator!=(const iterator<Const>& x, const sentinel& y)
        {
            return !(x == y);
        }

        friend constexpr bool operator!=(const sentinel& x, const iterator<Const>& y)
        {
            return !(x == y);
        }

        template <typename B = Base>
        friend constexpr auto operator-(const iterator<Const>& x, const sentinel& y)
            -> std::enable_if_t<sized_sentinel_for<sentinel_t<B>, iterator_t<B>>,
                                range_difference_t<B>>
        {
            return x.current_ - y.end_;
        }

        template <typename B = Base>
        friend constexpr auto operator-(const sentinel& x, const iterator<Const>& y)
            -> std::enable_if_t<sized_sentinel_for<sentinel_t<B>, iterator_t<B>>,
                                range_difference_t<B>>
        {
            x.end_ - y.current_;
        }
    };

    V base_ = V();
    detail::semiregular_box<F> fun_;

public:
    transform_view() = default;

    constexpr transform_view(V base, F fun)
        : base_(std::move(base)),
          fun_(std::move(fun))
    {}

    template <typename R, std::enable_if_t<
        input_range<R> &&
        viewable_range<R> &&
        constructible_from<V, all_view<R>>, int> = 0>
    constexpr transform_view(R&& r, F fun)
        : base_(views::all(std::forward<R>(r))),
          fun_(std::move(fun))
    {}

    constexpr V base() const { return base_; }

    constexpr iterator<false> begin()
    {
        return iterator<false>{*this, ranges::begin(base_)};
    }

    template <typename VV = V, std::enable_if_t<
        range<const VV> &&
        regular_invocable<const F&, range_reference_t<const VV>>, int> = 0>
    constexpr iterator<true> begin() const
    {
        return iterator<true>{*this, ranges::begin(base_)};
    }

    constexpr auto end()
    {
        if constexpr (common_range<V>) {
            return iterator<false>{*this, ranges::end(base_)};
        } else {
            return sentinel<false>{ranges::end(base_)};
        }
    }

    template <typename VV = V, std::enable_if_t<
        range<const VV> &&
        regular_invocable<const F&, range_reference_t<const VV>>, int> = 0>
    constexpr auto end() const
    {
        if constexpr (common_range<V>) {
            return iterator<true>{*this, ranges::end(base_)};
        } else {
            return sentinel<true>{ranges::end(base_)};
        }
    }

    template <typename VV = V, std::enable_if_t<sized_range<VV>, int> = 0>
    constexpr auto size() { return ranges::size(base_); }

    template <typename VV = V, std::enable_if_t<sized_range<const VV>, int> = 0>
    constexpr auto size() const { return ranges::size(base_); }

};

template <typename R, typename F, std::enable_if_t<
    input_range<all_view<R>> &&
    copy_constructible<F> &&
    std::is_object_v<F> &&
    regular_invocable<F&, range_reference_t<all_view<R>>>, int> = 0>
transform_view(R&&, F) -> transform_view<all_view<R>, F>;

} // namespace transform_view_

using transform_view_::transform_view;

namespace detail {

struct transform_view_fn {
    template <typename E, typename F>
    constexpr auto operator()(E&& e, F&& f) const
        -> decltype(transform_view{std::forward<E>(e), std::forward<F>(f)})
    {
        return transform_view{std::forward<E>(e), std::forward<F>(f)};
    }

    template <typename F>
    constexpr auto operator()(F f) const
    {
        return detail::rao_proxy{[f = std::move(f)](auto&& r) mutable
#ifndef NANO_MSVC_LAMBDA_PIPE_WORKAROUND
            -> decltype(transform_view{std::forward<decltype(r)>(r), std::declval<F&&>()})
#endif
        {
            return transform_view{std::forward<decltype(r)>(r), std::move(f)};
        }};
    }
};

}

namespace views {

NANO_INLINE_VAR(nano::detail::transform_view_fn, transform)

}

NANO_END_NAMESPACE

#endif
