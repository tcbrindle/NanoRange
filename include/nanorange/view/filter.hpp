// nanorange/view/filter.hpp
//
// Copyright (c) 2019 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_VIEW_FILTER_HPP_INCLUDED
#define NANORANGE_VIEW_FILTER_HPP_INCLUDED

#include <nanorange/algorithm/find.hpp>
#include <nanorange/detail/view/semiregular_box.hpp>
#include <nanorange/view/all.hpp>

#include <cassert>

NANO_BEGIN_NAMESPACE

namespace detail {

template <typename V>
constexpr auto filter_view_iter_cat_helper()
{
    using C = iterator_category_t<iterator_t<V>>;
    if constexpr (DerivedFrom<C, bidirectional_iterator_tag>) {
        return bidirectional_iterator_tag{};
    } else if constexpr (DerivedFrom<C, forward_iterator_tag>) {
        return forward_iterator_tag{};
    } else {
        return input_iterator_tag{};
    }
}


}

namespace filter_view_ {

template <typename V, typename Pred>
struct filter_view : view_interface<filter_view<V, Pred>> {

    static_assert(InputRange<V>);
    static_assert(IndirectUnaryPredicate<Pred, iterator_t<V>>);
    static_assert(View<V>);
    static_assert(std::is_object_v<Pred>);

private:
    V base_ = V();
    detail::semiregular_box<Pred> pred_;

    struct iterator {
    private:
        iterator_t<V> current_ = iterator_t<V>();
        filter_view* parent_ = nullptr;

    public:
        // using iterator_concept = ...
        using iterator_category =
            decltype(detail::filter_view_iter_cat_helper<V>());
        using value_type = iter_value_t<iterator_t<V>>;
        using difference_type = iter_difference_t<iterator_t<V>>;

        iterator() = default;
        constexpr iterator(filter_view& parent, iterator_t<V> current)
            : current_(std::move(current)), parent_(std::addressof(parent))
        {}

        constexpr iterator_t<V> base() const { return current_; }

        constexpr iter_reference_t<iterator_t<V>> operator*() const
        {
            return *current_;
        }

        template <typename VV = V>
        constexpr auto operator->() const
            -> std::enable_if_t<detail::HasArrow<iterator_t<VV>>, iterator_t<V>>
        {
            return current_;
        }

        constexpr iterator& operator++()
        {
            current_ = ranges::find_if(++current_,
                                       ranges::end(parent_->base_),
                                       std::ref(*parent_->pred_));
            return *this;
        }

        constexpr auto operator++(int)
        {
            if constexpr (ForwardRange<V>) {
                auto tmp = *this;
                ++*this;
                return tmp;
            } else {
                ++*this;
            }
        }

        template <typename VV = V>
        constexpr auto operator--()
            -> std::enable_if_t<BidirectionalRange<VV>, iterator&>
        {
            do {
                --current_;
            } while (!nano::invoke(*parent_->pred_, *current_));
            return *this;
        }

        template <typename VV = V>
        constexpr auto operator--(int)
            -> std::enable_if_t<BidirectionalRange<VV>, iterator>
        {
            auto tmp = *this;
            --*this;
            return tmp;
        }

        friend constexpr auto operator==(const iterator& x, const iterator& y)
            -> std::enable_if_t<EqualityComparable<iterator_t<V>>, bool>
        {
            return x.current_ == y.current_;
        }

        friend constexpr auto operator!=(const iterator& x, const iterator& y)
            -> std::enable_if_t<EqualityComparable<iterator_t<V>>, bool>
        {
            return !(x == y);
        }

        friend constexpr iter_rvalue_reference_t<iterator_t<V>>
        iter_move(const iterator& i) noexcept(
            noexcept(ranges::iter_move(i.current_)))
        {
            return ranges::iter_move(i.current_);
        }

        template <typename VV = V>
        friend constexpr auto
        iter_swap(const iterator& x, const iterator& y) noexcept(
            noexcept(ranges::iter_swap(x.current_, y.current_)))
            -> std::enable_if_t<IndirectlySwappable<iterator_t<VV>>>
        {
            ranges::iter_swap(x.current_, y.current_);
        }
    };

    struct sentinel {
    private:
        sentinel_t<V> end_ = sentinel_t<V>();

    public:
        sentinel() = default;

        constexpr explicit sentinel(filter_view& parent)
            : end_(ranges::end(parent.base_))
        {}

        constexpr sentinel_t<V> base() const { return end_; }

        friend constexpr bool operator==(const iterator& i, const sentinel& s)
        {
            return i.current_ = s.end_;
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
    };

    std::optional<iterator> begin_cache_{};

public:
    filter_view() = default;

    constexpr filter_view(V base, Pred pred)
        : base_(std::move(base)), pred_(std::move(pred))
    {}

    template <typename R,
              std::enable_if_t<InputRange<R> && Constructible<V, all_view<R>>,
                               int> = 0>
    constexpr filter_view(R&& r, Pred pred)
        : base_(view::all(std::forward<R>(r))), pred_(std::move(pred))
    {}

    constexpr V base() const { return base_; }

    constexpr iterator begin()
    {
        if (begin_cache_) {
            return *begin_cache_;
        }

        assert(pred_.has_value());
        begin_cache_ = iterator{*this, nano::find_if(base_, std::ref(*pred_))};
        return *begin_cache_;
    }

    constexpr auto end()
    {
        if constexpr (CommonRange<V>) {
            return iterator{*this, ranges::end(base_)};
        } else {
            return sentinel{*this};
        }
    }
};

template <typename R, typename Pred>
filter_view(R&&, Pred)->filter_view<all_view<R>, Pred>;

}

using filter_view_::filter_view;

namespace detail {

struct filter_fn {

    template <typename R, typename Pred>
    constexpr auto operator()(R&& r, Pred pred) const
        noexcept(noexcept(filter_view{std::forward<R>(r), std::move(pred)}))
        -> decltype(filter_view{std::forward<R>(r), std::move(pred)})
    {
        return filter_view{std::forward<R>(r), std::move(pred)};
    }

};

}

namespace view {

NANO_INLINE_VAR(nano::detail::filter_fn, filter)

}

NANO_END_NAMESPACE

#endif