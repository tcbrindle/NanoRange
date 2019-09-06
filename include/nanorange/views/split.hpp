// nanorange/views/split.hpp
//
// Copyright (c) 2019 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_VIEWS_SPLIT_HPP_INCLUDED
#define NANORANGE_VIEWS_SPLIT_HPP_INCLUDED

#include <nanorange/algorithm/mismatch.hpp>
#include <nanorange/detail/views/range_adaptors.hpp>
#include <nanorange/iterator/default_sentinel.hpp>
#include <nanorange/views/all.hpp>
#include <nanorange/views/interface.hpp>
#include <nanorange/views/single.hpp>

NANO_BEGIN_NAMESPACE

namespace detail {

template <auto> struct require_constant;

struct tiny_range_concept {

    template <typename R>
    auto requires_() -> decltype(
        std::declval<require_constant<std::remove_reference_t<R>::size()>>()
    );

    template <typename R>
    static auto test(long) -> std::false_type;

    template <typename R>
    static auto test(int) -> std::enable_if_t<
        sized_range<R> &&
        detail::requires_<tiny_range_concept, R> &&
        (std::remove_reference_t<R>::size() <= 1),
        std::true_type>;
};

template <typename R>
NANO_CONCEPT tiny_range = decltype(tiny_range_concept::test<R>(0))::value;

template <typename V, typename P, bool NotForwardRange>
struct split_view_data {
    V base_ = V();
    P pattern_ = P();
};

template <typename V, typename P>
struct split_view_data<V, P, true> {
    V base_ = V();
    P pattern_ = P();
    iterator_t<V> current_ = iterator_t<V>();
};

} // namespace detail

namespace split_view_ {

template <typename V, typename Pattern>
struct split_view : view_interface<split_view<V, Pattern>> {
private:

    static_assert(input_range<V>);
    static_assert(forward_range<Pattern>);
    static_assert(view<V>);
    static_assert(view<Pattern>);
    static_assert(indirectly_comparable<iterator_t<V>, iterator_t<Pattern>,
                                        ranges::equal_to>);
    static_assert(forward_range<V> || detail::tiny_range<Pattern>);

    detail::split_view_data<V, Pattern, !forward_range<V>> data_{};



    template <bool>
    struct inner_iterator;

    template <bool Const>
    struct outer_iterator {
    private:
        friend struct outer_iterator<!Const>;
        friend struct inner_iterator<Const>;

        using Parent = detail::conditional_t<Const, const split_view, split_view>;
        using Base = detail::conditional_t<Const, const V, V>;
        Parent* parent_ = nullptr;
        iterator_t<Base> current_ = iterator_t<Base>();

        constexpr decltype(auto) get_current()
        {
            if constexpr (forward_range<V>) {
                return (current_);
            } else {
                return (parent_->data_.current_);
            }
        }

        constexpr decltype(auto) get_current() const
        {
            if constexpr (forward_range<V>) {
                return (current_);
            } else {
                return (parent_->data_.current_);
            }
        }

        constexpr bool done() const
        {
            return get_current() == ranges::end(parent_->data_.base_);
        }

    public:
        // FIXME: iterator_concept
        using iterator_category = detail::conditional_t<
            forward_range<Base>, forward_iterator_tag, input_iterator_tag>;

        struct value_type {
        private:
            outer_iterator i_ = outer_iterator();

        public:
            value_type() = default;

            constexpr explicit value_type(outer_iterator i)
                : i_(std::move(i))
            {}

            constexpr inner_iterator<Const> begin() const
            {
                return inner_iterator<Const>{i_};
            }

            constexpr default_sentinel_t end() const
            {
                return default_sentinel;
            }
        };

        using difference_type = range_difference_t<Base>;
        // Extension: legacy typedefs
        using pointer = void;
        using reference = value_type;

        outer_iterator() = default;

        template <typename B = Base, std::enable_if_t<!forward_range<B>, int> = 0>
        constexpr explicit outer_iterator(Parent& parent)
            : parent_(std::addressof(parent))
        {}

        template <typename B = Base, std::enable_if_t<forward_range<B>, int> = 0>
        constexpr outer_iterator(Parent& parent, iterator_t<Base> current)
            : parent_(std::addressof(parent)),
              current_(std::move(current))
        {}

        template <typename I,
                  std::enable_if_t<same_as<I, outer_iterator<!Const>>, int> = 0,
                  bool C = Const, typename VV = V,
                  std::enable_if_t<C &&
                      convertible_to<iterator_t<VV>, iterator_t<const VV>>, int> = 0>
        constexpr outer_iterator(I i)
            : parent_(i.parent_),
              current_(std::move(i.current_))
        {}

        constexpr value_type operator*() const { return value_type{*this}; }

        constexpr outer_iterator& operator++()
        {
            const auto end = ranges::end(parent_->data_.base_);
            if (get_current() == end) {
                return *this;
            }
            const auto [pbegin, pend] = subrange{parent_->data_.pattern_};
            if (pbegin == pend) {
                ++get_current();
            } else {
                do {
                    const auto [b, p] = ranges::mismatch(get_current(), end,
                                                         pbegin, pend);
                    if (p == pend) {
                        get_current() = b;
                        break;
                    }
                } while (++get_current() != end);
            }
            return *this;
        }

        constexpr decltype(auto) operator++(int)
        {
            if constexpr (forward_range<Base>) {
                auto tmp = *this;
                ++*this;
                return tmp;
            } else {
                ++*this;
            }
        }

        template <typename B = Base>
        friend constexpr auto operator==(const outer_iterator& x, const outer_iterator& y)
            -> std::enable_if_t<forward_range<B>, bool>
        {
            return x.current_ == y.current_;
        }

        template <typename B = Base>
        friend constexpr auto operator!=(const outer_iterator& x, const outer_iterator& y)
            -> std::enable_if_t<forward_range<B>, bool>
        {
            return !(x == y);
        }

        friend constexpr bool operator==(const outer_iterator& x, default_sentinel_t)
        {
            return x.done();
        }

        friend constexpr bool operator==(default_sentinel_t d, const outer_iterator& x)
        {
            return x == d;
        }

        friend constexpr bool operator!=(const outer_iterator& x, default_sentinel_t d)
        {
            return !(x == d);
        }

        friend constexpr bool operator!=(default_sentinel_t d, const outer_iterator& x)
        {
            return !(x == d);
        }
    };

    template <bool Const>
    struct inner_iterator {
    private:
        using Base = detail::conditional_t<Const, const V, V>;
        static constexpr bool NoReallyGccConst = Const;
        outer_iterator<Const> i_ = outer_iterator<NoReallyGccConst>();
        bool incremented_ = false;

        constexpr bool done() const
        {
            auto cur = i_.get_current();
            auto end = ranges::end(i_.parent_->data_.base_);
            if (cur == end) {
                return true;
            }
            auto [pcur, pend] = subrange{i_.parent_->data_.pattern_};
            if (pcur == pend) {
                return incremented_;
            }
            do {
                if (*cur != *pcur) {
                    return false;
                }
                if (++pcur == pend) {
                    return true;
                }
            } while (++cur != end);
            return false;
        }

        constexpr decltype(auto) get_outer_current() const { return i_.get_current(); }

    public:
        using iterator_category = detail::conditional_t<
            derived_from<iterator_category_t<iterator_t<Base>>, forward_iterator_tag>,
                forward_iterator_tag,
                input_iterator_tag>;
        using value_type = range_value_t<Base>;
        using difference_type = range_difference_t<Base>;

        inner_iterator() = default;

        constexpr explicit inner_iterator(outer_iterator<Const> i)
            : i_(std::move(i))
        {}

        constexpr decltype(auto) operator*() const { return *i_.get_current(); }

        constexpr inner_iterator& operator++()
        {
            incremented_ = true;
            if constexpr (!forward_range<Base>) {
                if constexpr (Pattern::size() == 0) {
                    return *this;
                }
            }
            ++i_.get_current();
            return *this;
        }


        constexpr decltype(auto) operator++(int)
        {
            if constexpr (forward_range<V>) {
                auto tmp = *this;
                ++*this;
                return tmp;
            } else {
                ++*this;
            }
        }

        template <typename B = Base>
        friend constexpr auto operator==(const inner_iterator& x, const inner_iterator& y)
            -> std::enable_if_t<forward_range<B>, bool>
        {
            return x.get_outer_current() == y.get_outer_current();
        }

        template <typename B = Base>
        friend constexpr auto operator!=(const inner_iterator& x, const inner_iterator& y)
            -> std::enable_if_t<forward_range<B>, bool>
        {
            return !(x == y);
        }

        friend constexpr bool operator==(const inner_iterator& x, default_sentinel_t)
        {
            return x.done();
        }

        friend constexpr bool operator==(default_sentinel_t d, const inner_iterator& x)
        {
            return x == d;
        }

        friend constexpr bool operator!=(const inner_iterator& x, default_sentinel_t d)
        {
            return !(x == d);
        }

        friend constexpr bool operator!=(default_sentinel_t d, const inner_iterator& x)
        {
            return !(x == d);
        }

        friend constexpr decltype(auto) iter_move(const inner_iterator& i)
            noexcept(noexcept(ranges::iter_move(i.get_outer_current())))
        {
            return ranges::iter_move(i.get_outer_current());
        }

        template <typename B = Base>
        friend constexpr auto iter_swap(const inner_iterator& x, const inner_iterator& y)
            noexcept(noexcept(ranges::iter_swap(x.get_outer_current(), y.get_outer_current())))
            -> std::enable_if_t<indirectly_swappable<B>>
        {
            ranges::iter_swap(x.get_outer_current(), y.get_outer_current());
        }
    };

public:
    split_view() = default;

    constexpr split_view(V base, Pattern pattern)
        : data_{std::move(base), std::move(pattern)}
    {}

    template <typename R, typename P,
              std::enable_if_t<constructible_from<V, all_view<R>>, int> = 0,
              std::enable_if_t<constructible_from<Pattern, all_view<P>>, int> = 0,
              std::enable_if_t<input_range<R> && forward_range<P>, int> = 0>
    constexpr split_view(R&& r, P&& p)
        : data_{views::all(std::forward<R>(r)) , views::all(std::forward<P>(p))}
    {}

    template <typename R,
              std::enable_if_t<constructible_from<V, all_view<R>>, int> = 0,
              std::enable_if_t<
                  constructible_from<Pattern, single_view<range_value_t<R>>>, int> = 0,
              std::enable_if_t<input_range<R>, int> = 0>
    constexpr split_view(R&& r, range_value_t<R> e)
        : data_{views::all(std::forward<R>(r)), single_view{std::move(e)}}
    {}

    constexpr auto begin()
    {
        if constexpr (forward_range<V>) {
            return outer_iterator<detail::simple_view<V>>{*this, ranges::begin(data_.base_)};
        } else {
            data_.current_ = ranges::begin(data_.base_);
            return outer_iterator<false>{*this};
        }
    }

    template <typename VV = V,
              std::enable_if_t<forward_range<VV> && forward_range<const VV>, int> = 0>
    constexpr auto begin() const
    {
        return outer_iterator<true>{*this, ranges::begin(data_.base_)};
    }

    template <typename VV = V,
              std::enable_if_t<forward_range<VV> && common_range<VV>, int> = 0>
    constexpr auto end()
    {
        return outer_iterator<detail::simple_view<V>>{*this, ranges::end(data_.base_)};
    }

    constexpr auto end() const
    {
        if constexpr (forward_range<V> &&
                      forward_range<const V> &&
                      common_range<const V>) {
            return outer_iterator<true>{*this, ranges::end(data_.base_)};
        } else {
            return default_sentinel;
        }
    }
};

template <typename R, typename P>
split_view(R&&, P&&) -> split_view<all_view<R>, all_view<P>>;

template <typename R, std::enable_if_t<input_range<R>, int> = 0>
split_view(R&&, range_value_t<R>)
    -> split_view<all_view<R>, single_view<range_value_t<R>>>;

} // namespace split_view_

using split_view_::split_view;

namespace detail {

struct split_view_fn {

    template <typename E, typename F>
    constexpr auto operator()(E&& e, F&& f) const
        -> decltype(split_view{std::forward<E>(e), std::forward<F>(f)})
    {
        return split_view{std::forward<E>(e), std::forward<F>(f)};
    }

    template <typename P>
    constexpr auto operator()(P&& p) const
    {
        return detail::rao_proxy{
            [p = std::forward<P>(p)](auto&& r) mutable
#ifndef NANO_MSVC_LAMBDA_PIPE_WORKAROUND
                 -> decltype(split_view{std::forward<decltype(r)>(r), std::declval<P&&>()})
#endif
            {
                return split_view{std::forward<decltype(r)>(r), std::move(p)};
            }};
    }
};

} // namespace detail

namespace views {

NANO_INLINE_VAR(nano::detail::split_view_fn, split)

}

NANO_END_NAMESPACE

#endif
