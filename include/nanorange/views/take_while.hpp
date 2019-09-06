// nanorange/views/take_while.hpp
//
// Copyright (c) 2019 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_VIEWS_TAKE_WHILE_HPP_INCLUDED
#define NANORANGE_VIEWS_TAKE_WHILE_HPP_INCLUDED

#include <nanorange/detail/views/range_adaptors.hpp>
#include <nanorange/detail/views/semiregular_box.hpp>
#include <nanorange/views/all.hpp>
#include <nanorange/views/interface.hpp>

NANO_BEGIN_NAMESPACE

template <typename R, typename Pred>
struct take_while_view : view_interface<take_while_view<R, Pred>> {
private:
    static_assert(view<R>);
    // FIXME: Should be input_range (GCC9)
    static_assert(input_iterator<iterator_t<R>>);
    static_assert(std::is_object_v<Pred>);
    static_assert(indirect_unary_predicate<const Pred, iterator_t<R>>);

    template <bool Const>
    struct sentinel {
    private:
        friend struct sentinel<!Const>;
        using base_t = detail::conditional_t<Const, const R, R>;
        sentinel_t<base_t> end_ = sentinel_t<base_t>();
        const Pred* pred_{};

    public:
        sentinel() = default;

        constexpr explicit sentinel(sentinel_t<base_t>(end), const Pred* pred)
            : end_(std::move(end)),
              pred_(pred)
        {}

        // Use deduced type to avoid constraint recursion in GCC8
        template <typename S,
                  std::enable_if_t<same_as<S, sentinel<!Const>>, int> = 0,
                  bool C = Const, typename VV = R,
                  std::enable_if_t<C &&
                      convertible_to<sentinel_t<VV>, sentinel_t<base_t>>, int> = 0>
        constexpr sentinel(S s)
            : end_(std::move(s.end_)),
              pred_(s.pred_)
        {}

        constexpr sentinel_t<base_t> base() const { return end_; }

        // Make these friend functions templates to keep MSVC happy
#if (defined(_MSC_VER) && _MSC_VER < 1922)
        template <typename = void>
#endif
        friend constexpr bool operator==(const iterator_t<base_t>& x, const sentinel& y)
        {
            return y.end_ == x || !nano::invoke(*y.pred_, *x);
        }

#if (defined(_MSC_VER) && _MSC_VER < 1922)
        template <typename = void>
#endif
        friend constexpr bool operator==(const sentinel& y, const iterator_t<base_t>& x)
        {
            return x == y;
        }

#if (defined(_MSC_VER) && _MSC_VER < 1922)
        template <typename = void>
#endif
        friend constexpr bool operator!=(const iterator_t<base_t>& x, const sentinel& y)
        {
            return !(x == y);
        }

#if (defined(_MSC_VER) && _MSC_VER < 1922)
        template <typename = void>
#endif
        friend constexpr bool operator!=(const sentinel& y, const iterator_t<base_t>& x)
        {
            return !(x == y);
        }
    };

    R base_;
    detail::semiregular_box<Pred> pred_;

public:
    take_while_view() = default;

    constexpr take_while_view(R base, Pred pred)
        : base_(std::move(base)),
          pred_(std::move(pred))
    {}

    constexpr R base() const { return base_; }

    constexpr const Pred& pred() const { return *pred_; }

    template <typename RR = R, std::enable_if_t<!detail::simple_view<RR>, int> = 0>
    constexpr auto begin() { return ranges::begin(base_); }

    template <typename RR = R, std::enable_if_t<range<const RR>, int> = 0>
    constexpr auto begin() const { return ranges::begin(base_); }

    template <typename RR = R, std::enable_if_t<!detail::simple_view<RR>, int> = 0>
    constexpr auto end()
    {
        return sentinel<false>{ranges::end(base_), std::addressof(*pred_)};
    }

    template <typename RR = R, std::enable_if_t<range<const RR>, int> = 0>
    constexpr auto end() const
    {
        return sentinel<true>{ranges::end(base_), std::addressof(*pred_)};
    }
};

template <typename R, typename Pred>
take_while_view(R&&, Pred) -> take_while_view<all_view<R>, Pred>;

namespace detail {

struct take_while_view_fn {

    template <typename E, typename F>
    constexpr auto operator()(E&& e, F&& f) const
    -> decltype(take_while_view{std::forward<E>(e), std::forward<F>(f)})
    {
        return take_while_view{std::forward<E>(e), std::forward<F>(f)};
    }

    template <typename Pred>
    constexpr auto operator()(Pred&& pred) const
    {
        return detail::rao_proxy{[p = std::forward<Pred>(pred)](auto&& r) mutable
#ifndef NANO_MSVC_LAMBDA_PIPE_WORKAROUND
            -> decltype(take_while_view{std::forward<decltype(r)>(r), std::declval<Pred&&>()})
#endif
        {
            return take_while_view{std::forward<decltype(r)>(r), std::move(p)};
        }};
    }

};

}

namespace views {

NANO_INLINE_VAR(nano::detail::take_while_view_fn, take_while)

}

NANO_END_NAMESPACE

#endif
