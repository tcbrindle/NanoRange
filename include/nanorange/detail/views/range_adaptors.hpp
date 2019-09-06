// nanorange/detail/views/range_adaptors.hpp
//
// Copyright (c) 2019 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_DETAIL_VIEW_RANGE_ADAPTORS_HPP_INCLUDED
#define NANORANGE_DETAIL_VIEW_RANGE_ADAPTORS_HPP_INCLUDED

#include <nanorange/detail/ranges/concepts.hpp>

NANO_BEGIN_NAMESPACE

namespace detail {

template <typename>
inline constexpr bool is_raco = false;

template <typename R, typename C,
          typename = std::enable_if_t<
              viewable_range<R> &&
              !is_raco<remove_cvref_t<R>> && is_raco<remove_cvref_t<C>>>>
constexpr auto operator|(R&& lhs, C&& rhs)
    -> decltype(std::forward<C>(rhs)(std::forward<R>(lhs)))
{
    return std::forward<C>(rhs)(std::forward<R>(lhs));
}

template <typename LHS, typename RHS>
struct raco_pipe {
private:
    LHS lhs_;
    RHS rhs_;

public:
    constexpr raco_pipe(LHS&& lhs, RHS&& rhs)
        : lhs_(std::move(lhs)),
          rhs_(std::move(rhs))
    {}

    // FIXME: Do I need to do ref-qualified overloads of these too?

    template <typename R, std::enable_if_t<viewable_range<R>, int> = 0>
    constexpr auto operator()(R&& r)
        -> decltype(rhs_(lhs_(std::forward<R>(r))))
    {
        return rhs_(lhs_(std::forward<R>(r)));
    }


    template <typename R, std::enable_if_t<viewable_range<R>, int> = 0>
    constexpr auto operator()(R&& r) const
        -> decltype(rhs_(lhs_(std::forward<R>(r))))
    {
        return rhs_(lhs_(std::forward<R>(r)));
    }
};

template <typename LHS, typename RHS>
inline constexpr bool is_raco<raco_pipe<LHS, RHS>> = true;

template <typename LHS, typename RHS>
constexpr auto operator|(LHS&& lhs, RHS&& rhs)
    -> std::enable_if_t<
        is_raco<remove_cvref_t<LHS>> && is_raco<remove_cvref_t<RHS>>,
        raco_pipe<LHS, RHS>>
{
    return raco_pipe<LHS, RHS>{std::forward<LHS>(lhs), std::forward<RHS>(rhs)};
}

template <typename Lambda>
struct rao_proxy : Lambda {
    constexpr explicit rao_proxy(Lambda&& l) : Lambda(std::move(l)) {}
};

template <typename L>
inline constexpr bool is_raco<rao_proxy<L>> = true;

} // namespace detail

NANO_END_NAMESPACE

#endif
