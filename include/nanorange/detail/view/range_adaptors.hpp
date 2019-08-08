// nanorange/detail/view/range_adaptors.hpp
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

template <typename R, typename C, std::enable_if_t<
          ViewableRange<R> &&
          !is_raco<uncvref_t<R>> &&
          is_raco<uncvref_t<C>>, int> = 0>
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

    template <typename R, std::enable_if_t<
        ViewableRange<R>, int> = 0>
    constexpr auto operator()(R&& r)
        -> decltype(rhs_(lhs_(std::forward<R>(r))))
    {
        return rhs_(lhs_(std::forward<R>(r)));
    }


    template <typename R, std::enable_if_t<
              ViewableRange<R>, int> = 0>
    constexpr auto operator()(R&& r) const
        -> decltype(rhs_(lhs_(std::forward<R>(r))))
    {
        return rhs_(lhs_(std::forward<R>(r)));
    }
};

template <typename LHS, typename RHS>
inline constexpr bool is_raco<raco_pipe<LHS, RHS>> = true;

template <typename LHS, typename RHS, std::enable_if_t<
    is_raco<uncvref_t<LHS>> && is_raco<uncvref_t<RHS>>, int> = 0>
constexpr auto operator|(LHS&& lhs, RHS&& rhs)
{
    return raco_pipe<LHS, RHS>{std::forward<LHS>(lhs), std::forward<RHS>(rhs)};
}

} // namespace detail

NANO_END_NAMESPACE

#endif
