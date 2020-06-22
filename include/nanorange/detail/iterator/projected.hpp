// nanorange/detail/iterator/projected.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_DETAIL_ITERATOR_PROJECTED_HPP_INCLUDED
#define NANORANGE_DETAIL_ITERATOR_PROJECTED_HPP_INCLUDED

#include <nanorange/detail/functional/identity.hpp>
#include <nanorange/detail/iterator/indirect_callable_concepts.hpp>

NANO_BEGIN_NAMESPACE

// [range.projected]

//template <typename I, typename Proj>
//struct projected;

namespace detail {

template <typename, typename, typename = void>
struct projected_helper {
};

template <typename I, typename Proj>
struct projected_helper<
    I, Proj,
    std::enable_if_t<readable<I> &&
                     indirect_regular_unary_invocable<Proj, I>>> {
    using value_type = remove_cvref_t<indirect_result_t<Proj&, I>>;

    indirect_result_t<Proj&, I> operator*() const;
};

template <typename, typename, typename = void>
struct projected_difference_t_helper {};

template <typename I, typename Proj>
struct projected_difference_t_helper<I, Proj, std::enable_if_t<
    weakly_incrementable<I>>> {
    using difference_type = iter_difference_t<I>;
};

} // namespace detail

template <typename I, typename Proj>
using projected = detail::conditional_t<
    same_as<Proj, identity>, I, detail::projected_helper<I, Proj>>;

template <typename I, typename Proj>
struct incrementable_traits<detail::projected_helper<I, Proj>>
    : detail::projected_difference_t_helper<I, Proj> {};

NANO_END_NAMESPACE

#endif
