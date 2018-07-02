// nanorange/detail/iterator/projected.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_DETAIL_ITERATOR_PROJECTED_HPP_INCLUDED
#define NANORANGE_DETAIL_ITERATOR_PROJECTED_HPP_INCLUDED

#include <nanorange/detail/iterator/indirect_callable_concepts.hpp>

NANO_BEGIN_NAMESPACE

// [range.projected]

template <typename I, typename Proj>
struct projected;

namespace detail {

template <typename, typename, typename = void>
struct projected_helper {
};

template <typename I, typename Proj>
struct projected_helper<
    I, Proj,
    std::enable_if_t<Readable<I> && IndirectRegularUnaryInvocable<Proj, I>>> {
    using value_type =
        std::remove_cv_t<std::remove_reference_t<indirect_result_t<Proj&, I>>>;

    // We shouldn't need to define this, as we only need its return type,
    // but GCC gets stroppy sometimes.
    indirect_result_t<Proj&, I> operator*() const { throw 0; };
};

template <typename, typename, typename = void>
struct projected_difference_t_helper {};

template <typename I, typename Proj>
struct projected_difference_t_helper<I, Proj, std::enable_if_t<
    WeaklyIncrementable<I>>> {
    using difference_type = iter_difference_t<I>;
};

} // namespace detail

template <typename I, typename Proj>
struct projected : detail::projected_helper<I, Proj> {
};

template <typename I, typename Proj>
struct incrementable_traits<projected<I, Proj>>
    : detail::projected_difference_t_helper<I, Proj> {};

NANO_END_NAMESPACE

#endif