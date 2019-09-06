// nanorange/algorithm/minmax_element.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

// Uses code from CMCSTL2
// Copyright Casey Carter 2015

#ifndef NANORANGE_ALGORITHM_MINMAX_ELEMENT_HPP_INCLUDED
#define NANORANGE_ALGORITHM_MINMAX_ELEMENT_HPP_INCLUDED

#include <nanorange/algorithm/minmax.hpp>

NANO_BEGIN_NAMESPACE

namespace detail {

struct minmax_element_fn {
private:
    template <typename I, typename S, typename Comp, typename Proj>
    static constexpr minmax_result<I> impl(I first, S last, Comp& comp, Proj& proj)
    {
        minmax_result<I> result{first, first};

        if (first == last || ++first == last) {
            return result;
        }

       if (nano::invoke(comp, nano::invoke(proj, *first),
                        nano::invoke(proj, *result.min))) {
           result.min = first;
       } else if (!nano::invoke(comp, nano::invoke(proj, *first),
                                nano::invoke(proj, *result.max))){
           result.max = first;
       }

       while (++first != last) {
           I it = first;

           // Last iteration
           if (++first == last) {
               if (nano::invoke(comp, nano::invoke(proj, *it),
                                nano::invoke(proj, *result.min))) {
                   result.min = std::move(it);
               }
               else if (!nano::invoke(comp, nano::invoke(proj, *it),
                                      nano::invoke(proj, *result.max))) {
                   result.max = std::move(it);
               }
               break;
           }

           if (nano::invoke(comp, nano::invoke(proj, *first),
                            nano::invoke(proj, *it))) {
               if (nano::invoke(comp, nano::invoke(proj, *first),
                                nano::invoke(proj, *result.min))) {
                   result.min = first;
               }
               if (!nano::invoke(comp, nano::invoke(proj, *it),
                                 nano::invoke(proj, *result.max))) {
                   result.max = it;
               }
           }
           else {
               if (nano::invoke(comp, nano::invoke(proj, *it),
                                nano::invoke(proj, *result.min))) {
                   result.min = it;
               }
               if (!nano::invoke(comp, nano::invoke(proj, *first),
                                 nano::invoke(proj, *result.max))) {
                   result.max = first;
               }
           }
       }

        return result;
    }


public:
    template <typename I, typename S, typename Comp = ranges::less,
            typename Proj = identity>
    constexpr std::enable_if_t<
        forward_iterator<I> && sentinel_for<S, I> &&
            indirect_strict_weak_order<Comp, projected<I, Proj>>,
        minmax_result<I>>
    operator()(I first, S last, Comp comp = Comp{}, Proj proj = Proj{}) const
    {
        return minmax_element_fn::impl(std::move(first), std::move(last),
                                    comp, proj);
    }

    template <typename Rng, typename Comp = ranges::less, typename Proj = identity>
    constexpr std::enable_if_t<
        forward_range<Rng> &&
            indirect_strict_weak_order<Comp, projected<iterator_t<Rng>, Proj>>,
        minmax_result<safe_iterator_t<Rng>>>
    operator()(Rng&& rng, Comp comp = Comp{}, Proj proj = Proj{}) const
    {
        return minmax_element_fn::impl(nano::begin(rng), nano::end(rng),
                                       comp, proj);
    }
};

}

NANO_INLINE_VAR(detail::minmax_element_fn, minmax_element)

NANO_END_NAMESPACE

#endif
