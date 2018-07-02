// nanorange/algorithm/minmax_element.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

// Uses code from CMCSTL2
// Copyright Casey Carter 2015

#ifndef NANORANGE_ALGORITHM_MINMAX_ELEMENT_HPP_INCLUDED
#define NANORANGE_ALGORITHM_MINMAX_ELEMENT_HPP_INCLUDED

#include <nanorange/ranges.hpp>

NANO_BEGIN_NAMESPACE

namespace detail {

// FIXME: Use tagged_pair
struct minmax_element_fn {
private:
    template <typename I, typename S, typename Comp, typename Proj>
    static constexpr std::pair<I, I> impl(I first, S last, Comp& comp, Proj& proj)
    {
        std::pair<I, I> result{first, first};

        if (first == last || ++first == last) {
            return result;
        }

       if (nano::invoke(comp, nano::invoke(proj, *first),
                        nano::invoke(proj, *result.first))) {
           result.first = first;
       } else if (!nano::invoke(comp, nano::invoke(proj, *first),
                                nano::invoke(proj, *result.second))){
           result.second = first;
       }

       while (++first != last) {
           I it = first;

           // Last iteration
           if (++first == last) {
               if (nano::invoke(comp, nano::invoke(proj, *it),
                                nano::invoke(proj, *result.first))) {
                   result.first = std::move(it);
               }
               else if (!nano::invoke(comp, nano::invoke(proj, *it),
                                      nano::invoke(proj, *result.second))) {
                   result.second = std::move(it);
               }
               break;
           }

           if (nano::invoke(comp, nano::invoke(proj, *first),
                            nano::invoke(proj, *it))) {
               if (nano::invoke(comp, nano::invoke(proj, *first),
                                nano::invoke(proj, *result.first))) {
                   result.first = first;
               }
               if (!nano::invoke(comp, nano::invoke(proj, *it),
                                 nano::invoke(proj, *result.second))) {
                   result.second = it;
               }
           }
           else {
               if (nano::invoke(comp, nano::invoke(proj, *it),
                                nano::invoke(proj, *result.first))) {
                   result.first = it;
               }
               if (!nano::invoke(comp, nano::invoke(proj, *first),
                                 nano::invoke(proj, *result.second))) {
                   result.second = first;
               }
           }
       }

        return result;
    }


public:
    template <typename I, typename S, typename Comp = less<>,
            typename Proj = identity>
    constexpr std::enable_if_t<
        ForwardIterator<I> &&
        Sentinel<S, I> &&
        IndirectStrictWeakOrder<Comp, projected<I, Proj>>,
        std::pair<I, I>>
    operator()(I first, S last, Comp comp = Comp{}, Proj proj = Proj{}) const
    {
        return minmax_element_fn::impl(std::move(first), std::move(last),
                                    comp, proj);
    }

    template <typename Rng, typename Comp = less<>, typename Proj = identity>
    constexpr std::enable_if_t<
        ForwardRange<Rng> &&
        IndirectStrictWeakOrder<Comp, projected<iterator_t<Rng>, Proj>>,
        std::pair<safe_iterator_t<Rng>, safe_iterator_t<Rng>>>
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
