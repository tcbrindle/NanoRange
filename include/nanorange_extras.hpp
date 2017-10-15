// nanorange_extras.hpp
//
// Copyright (c) 2017 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_EXTRAS_HPP_INCLUDED
#define NANORANGE_EXTRAS_HPP_INCLUDED

#include "nanorange.hpp"

namespace nanorange {
inline namespace ext {

template <typename I, typename S>
struct iterator_range : std::pair<I, S>
{
    using std::pair<I, S>::pair;

    I begin() const { return this->first; }
    S end() const { return this->second; }
};

template <typename I, typename S,
          typename = std::enable_if_t<Iterator<I> && Sentinel<S, I>>>
iterator_range<I, S> make_range(I i, S s)
{
    return {std::move(i), std::move(s)};
}

} // namespace ext
} // namespace nanorange

#endif
