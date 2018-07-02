// nanorange/detail/ranges/dangling.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_DETAIL_RANGES_DANGLING_HPP_INCLUDED
#define NANORANGE_DETAIL_RANGES_DANGLING_HPP_INCLUDED

#include <nanorange/detail/ranges/concepts.hpp>

NANO_BEGIN_NAMESPACE

// [range.dangling.wrap]

template <typename T>
struct dangling {
    static_assert(CopyConstructible<T>,
                  "Argument to dangling<T> must satisfy CopyConstructible<T>");

    template <typename U = T, std::enable_if_t<DefaultConstructible<U>>>
    dangling() : value_{}
    {}

    dangling(T t) : value_(std::move(t)) {}

    T get_unsafe() { return value_; }

private:
    T value_;
};

namespace detail {

template <typename R, typename = void_t<iterator_t<R>>>
struct safe_iterator_helper {
    using type = dangling<iterator_t<R>>;
};

template <typename R>
struct safe_iterator_helper<
    R, void_t<decltype(ranges::begin(std::declval<R>()))>> {
    using type = iterator_t<R>;
};

} // namespace detail

template <typename Range>
using safe_iterator_t = typename detail::safe_iterator_helper<Range>::type;

NANO_END_NAMESPACE

#endif
