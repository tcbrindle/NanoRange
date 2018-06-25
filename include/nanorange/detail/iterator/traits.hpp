// nanorange/detail/iterator/traits.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_DETAIL_ITERATOR_TRAITS_HPP_INCLUDED
#define NANORANGE_DETAIL_ITERATOR_TRAITS_HPP_INCLUDED

#include <nanorange/detail/iterator/dereferenceable.hpp>
#include <nanorange/detail/iterator/iter_move.hpp>

#include <iterator>

NANO_BEGIN_NAMESPACE

// [range.iterator.assoc.types.iterator_category]
using std::bidirectional_iterator_tag;
using std::forward_iterator_tag;
using std::input_iterator_tag;
using std::output_iterator_tag;
using std::random_access_iterator_tag;

struct contiguous_iterator_tag : random_access_iterator_tag {};

template <typename T>
struct iterator_category;

namespace detail {

template <typename T, typename = void>
struct iterator_category_ {
};

template <typename T>
struct iterator_category_<T*>
    : std::enable_if<std::is_object<T>::value, contiguous_iterator_tag> {
};

template <typename T>
struct iterator_category_<const T> : iterator_category<T> {
};

template <typename T>
struct iterator_category_<T, void_t<typename T::iterator_category>> {
    using type = typename T::iterator_category;
};

} // namespace detail

template <typename T>
struct iterator_category : detail::iterator_category_<T> {
};

template <typename T>
using iterator_category_t = typename iterator_category<T>::type;

namespace detail {

template <typename T, typename = void>
struct legacy_iterator_category
    : iterator_category<T> {};

template <typename T>
struct legacy_iterator_category<T,
        std::enable_if_t<std::is_same<iterator_category_t<T>, contiguous_iterator_tag>::value>>
{
    using type = random_access_iterator_tag;
};

template <typename T>
using legacy_iterator_category_t = typename legacy_iterator_category<T>::type;

}


namespace detail {

template <typename, typename = void>
struct reference_helper {
};

template <typename T>
struct reference_helper<T, std::enable_if_t<Dereferenceable<T>>> {
    using type = decltype(*std::declval<T&>());
};

} // namespace detail

template <typename T>
using iter_reference_t = typename detail::reference_helper<T>::type;

template <typename T>
using iter_rvalue_reference_t = decltype(ranges::iter_move(std::declval<T&>()));

NANO_END_NAMESPACE

#endif
