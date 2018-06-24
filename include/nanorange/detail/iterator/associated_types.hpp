// nanorange/detail/iterator/associated_types.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_DETAIL_ITERATOR_ASSOCIATED_TYPES_HPP_INCLUDED
#define NANORANGE_DETAIL_ITERATOR_ASSOCIATED_TYPES_HPP_INCLUDED

#include <nanorange/detail/concepts/core.hpp>
#include <nanorange/detail/type_traits.hpp>

NANO_BEGIN_NAMESPACE

template <typename>
struct difference_type;

namespace detail {

template <typename, typename = void>
struct difference_type_ {
};

// Workaround for GCC silliness: void* has no difference_type
// FIXME: This is required to stop WeaklyIncrementable<void*> being a hard error
// Can we formulate the concept differently to avoid the need for this hack?
template <>
struct difference_type_<void*> {};

template <typename T>
struct difference_type_<T*>
    : std::enable_if<std::is_object<T>::value, std::ptrdiff_t> {
};

template <class I>
struct difference_type_<const I> : difference_type<std::decay_t<I>> {
};

template <typename, typename = void>
struct has_member_difference_type_helper : std::false_type {};

template <typename T>
struct has_member_difference_type_helper<T, void_t<typename T::difference_type>>
    : std::true_type{};

template <typename T, typename = void>
constexpr bool has_member_difference_type_v =
        has_member_difference_type_helper<T>::value;

template <typename T>
struct difference_type_<T, std::enable_if_t<has_member_difference_type_v<T>>> {
    using type = typename T::difference_type;
};

template <typename T>
struct difference_type_<
    T, std::enable_if_t<!std::is_pointer<T>::value &&
                        !has_member_difference_type_v<T> &&
                        Integral<decltype(std::declval<const T&>() -
                                          std::declval<const T&>())>>>
    : std::make_signed<decltype(std::declval<T>() - std::declval<T>())> {
};

} // namespace detail

template <typename T>
struct difference_type : detail::difference_type_<T> {
};

template <typename T>
using difference_type_t = typename difference_type<T>::type;

// [range.iterator.assoc.types.value_type]

template <typename>
struct readable_traits;

namespace detail {

struct empty {};

template <typename T>
struct with_value_type {
    using value_type = T;
};

template <typename, typename = void>
struct readable_traits_helper {};

template <typename T>
struct readable_traits_helper<T*>
    : std::conditional_t<std::is_object<T>::value,
            with_value_type<std::remove_cv_t<T>>,
            empty> {};

template <typename I>
struct readable_traits_helper<I, std::enable_if_t<std::is_array<I>::value>>
    : readable_traits<std::decay_t<I>> {};

template <typename I>
struct readable_traits_helper<const I, std::enable_if_t<!std::is_array<I>::value>>
    : readable_traits<std::decay_t<I>> {};

template <typename T, typename V = typename T::value_type>
struct member_value_type
    : std::conditional_t<std::is_object<V>::value,
            with_value_type<V>, empty> {};

template <typename T, typename E = typename T::element_type>
struct member_element_type
    : std::conditional_t<std::is_object<E>::value,
            with_value_type<std::remove_cv_t<E>>, empty> {};

template <typename T>
using member_value_type_t = typename T::value_type;

template <typename T>
constexpr bool has_member_value_type_v = exists_v<member_value_type_t, T>;

template <typename T>
using member_element_type_t = typename T::element_type;

template <typename T>
constexpr bool has_member_element_type_v = exists_v<member_element_type_t, T>;

template <typename T>
struct readable_traits_helper<T, std::enable_if_t<has_member_value_type_v<T>>>
    : member_value_type<T> {};

template <typename T>
struct readable_traits_helper<T, std::enable_if_t<has_member_element_type_v<T>>>
    : member_element_type<T> {};

} // namespace detail

template <typename T>
struct readable_traits : detail::readable_traits_helper<T> {};

template <typename T>
using iter_value_t = typename readable_traits<T>::value_type;

NANO_END_NAMESPACE

#endif
