// nanorange/detail/memory/concepts.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_DETAIL_MEMORY_CONCEPTS_HPP_INCLUDED
#define NANORANGE_DETAIL_MEMORY_CONCEPTS_HPP_INCLUDED

#include <nanorange/detail/ranges/concepts.hpp>

NANO_BEGIN_NAMESPACE

namespace detail {

struct no_throw_input_iterator_concept {
    template <typename>
    static auto test(long) -> std::false_type;

    template <typename I>
    static auto test(int) -> std::enable_if_t<
        input_iterator<I> &&
        std::is_lvalue_reference<iter_reference_t<I>>::value &&
        same_as<remove_cvref_t<iter_reference_t<I>>, iter_value_t<I>>,
        std::true_type>;
};

template <typename I>
NANO_CONCEPT no_throw_input_iterator =
    decltype(no_throw_input_iterator_concept::test<I>(0))::value;

template <typename S, typename I>
NANO_CONCEPT no_throw_sentinel = sentinel_for<S, I>;

struct no_throw_input_range_concept {
    template <typename>
    static auto test(long) -> std::false_type;

    template <typename R>
    static auto test(int) -> std::enable_if_t<
        range<R> &&
        no_throw_input_iterator<iterator_t<R>> &&
        no_throw_sentinel<sentinel_t<R>, iterator_t<R>>,
        std::true_type>;
};

template <typename R>
NANO_CONCEPT no_throw_input_range =
    decltype(no_throw_input_range_concept::test<R>(0))::value;

template <typename I>
NANO_CONCEPT no_throw_forward_iterator =
    no_throw_input_iterator<I> && forward_iterator<I> && no_throw_sentinel<I, I>;

struct no_throw_forward_range_concept {
    template <typename>
    static auto test(long) -> std::false_type;

    template <typename R>
    static auto test(int) -> std::enable_if_t<
        no_throw_input_range<R> &&
        no_throw_forward_iterator<iterator_t<R>>,
        std::true_type>;
};

template <typename R>
NANO_CONCEPT no_throw_forward_range =
    decltype(no_throw_forward_range_concept::test<R>(0))::value;

template <typename T>
void* voidify(T& ptr) noexcept
{
    return const_cast<void*>(static_cast<const volatile void*>(std::addressof(ptr)));
}

}

NANO_END_NAMESPACE

#endif
