// cmcstl2 - A concept-enabled C++ standard library
//
//  Copyright Casey Carter 2015
//  Copyright Eric Niebler 2015
//
//  Use, modification and distribution is subject to the
//  Boost Software License, Version 1.0. (See accompanying
//  file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt)
//
// Project home: https://github.com/caseycarter/cmcstl2
//
#include "validate.hpp"

#include <nanorange/ranges.hpp>

#include <iostream>

namespace ns = ::nano::ranges;

template <bool allow_nonconst, bool allow_const, bool allow_size>
struct arbitrary_range {
    template <bool B = allow_nonconst, typename = std::enable_if_t<B>>
	int* begin();
    template <bool B = allow_nonconst, typename = std::enable_if_t<B>>
    int* end();
    template <bool B = allow_const, typename = std::enable_if_t<B>>
    const int* begin() const;
    template <bool B = allow_const, typename = std::enable_if_t<B>>
    const int* end() const;

    template <bool B = allow_size, typename = std::enable_if_t<B>>
    int size() const;
};

#include <set>
#include <unordered_set>
#include <vector>

using mutable_unsized_range =
	arbitrary_range<true, true, false>;

using mutable_only_unsized_range =
	arbitrary_range<true, false, false>;

using immutable_unsized_range =
	arbitrary_range<false, true, false>;

using mutable_sized_range =
	arbitrary_range<true, true, true>;

using mutable_only_sized_range =
	arbitrary_range<true, false, true>;

using immutable_sized_range =
	arbitrary_range<false, true, true>;

// size() launches the missiles.
struct mutable_badsized_range :
	mutable_sized_range {};

struct mutable_only_badsized_range :
	mutable_only_sized_range {};

struct immutable_badsized_range :
	immutable_sized_range {};

namespace nano {
template <>
constexpr bool disable_sized_range<mutable_badsized_range> = true;
template <>
constexpr bool disable_sized_range<mutable_only_badsized_range> = true;
template <>
constexpr bool disable_sized_range<immutable_badsized_range> = true;
}

struct strange_view
{
	std::vector<int>::iterator begin();
	std::vector<int>::const_iterator begin() const;

	std::vector<int>::iterator end();
	std::vector<int>::const_iterator end() const;
};

struct strange_view2 : strange_view, ns::view_base {};
struct strange_view3 : strange_view2 {};

namespace nano {
	template <>
	inline constexpr bool enable_view<strange_view> = true;
	template <>
	inline constexpr bool enable_view<strange_view3> = false;
}

namespace models = nano::ranges;

void ridiculously_exhaustive_range_property_test() {
	CONCEPT_ASSERT(!models::range<void>);
	CONCEPT_ASSERT(!models::sized_range<void>);
	//CONCEPT_ASSERT(!models::_ContainerLike<void>);
	CONCEPT_ASSERT(!models::view<void>);

	using I = int*;
	using CI = const int*;

	CONCEPT_ASSERT(models::input_or_output_iterator<I>);
	CONCEPT_ASSERT(models::input_or_output_iterator<CI>);

	CONCEPT_ASSERT(models::same_as<ns::iterator_t<int[2]>, I>);
	CONCEPT_ASSERT(models::same_as<ns::sentinel_t<int[2]>, I>);
	CONCEPT_ASSERT(models::range<int[2]>);
	CONCEPT_ASSERT(models::sized_range<int[2]>);
	//CONCEPT_ASSERT(models::_ContainerLike<int[2]>);
	CONCEPT_ASSERT(!models::view<int[2]>);

	CONCEPT_ASSERT(models::same_as<ns::iterator_t<int(&)[2]>, I>);
	CONCEPT_ASSERT(models::same_as<ns::sentinel_t<int(&)[2]>, I>);
	CONCEPT_ASSERT(models::range<int(&)[2]>);
	CONCEPT_ASSERT(models::sized_range<int(&)[2]>);
	//CONCEPT_ASSERT(!models::_ContainerLike<int(&)[2]>);
	CONCEPT_ASSERT(!models::view<int(&)[2]>);

	CONCEPT_ASSERT(models::same_as<ns::iterator_t<const int[2]>, CI>);
	CONCEPT_ASSERT(models::same_as<ns::sentinel_t<const int[2]>, CI>);
	CONCEPT_ASSERT(models::range<const int[2]>);
	CONCEPT_ASSERT(models::sized_range<const int[2]>);
	//CONCEPT_ASSERT(!models::_ContainerLike<const int[2]>);
	CONCEPT_ASSERT(!models::view<const int[2]>);

	CONCEPT_ASSERT(models::same_as<ns::iterator_t<const int(&)[2]>, CI>);
	CONCEPT_ASSERT(models::same_as<ns::sentinel_t<const int(&)[2]>, CI>);
	CONCEPT_ASSERT(models::range<const int(&)[2]>);
	CONCEPT_ASSERT(models::sized_range<const int(&)[2]>);
	//CONCEPT_ASSERT(!models::_ContainerLike<const int(&)[2]>);
	CONCEPT_ASSERT(!models::view<const int(&)[2]>);

	CONCEPT_ASSERT(models::same_as<ns::iterator_t<mutable_unsized_range>, I>);
	CONCEPT_ASSERT(models::same_as<ns::sentinel_t<mutable_unsized_range>, I>);
	CONCEPT_ASSERT(models::range<mutable_unsized_range>);
	CONCEPT_ASSERT(models::sized_range<mutable_unsized_range>);
	//CONCEPT_ASSERT(models::_ContainerLike<mutable_unsized_range>);
	CONCEPT_ASSERT(!models::view<mutable_unsized_range>);

	CONCEPT_ASSERT(models::same_as<ns::iterator_t<mutable_unsized_range&>, I>);
	CONCEPT_ASSERT(models::same_as<ns::sentinel_t<mutable_unsized_range&>, I>);
	CONCEPT_ASSERT(models::range<mutable_unsized_range&>);
	CONCEPT_ASSERT(models::sized_range<mutable_unsized_range&>);
	//CONCEPT_ASSERT(models::_ContainerLike<mutable_unsized_range>);
	CONCEPT_ASSERT(!models::view<mutable_unsized_range>);

	CONCEPT_ASSERT(models::same_as<ns::iterator_t<const mutable_unsized_range>, CI>);
	CONCEPT_ASSERT(models::same_as<ns::sentinel_t<const mutable_unsized_range>, CI>);
	CONCEPT_ASSERT(models::range<const mutable_unsized_range>);
	CONCEPT_ASSERT(models::sized_range<const mutable_unsized_range>);
	//CONCEPT_ASSERT(!models::_ContainerLike<const mutable_unsized_range>);
	CONCEPT_ASSERT(!models::view<const mutable_unsized_range>);

	CONCEPT_ASSERT(models::same_as<ns::iterator_t<const mutable_unsized_range&>, CI>);
	CONCEPT_ASSERT(models::same_as<ns::sentinel_t<const mutable_unsized_range&>, CI>);
	CONCEPT_ASSERT(models::range<const mutable_unsized_range&>);
	CONCEPT_ASSERT(models::sized_range<const mutable_unsized_range&>);
	//CONCEPT_ASSERT(!models::_ContainerLike<const mutable_unsized_range&>);
	CONCEPT_ASSERT(!models::view<const mutable_unsized_range&>);


	CONCEPT_ASSERT(models::same_as<ns::iterator_t<mutable_only_unsized_range&>, I>);
	CONCEPT_ASSERT(models::same_as<ns::sentinel_t<mutable_only_unsized_range&>, I>);
	CONCEPT_ASSERT(models::range<mutable_only_unsized_range>);
	// FIXME: This no longer asserts with P0970. Is that intentional?
//	CONCEPT_ASSERT(!models::SizedRange<mutable_only_unsized_range>);
	//CONCEPT_ASSERT(!models::_ContainerLike<mutable_only_unsized_range>);
	CONCEPT_ASSERT(models::view<mutable_only_unsized_range>);

	CONCEPT_ASSERT(models::same_as<ns::iterator_t<mutable_only_unsized_range&>, I>);
	CONCEPT_ASSERT(models::same_as<ns::sentinel_t<mutable_only_unsized_range&>, I>);
	CONCEPT_ASSERT(models::range<mutable_only_unsized_range&>);
// FIXME: This no longer asserts with P0970. Is that intentional?
	//	CONCEPT_ASSERT(!models::SizedRange<mutable_only_unsized_range&>);
	//CONCEPT_ASSERT(!models::_ContainerLike<mutable_only_unsized_range&>);
	CONCEPT_ASSERT(!models::view<mutable_only_unsized_range&>);

	CONCEPT_ASSERT(!models::range<const mutable_only_unsized_range>);
	CONCEPT_ASSERT(!models::sized_range<const mutable_only_unsized_range>);
	//CONCEPT_ASSERT(!models::_ContainerLike<const mutable_only_unsized_range>);
	CONCEPT_ASSERT(!models::view<const mutable_only_unsized_range>);

	CONCEPT_ASSERT(!models::range<const mutable_only_unsized_range&>);
	CONCEPT_ASSERT(!models::sized_range<const mutable_only_unsized_range&>);
	//CONCEPT_ASSERT(!models::_ContainerLike<const mutable_only_unsized_range&>);
	CONCEPT_ASSERT(!models::view<const mutable_only_unsized_range&>);


	CONCEPT_ASSERT(models::same_as<ns::iterator_t<immutable_unsized_range>, CI>);
	CONCEPT_ASSERT(models::same_as<ns::sentinel_t<immutable_unsized_range>, CI>);
	CONCEPT_ASSERT(models::range<immutable_unsized_range>);
	CONCEPT_ASSERT(models::sized_range<immutable_unsized_range>);
	//CONCEPT_ASSERT(!models::_ContainerLike<immutable_unsized_range>);
	CONCEPT_ASSERT(models::view<immutable_unsized_range>);

	CONCEPT_ASSERT(models::same_as<ns::iterator_t<immutable_unsized_range&>, CI>);
	CONCEPT_ASSERT(models::same_as<ns::sentinel_t<immutable_unsized_range&>, CI>);
	CONCEPT_ASSERT(models::range<immutable_unsized_range&>);
	CONCEPT_ASSERT(models::sized_range<immutable_unsized_range&>);
	//CONCEPT_ASSERT(!models::_ContainerLike<immutable_unsized_range&>);
	CONCEPT_ASSERT(!models::view<immutable_unsized_range&>);

	CONCEPT_ASSERT(models::same_as<ns::iterator_t<const immutable_unsized_range>, CI>);
	CONCEPT_ASSERT(models::same_as<ns::sentinel_t<const immutable_unsized_range>, CI>);
	CONCEPT_ASSERT(models::range<const immutable_unsized_range>);
	CONCEPT_ASSERT(models::sized_range<const immutable_unsized_range>);
	//CONCEPT_ASSERT(!models::_ContainerLike<const immutable_unsized_range>);
	CONCEPT_ASSERT(!models::view<const immutable_unsized_range>);

	CONCEPT_ASSERT(models::same_as<ns::iterator_t<const immutable_unsized_range&>, CI>);
	CONCEPT_ASSERT(models::same_as<ns::sentinel_t<const immutable_unsized_range&>, CI>);
	CONCEPT_ASSERT(models::range<const immutable_unsized_range&>);
	CONCEPT_ASSERT(models::sized_range<const immutable_unsized_range&>);
	//CONCEPT_ASSERT(!models::_ContainerLike<const immutable_unsized_range&>);
	CONCEPT_ASSERT(!models::view<const immutable_unsized_range&>);


	CONCEPT_ASSERT(models::same_as<ns::iterator_t<mutable_sized_range>, I>);
	CONCEPT_ASSERT(models::same_as<ns::sentinel_t<mutable_sized_range>, I>);
	CONCEPT_ASSERT(models::range<mutable_sized_range>);
	CONCEPT_ASSERT(models::sized_range<mutable_sized_range>);
	//CONCEPT_ASSERT(models::_ContainerLike<mutable_sized_range>);
	CONCEPT_ASSERT(!models::view<mutable_sized_range>);

	CONCEPT_ASSERT(models::same_as<ns::iterator_t<mutable_sized_range&>, I>);
	CONCEPT_ASSERT(models::same_as<ns::sentinel_t<mutable_sized_range&>, I>);
	CONCEPT_ASSERT(models::range<mutable_sized_range&>);
	CONCEPT_ASSERT(models::sized_range<mutable_sized_range&>);
	//CONCEPT_ASSERT(!models::_ContainerLike<mutable_sized_range&>);
	CONCEPT_ASSERT(!models::view<mutable_sized_range&>);

	CONCEPT_ASSERT(models::same_as<ns::iterator_t<const mutable_sized_range>, CI>);
	CONCEPT_ASSERT(models::same_as<ns::sentinel_t<const mutable_sized_range>, CI>);
	CONCEPT_ASSERT(models::range<const mutable_sized_range>);
	CONCEPT_ASSERT(models::sized_range<const mutable_sized_range>);
	//CONCEPT_ASSERT(!models::_ContainerLike<const mutable_sized_range>);
	CONCEPT_ASSERT(!models::view<const mutable_sized_range>);

	CONCEPT_ASSERT(models::same_as<ns::iterator_t<const mutable_sized_range&>, CI>);
	CONCEPT_ASSERT(models::same_as<ns::sentinel_t<const mutable_sized_range&>, CI>);
	CONCEPT_ASSERT(models::range<const mutable_sized_range&>);
	CONCEPT_ASSERT(models::sized_range<const mutable_sized_range&>);
	//CONCEPT_ASSERT(!models::_ContainerLike<const mutable_sized_range&>);
	CONCEPT_ASSERT(!models::view<const mutable_sized_range&>);


	CONCEPT_ASSERT(models::same_as<ns::iterator_t<mutable_only_sized_range>, I>);
	CONCEPT_ASSERT(models::same_as<ns::sentinel_t<mutable_only_sized_range>, I>);
	CONCEPT_ASSERT(models::range<mutable_only_sized_range>);
	CONCEPT_ASSERT(models::sized_range<mutable_only_sized_range>);
	//CONCEPT_ASSERT(!models::_ContainerLike<mutable_only_sized_range>);
	CONCEPT_ASSERT(models::view<mutable_only_sized_range>);

	CONCEPT_ASSERT(models::same_as<ns::iterator_t<mutable_only_sized_range&>, I>);
	CONCEPT_ASSERT(models::same_as<ns::sentinel_t<mutable_only_sized_range&>, I>);
	CONCEPT_ASSERT(models::range<mutable_only_sized_range&>);
	CONCEPT_ASSERT(models::sized_range<mutable_only_sized_range&>);
	//CONCEPT_ASSERT(!models::_ContainerLike<mutable_only_sized_range&>);
	CONCEPT_ASSERT(!models::view<mutable_only_sized_range&>);

	CONCEPT_ASSERT(!models::range<const mutable_only_sized_range>);
	CONCEPT_ASSERT(!models::sized_range<const mutable_only_sized_range>);
	//CONCEPT_ASSERT(!models::_ContainerLike<const mutable_only_sized_range>);
	CONCEPT_ASSERT(!models::view<const mutable_only_sized_range>);

	CONCEPT_ASSERT(!models::range<const mutable_only_sized_range&>);
	CONCEPT_ASSERT(!models::sized_range<const mutable_only_sized_range&>);
	//CONCEPT_ASSERT(!models::_ContainerLike<const mutable_only_sized_range&>);
	CONCEPT_ASSERT(!models::view<const mutable_only_sized_range&>);


	CONCEPT_ASSERT(models::same_as<ns::iterator_t<immutable_sized_range>, CI>);
	CONCEPT_ASSERT(models::same_as<ns::sentinel_t<immutable_sized_range>, CI>);
	CONCEPT_ASSERT(models::range<immutable_sized_range>);
	CONCEPT_ASSERT(models::sized_range<immutable_sized_range>);
	//CONCEPT_ASSERT(!models::_ContainerLike<immutable_sized_range>);
	CONCEPT_ASSERT(models::view<immutable_sized_range>);

	CONCEPT_ASSERT(models::same_as<ns::iterator_t<immutable_sized_range&>, CI>);
	CONCEPT_ASSERT(models::same_as<ns::sentinel_t<immutable_sized_range&>, CI>);
	CONCEPT_ASSERT(models::range<immutable_sized_range&>);
	CONCEPT_ASSERT(models::sized_range<immutable_sized_range&>);
	//CONCEPT_ASSERT(!models::_ContainerLike<immutable_sized_range&>);
	CONCEPT_ASSERT(!models::view<immutable_sized_range&>);

	CONCEPT_ASSERT(models::same_as<ns::iterator_t<const immutable_sized_range>, CI>);
	CONCEPT_ASSERT(models::same_as<ns::sentinel_t<const immutable_sized_range>, CI>);
	CONCEPT_ASSERT(models::range<const immutable_sized_range>);
	CONCEPT_ASSERT(models::sized_range<const immutable_sized_range>);
	//CONCEPT_ASSERT(!models::_ContainerLike<const immutable_sized_range>);
	CONCEPT_ASSERT(!models::view<const immutable_sized_range>);

	CONCEPT_ASSERT(models::same_as<ns::iterator_t<const immutable_sized_range&>, CI>);
	CONCEPT_ASSERT(models::same_as<ns::sentinel_t<const immutable_sized_range&>, CI>);
	CONCEPT_ASSERT(models::range<const immutable_sized_range&>);
	CONCEPT_ASSERT(models::sized_range<const immutable_sized_range&>);
	//CONCEPT_ASSERT(!models::_ContainerLike<const immutable_sized_range&>);
	CONCEPT_ASSERT(!models::view<const immutable_sized_range&>);


	CONCEPT_ASSERT(models::same_as<ns::iterator_t<mutable_badsized_range>, I>);
	CONCEPT_ASSERT(models::same_as<ns::sentinel_t<mutable_badsized_range>, I>);
	CONCEPT_ASSERT(models::range<mutable_badsized_range>);
	CONCEPT_ASSERT(!models::sized_range<mutable_badsized_range>);
	//CONCEPT_ASSERT(models::_ContainerLike<mutable_badsized_range>);
	CONCEPT_ASSERT(!models::view<mutable_badsized_range>);

	CONCEPT_ASSERT(models::same_as<ns::iterator_t<mutable_badsized_range&>, I>);
	CONCEPT_ASSERT(models::same_as<ns::sentinel_t<mutable_badsized_range&>, I>);
	CONCEPT_ASSERT(models::range<mutable_badsized_range&>);
	CONCEPT_ASSERT(!models::sized_range<mutable_badsized_range&>);
	//CONCEPT_ASSERT(!models::_ContainerLike<mutable_badsized_range&>);
	CONCEPT_ASSERT(!models::view<mutable_badsized_range&>);

	CONCEPT_ASSERT(models::same_as<ns::iterator_t<const mutable_badsized_range>, CI>);
	CONCEPT_ASSERT(models::same_as<ns::sentinel_t<const mutable_badsized_range>, CI>);
	CONCEPT_ASSERT(models::range<const mutable_badsized_range>);
	CONCEPT_ASSERT(!models::sized_range<const mutable_badsized_range>);
	//CONCEPT_ASSERT(!models::_ContainerLike<const mutable_badsized_range>);
	CONCEPT_ASSERT(!models::view<const mutable_badsized_range>);

	CONCEPT_ASSERT(models::same_as<ns::iterator_t<const mutable_badsized_range&>, CI>);
	CONCEPT_ASSERT(models::same_as<ns::sentinel_t<const mutable_badsized_range&>, CI>);
	CONCEPT_ASSERT(models::range<const mutable_badsized_range&>);
	CONCEPT_ASSERT(!models::sized_range<const mutable_badsized_range&>);
	//CONCEPT_ASSERT(!models::_ContainerLike<const mutable_badsized_range&>);
	CONCEPT_ASSERT(!models::view<const mutable_badsized_range&>);


	CONCEPT_ASSERT(models::same_as<ns::iterator_t<mutable_only_badsized_range>, I>);
	CONCEPT_ASSERT(models::same_as<ns::sentinel_t<mutable_only_badsized_range>, I>);
	CONCEPT_ASSERT(models::range<mutable_only_badsized_range>);
	CONCEPT_ASSERT(!models::sized_range<mutable_only_badsized_range>);
	//CONCEPT_ASSERT(!models::_ContainerLike<mutable_only_badsized_range>);
	CONCEPT_ASSERT(models::view<mutable_only_badsized_range>);

	CONCEPT_ASSERT(models::same_as<ns::iterator_t<mutable_only_badsized_range&>, I>);
	CONCEPT_ASSERT(models::same_as<ns::sentinel_t<mutable_only_badsized_range&>, I>);
	CONCEPT_ASSERT(models::range<mutable_only_badsized_range&>);
	CONCEPT_ASSERT(!models::sized_range<mutable_only_badsized_range&>);
	//CONCEPT_ASSERT(!models::_ContainerLike<mutable_only_badsized_range&>);
	CONCEPT_ASSERT(!models::view<mutable_only_badsized_range&>);

	CONCEPT_ASSERT(!models::range<const mutable_only_badsized_range>);
	CONCEPT_ASSERT(!models::sized_range<const mutable_only_badsized_range>);
	//CONCEPT_ASSERT(!models::_ContainerLike<const mutable_only_badsized_range>);
	CONCEPT_ASSERT(!models::view<const mutable_only_badsized_range>);

	CONCEPT_ASSERT(!models::range<const mutable_only_badsized_range&>);
	CONCEPT_ASSERT(!models::sized_range<const mutable_only_badsized_range&>);
	//CONCEPT_ASSERT(!models::_ContainerLike<const mutable_only_badsized_range&>);
	CONCEPT_ASSERT(!models::view<const mutable_only_badsized_range&>);


	CONCEPT_ASSERT(models::same_as<ns::iterator_t<immutable_badsized_range>, CI>);
	CONCEPT_ASSERT(models::same_as<ns::sentinel_t<immutable_badsized_range>, CI>);
	CONCEPT_ASSERT(models::range<immutable_badsized_range>);
	CONCEPT_ASSERT(!models::sized_range<immutable_badsized_range>);
	//CONCEPT_ASSERT(!models::_ContainerLike<immutable_badsized_range>);
	CONCEPT_ASSERT(models::view<immutable_badsized_range>);

	CONCEPT_ASSERT(models::same_as<ns::iterator_t<immutable_badsized_range&>, CI>);
	CONCEPT_ASSERT(models::same_as<ns::sentinel_t<immutable_badsized_range&>, CI>);
	CONCEPT_ASSERT(models::range<immutable_badsized_range&>);
	CONCEPT_ASSERT(!models::sized_range<immutable_badsized_range&>);
	//CONCEPT_ASSERT(!models::_ContainerLike<immutable_badsized_range&>);
	CONCEPT_ASSERT(!models::view<immutable_badsized_range&>);

	CONCEPT_ASSERT(models::same_as<ns::iterator_t<const immutable_badsized_range>, CI>);
	CONCEPT_ASSERT(models::same_as<ns::sentinel_t<const immutable_badsized_range>, CI>);
	CONCEPT_ASSERT(models::range<const immutable_badsized_range>);
	CONCEPT_ASSERT(!models::sized_range<const immutable_badsized_range>);
	//CONCEPT_ASSERT(!models::_ContainerLike<const immutable_badsized_range>);
	CONCEPT_ASSERT(!models::view<const immutable_badsized_range>);

	CONCEPT_ASSERT(models::same_as<ns::iterator_t<const immutable_badsized_range&>, CI>);
	CONCEPT_ASSERT(models::same_as<ns::sentinel_t<const immutable_badsized_range&>, CI>);
	CONCEPT_ASSERT(models::range<const immutable_badsized_range&>);
	CONCEPT_ASSERT(!models::sized_range<const immutable_badsized_range&>);
	//CONCEPT_ASSERT(!models::_ContainerLike<const immutable_badsized_range&>);
	CONCEPT_ASSERT(!models::view<const immutable_badsized_range&>);


	CONCEPT_ASSERT(models::same_as<ns::iterator_t<std::vector<int>>, std::vector<int>::iterator>);
	CONCEPT_ASSERT(models::same_as<ns::sentinel_t<std::vector<int>>, std::vector<int>::iterator>);
	CONCEPT_ASSERT(models::range<std::vector<int>>);
	CONCEPT_ASSERT(models::sized_range<std::vector<int>>);
	//CONCEPT_ASSERT(models::_ContainerLike<std::vector<int>>);
	CONCEPT_ASSERT(!models::view<std::vector<int>>);


	CONCEPT_ASSERT(models::range<strange_view>);
	CONCEPT_ASSERT(models::range<strange_view&>);
	CONCEPT_ASSERT(models::view<strange_view>);
	CONCEPT_ASSERT(!models::view<strange_view&>);
	CONCEPT_ASSERT(!models::view<const strange_view>);

	CONCEPT_ASSERT(models::range<strange_view2>);
	CONCEPT_ASSERT(models::range<strange_view2&>);
	CONCEPT_ASSERT(models::view<strange_view2>);
	CONCEPT_ASSERT(!models::view<strange_view2&>);
	CONCEPT_ASSERT(!models::view<const strange_view2>);

	CONCEPT_ASSERT(models::range<strange_view3>);
	CONCEPT_ASSERT(models::range<strange_view3&>);
	CONCEPT_ASSERT(!models::view<strange_view3>);
	CONCEPT_ASSERT(!models::view<strange_view3&>);
	CONCEPT_ASSERT(!models::view<const strange_view3>);

	CONCEPT_ASSERT(models::range<mutable_only_unsized_range&>);
	CONCEPT_ASSERT(models::range<mutable_only_unsized_range>);
	CONCEPT_ASSERT(models::view<mutable_only_unsized_range>);
	CONCEPT_ASSERT(!models::view<mutable_only_unsized_range&>);
	CONCEPT_ASSERT(!models::view<mutable_only_unsized_range&&>);
	CONCEPT_ASSERT(!models::range<const mutable_only_unsized_range&>);
	CONCEPT_ASSERT(!models::view<const mutable_only_unsized_range&>);
}

template <class I, class S,
	      std::enable_if_t<models::input_iterator<I> && models::sentinel_for<S, I>, int> = 0>
I complicated_algorithm(I i, S s) {
	static constexpr bool output = false;
	if (output) std::cout << '{';
	if (i != s) {
		if (output) std::cout << *i;
		while (++i != s) {
			if (output) std::cout << ", " << *i;
		}
	}
	if (output) std::cout << "}\n";
	return i;
}

template <class R, typename = std::enable_if_t<models::range<R>>>
ns::iterator_t<R> complicated_algorithm(R&& r) {
	return complicated_algorithm(ns::begin(r), ns::end(r));
}

template <class T>
struct array_view {
	T* first_;
	std::size_t n_;

	array_view() = default;
	template <std::size_t N>
	array_view(T (&a)[N]) : first_{a}, n_{N} {}

	auto begin() const { return first_; }
	auto end() const { return first_ + n_; }
	auto size() const { return n_; }
};

void complicated_algorithm_test() {
	static int some_ints[] = {2, 3, 5, 7};
	CONCEPT_ASSERT(models::range<decltype(some_ints)>);
	CONCEPT_ASSERT(models::sized_range<decltype(some_ints)>);
	//CONCEPT_ASSERT(models::_ContainerLike<decltype(some_ints)>);
	CONCEPT_ASSERT(!models::view<decltype(some_ints)>);
	CHECK(complicated_algorithm(some_ints) == ns::end(some_ints));
	CONCEPT_ASSERT(models::range<array_view<int>>);
	CONCEPT_ASSERT(models::sized_range<array_view<int>>);
	//CONCEPT_ASSERT(!models::_ContainerLike<array_view<int>>);
	CONCEPT_ASSERT(models::view<array_view<int>>);
	CHECK(complicated_algorithm(array_view<int>{some_ints}) == ns::end(some_ints));
}

TEST_CASE("concepts.range")
{
	ridiculously_exhaustive_range_property_test();
	complicated_algorithm_test();

	{
		using T = int[2];
		CONCEPT_ASSERT(models::common_range<T>);
		CONCEPT_ASSERT(models::output_range<T, int>);
		CONCEPT_ASSERT(models::output_range<T, const int&>);
		CONCEPT_ASSERT(models::input_range<T>);
		CONCEPT_ASSERT(models::forward_range<T>);
		CONCEPT_ASSERT(models::bidirectional_range<T>);
		CONCEPT_ASSERT(models::random_access_range<T>);
	}

	CONCEPT_ASSERT(!models::view<std::vector<int>>);
	CONCEPT_ASSERT(!models::view<std::set<int>>);
	CONCEPT_ASSERT(!models::view<std::multiset<int>>);
	CONCEPT_ASSERT(!models::view<std::unordered_set<int>>);
	CONCEPT_ASSERT(!models::view<std::unordered_multiset<int>>);
}
