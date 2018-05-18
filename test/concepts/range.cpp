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

#include <nanorange.hpp>

#include <iostream>

namespace ns = ::nano::ranges;

// FIXME: This whole file gives MSVC a headache
#ifndef _MSC_VER

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
	struct enable_view<strange_view> : std::true_type {};
	template <>
	struct enable_view<strange_view3> : std::false_type {};
}

namespace models = nano::ranges;

void ridiculously_exhaustive_range_property_test() {
	CONCEPT_ASSERT(!models::Range<void>);
	CONCEPT_ASSERT(!models::SizedRange<void>);
	//CONCEPT_ASSERT(!models::_ContainerLike<void>);
	CONCEPT_ASSERT(!models::View<void>);

	using I = int*;
	using CI = const int*;

	CONCEPT_ASSERT(models::Iterator<I>);
	CONCEPT_ASSERT(models::Iterator<CI>);

	CONCEPT_ASSERT(models::Same<ns::iterator_t<int[2]>, I>);
	CONCEPT_ASSERT(models::Same<ns::sentinel_t<int[2]>, I>);
	CONCEPT_ASSERT(models::Range<int[2]>);
	CONCEPT_ASSERT(models::SizedRange<int[2]>);
	//CONCEPT_ASSERT(models::_ContainerLike<int[2]>);
	CONCEPT_ASSERT(!models::View<int[2]>);

	CONCEPT_ASSERT(models::Same<ns::iterator_t<int(&)[2]>, I>);
	CONCEPT_ASSERT(models::Same<ns::sentinel_t<int(&)[2]>, I>);
	CONCEPT_ASSERT(models::Range<int(&)[2]>);
	CONCEPT_ASSERT(models::SizedRange<int(&)[2]>);
	//CONCEPT_ASSERT(!models::_ContainerLike<int(&)[2]>);
	CONCEPT_ASSERT(!models::View<int(&)[2]>);

	CONCEPT_ASSERT(models::Same<ns::iterator_t<const int[2]>, CI>);
	CONCEPT_ASSERT(models::Same<ns::sentinel_t<const int[2]>, CI>);
	CONCEPT_ASSERT(models::Range<const int[2]>);
	CONCEPT_ASSERT(models::SizedRange<const int[2]>);
	//CONCEPT_ASSERT(!models::_ContainerLike<const int[2]>);
	CONCEPT_ASSERT(!models::View<const int[2]>);

	CONCEPT_ASSERT(models::Same<ns::iterator_t<const int(&)[2]>, CI>);
	CONCEPT_ASSERT(models::Same<ns::sentinel_t<const int(&)[2]>, CI>);
	CONCEPT_ASSERT(models::Range<const int(&)[2]>);
	CONCEPT_ASSERT(models::SizedRange<const int(&)[2]>);
	//CONCEPT_ASSERT(!models::_ContainerLike<const int(&)[2]>);
	CONCEPT_ASSERT(!models::View<const int(&)[2]>);

	CONCEPT_ASSERT(models::Same<ns::iterator_t<mutable_unsized_range>, I>);
	CONCEPT_ASSERT(models::Same<ns::sentinel_t<mutable_unsized_range>, I>);
	CONCEPT_ASSERT(models::Range<mutable_unsized_range>);
	CONCEPT_ASSERT(models::SizedRange<mutable_unsized_range>);
	//CONCEPT_ASSERT(models::_ContainerLike<mutable_unsized_range>);
	CONCEPT_ASSERT(!models::View<mutable_unsized_range>);

	CONCEPT_ASSERT(models::Same<ns::iterator_t<mutable_unsized_range&>, I>);
	CONCEPT_ASSERT(models::Same<ns::sentinel_t<mutable_unsized_range&>, I>);
	CONCEPT_ASSERT(models::Range<mutable_unsized_range&>);
	CONCEPT_ASSERT(models::SizedRange<mutable_unsized_range&>);
	//CONCEPT_ASSERT(models::_ContainerLike<mutable_unsized_range>);
	CONCEPT_ASSERT(!models::View<mutable_unsized_range>);

	CONCEPT_ASSERT(models::Same<ns::iterator_t<const mutable_unsized_range>, CI>);
	CONCEPT_ASSERT(models::Same<ns::sentinel_t<const mutable_unsized_range>, CI>);
	CONCEPT_ASSERT(models::Range<const mutable_unsized_range>);
	CONCEPT_ASSERT(models::SizedRange<const mutable_unsized_range>);
	//CONCEPT_ASSERT(!models::_ContainerLike<const mutable_unsized_range>);
	CONCEPT_ASSERT(!models::View<const mutable_unsized_range>);

	CONCEPT_ASSERT(models::Same<ns::iterator_t<const mutable_unsized_range&>, CI>);
	CONCEPT_ASSERT(models::Same<ns::sentinel_t<const mutable_unsized_range&>, CI>);
	CONCEPT_ASSERT(models::Range<const mutable_unsized_range&>);
	CONCEPT_ASSERT(models::SizedRange<const mutable_unsized_range&>);
	//CONCEPT_ASSERT(!models::_ContainerLike<const mutable_unsized_range&>);
	CONCEPT_ASSERT(!models::View<const mutable_unsized_range&>);


	CONCEPT_ASSERT(models::Same<ns::iterator_t<mutable_only_unsized_range&>, I>);
	CONCEPT_ASSERT(models::Same<ns::sentinel_t<mutable_only_unsized_range&>, I>);
	CONCEPT_ASSERT(models::Range<mutable_only_unsized_range>);
	// FIXME: This no longer asserts with P0970. Is that intentional?
//	CONCEPT_ASSERT(!models::SizedRange<mutable_only_unsized_range>);
	//CONCEPT_ASSERT(!models::_ContainerLike<mutable_only_unsized_range>);
	CONCEPT_ASSERT(models::View<mutable_only_unsized_range>);

	CONCEPT_ASSERT(models::Same<ns::iterator_t<mutable_only_unsized_range&>, I>);
	CONCEPT_ASSERT(models::Same<ns::sentinel_t<mutable_only_unsized_range&>, I>);
	CONCEPT_ASSERT(models::Range<mutable_only_unsized_range&>);
// FIXME: This no longer asserts with P0970. Is that intentional?
	//	CONCEPT_ASSERT(!models::SizedRange<mutable_only_unsized_range&>);
	//CONCEPT_ASSERT(!models::_ContainerLike<mutable_only_unsized_range&>);
	CONCEPT_ASSERT(!models::View<mutable_only_unsized_range&>);

	CONCEPT_ASSERT(!models::Range<const mutable_only_unsized_range>);
	CONCEPT_ASSERT(!models::SizedRange<const mutable_only_unsized_range>);
	//CONCEPT_ASSERT(!models::_ContainerLike<const mutable_only_unsized_range>);
	CONCEPT_ASSERT(!models::View<const mutable_only_unsized_range>);

	CONCEPT_ASSERT(!models::Range<const mutable_only_unsized_range&>);
	CONCEPT_ASSERT(!models::SizedRange<const mutable_only_unsized_range&>);
	//CONCEPT_ASSERT(!models::_ContainerLike<const mutable_only_unsized_range&>);
	CONCEPT_ASSERT(!models::View<const mutable_only_unsized_range&>);


	CONCEPT_ASSERT(models::Same<ns::iterator_t<immutable_unsized_range>, CI>);
	CONCEPT_ASSERT(models::Same<ns::sentinel_t<immutable_unsized_range>, CI>);
	CONCEPT_ASSERT(models::Range<immutable_unsized_range>);
	CONCEPT_ASSERT(models::SizedRange<immutable_unsized_range>);
	//CONCEPT_ASSERT(!models::_ContainerLike<immutable_unsized_range>);
	CONCEPT_ASSERT(models::View<immutable_unsized_range>);

	CONCEPT_ASSERT(models::Same<ns::iterator_t<immutable_unsized_range&>, CI>);
	CONCEPT_ASSERT(models::Same<ns::sentinel_t<immutable_unsized_range&>, CI>);
	CONCEPT_ASSERT(models::Range<immutable_unsized_range&>);
	CONCEPT_ASSERT(models::SizedRange<immutable_unsized_range&>);
	//CONCEPT_ASSERT(!models::_ContainerLike<immutable_unsized_range&>);
	CONCEPT_ASSERT(!models::View<immutable_unsized_range&>);

	CONCEPT_ASSERT(models::Same<ns::iterator_t<const immutable_unsized_range>, CI>);
	CONCEPT_ASSERT(models::Same<ns::sentinel_t<const immutable_unsized_range>, CI>);
	CONCEPT_ASSERT(models::Range<const immutable_unsized_range>);
	CONCEPT_ASSERT(models::SizedRange<const immutable_unsized_range>);
	//CONCEPT_ASSERT(!models::_ContainerLike<const immutable_unsized_range>);
	CONCEPT_ASSERT(!models::View<const immutable_unsized_range>);

	CONCEPT_ASSERT(models::Same<ns::iterator_t<const immutable_unsized_range&>, CI>);
	CONCEPT_ASSERT(models::Same<ns::sentinel_t<const immutable_unsized_range&>, CI>);
	CONCEPT_ASSERT(models::Range<const immutable_unsized_range&>);
	CONCEPT_ASSERT(models::SizedRange<const immutable_unsized_range&>);
	//CONCEPT_ASSERT(!models::_ContainerLike<const immutable_unsized_range&>);
	CONCEPT_ASSERT(!models::View<const immutable_unsized_range&>);


	CONCEPT_ASSERT(models::Same<ns::iterator_t<mutable_sized_range>, I>);
	CONCEPT_ASSERT(models::Same<ns::sentinel_t<mutable_sized_range>, I>);
	CONCEPT_ASSERT(models::Range<mutable_sized_range>);
	CONCEPT_ASSERT(models::SizedRange<mutable_sized_range>);
	//CONCEPT_ASSERT(models::_ContainerLike<mutable_sized_range>);
	CONCEPT_ASSERT(!models::View<mutable_sized_range>);

	CONCEPT_ASSERT(models::Same<ns::iterator_t<mutable_sized_range&>, I>);
	CONCEPT_ASSERT(models::Same<ns::sentinel_t<mutable_sized_range&>, I>);
	CONCEPT_ASSERT(models::Range<mutable_sized_range&>);
	CONCEPT_ASSERT(models::SizedRange<mutable_sized_range&>);
	//CONCEPT_ASSERT(!models::_ContainerLike<mutable_sized_range&>);
	CONCEPT_ASSERT(!models::View<mutable_sized_range&>);

	CONCEPT_ASSERT(models::Same<ns::iterator_t<const mutable_sized_range>, CI>);
	CONCEPT_ASSERT(models::Same<ns::sentinel_t<const mutable_sized_range>, CI>);
	CONCEPT_ASSERT(models::Range<const mutable_sized_range>);
	CONCEPT_ASSERT(models::SizedRange<const mutable_sized_range>);
	//CONCEPT_ASSERT(!models::_ContainerLike<const mutable_sized_range>);
	CONCEPT_ASSERT(!models::View<const mutable_sized_range>);

	CONCEPT_ASSERT(models::Same<ns::iterator_t<const mutable_sized_range&>, CI>);
	CONCEPT_ASSERT(models::Same<ns::sentinel_t<const mutable_sized_range&>, CI>);
	CONCEPT_ASSERT(models::Range<const mutable_sized_range&>);
	CONCEPT_ASSERT(models::SizedRange<const mutable_sized_range&>);
	//CONCEPT_ASSERT(!models::_ContainerLike<const mutable_sized_range&>);
	CONCEPT_ASSERT(!models::View<const mutable_sized_range&>);


	CONCEPT_ASSERT(models::Same<ns::iterator_t<mutable_only_sized_range>, I>);
	CONCEPT_ASSERT(models::Same<ns::sentinel_t<mutable_only_sized_range>, I>);
	CONCEPT_ASSERT(models::Range<mutable_only_sized_range>);
	CONCEPT_ASSERT(models::SizedRange<mutable_only_sized_range>);
	//CONCEPT_ASSERT(!models::_ContainerLike<mutable_only_sized_range>);
	CONCEPT_ASSERT(models::View<mutable_only_sized_range>);

	CONCEPT_ASSERT(models::Same<ns::iterator_t<mutable_only_sized_range&>, I>);
	CONCEPT_ASSERT(models::Same<ns::sentinel_t<mutable_only_sized_range&>, I>);
	CONCEPT_ASSERT(models::Range<mutable_only_sized_range&>);
	CONCEPT_ASSERT(models::SizedRange<mutable_only_sized_range&>);
	//CONCEPT_ASSERT(!models::_ContainerLike<mutable_only_sized_range&>);
	CONCEPT_ASSERT(!models::View<mutable_only_sized_range&>);

	CONCEPT_ASSERT(!models::Range<const mutable_only_sized_range>);
	CONCEPT_ASSERT(!models::SizedRange<const mutable_only_sized_range>);
	//CONCEPT_ASSERT(!models::_ContainerLike<const mutable_only_sized_range>);
	CONCEPT_ASSERT(!models::View<const mutable_only_sized_range>);

	CONCEPT_ASSERT(!models::Range<const mutable_only_sized_range&>);
	CONCEPT_ASSERT(!models::SizedRange<const mutable_only_sized_range&>);
	//CONCEPT_ASSERT(!models::_ContainerLike<const mutable_only_sized_range&>);
	CONCEPT_ASSERT(!models::View<const mutable_only_sized_range&>);


	CONCEPT_ASSERT(models::Same<ns::iterator_t<immutable_sized_range>, CI>);
	CONCEPT_ASSERT(models::Same<ns::sentinel_t<immutable_sized_range>, CI>);
	CONCEPT_ASSERT(models::Range<immutable_sized_range>);
	CONCEPT_ASSERT(models::SizedRange<immutable_sized_range>);
	//CONCEPT_ASSERT(!models::_ContainerLike<immutable_sized_range>);
	CONCEPT_ASSERT(models::View<immutable_sized_range>);

	CONCEPT_ASSERT(models::Same<ns::iterator_t<immutable_sized_range&>, CI>);
	CONCEPT_ASSERT(models::Same<ns::sentinel_t<immutable_sized_range&>, CI>);
	CONCEPT_ASSERT(models::Range<immutable_sized_range&>);
	CONCEPT_ASSERT(models::SizedRange<immutable_sized_range&>);
	//CONCEPT_ASSERT(!models::_ContainerLike<immutable_sized_range&>);
	CONCEPT_ASSERT(!models::View<immutable_sized_range&>);

	CONCEPT_ASSERT(models::Same<ns::iterator_t<const immutable_sized_range>, CI>);
	CONCEPT_ASSERT(models::Same<ns::sentinel_t<const immutable_sized_range>, CI>);
	CONCEPT_ASSERT(models::Range<const immutable_sized_range>);
	CONCEPT_ASSERT(models::SizedRange<const immutable_sized_range>);
	//CONCEPT_ASSERT(!models::_ContainerLike<const immutable_sized_range>);
	CONCEPT_ASSERT(!models::View<const immutable_sized_range>);

	CONCEPT_ASSERT(models::Same<ns::iterator_t<const immutable_sized_range&>, CI>);
	CONCEPT_ASSERT(models::Same<ns::sentinel_t<const immutable_sized_range&>, CI>);
	CONCEPT_ASSERT(models::Range<const immutable_sized_range&>);
	CONCEPT_ASSERT(models::SizedRange<const immutable_sized_range&>);
	//CONCEPT_ASSERT(!models::_ContainerLike<const immutable_sized_range&>);
	CONCEPT_ASSERT(!models::View<const immutable_sized_range&>);


	CONCEPT_ASSERT(models::Same<ns::iterator_t<mutable_badsized_range>, I>);
	CONCEPT_ASSERT(models::Same<ns::sentinel_t<mutable_badsized_range>, I>);
	CONCEPT_ASSERT(models::Range<mutable_badsized_range>);
	CONCEPT_ASSERT(!models::SizedRange<mutable_badsized_range>);
	//CONCEPT_ASSERT(models::_ContainerLike<mutable_badsized_range>);
	CONCEPT_ASSERT(!models::View<mutable_badsized_range>);

	CONCEPT_ASSERT(models::Same<ns::iterator_t<mutable_badsized_range&>, I>);
	CONCEPT_ASSERT(models::Same<ns::sentinel_t<mutable_badsized_range&>, I>);
	CONCEPT_ASSERT(models::Range<mutable_badsized_range&>);
	CONCEPT_ASSERT(!models::SizedRange<mutable_badsized_range&>);
	//CONCEPT_ASSERT(!models::_ContainerLike<mutable_badsized_range&>);
	CONCEPT_ASSERT(!models::View<mutable_badsized_range&>);

	CONCEPT_ASSERT(models::Same<ns::iterator_t<const mutable_badsized_range>, CI>);
	CONCEPT_ASSERT(models::Same<ns::sentinel_t<const mutable_badsized_range>, CI>);
	CONCEPT_ASSERT(models::Range<const mutable_badsized_range>);
	CONCEPT_ASSERT(!models::SizedRange<const mutable_badsized_range>);
	//CONCEPT_ASSERT(!models::_ContainerLike<const mutable_badsized_range>);
	CONCEPT_ASSERT(!models::View<const mutable_badsized_range>);

	CONCEPT_ASSERT(models::Same<ns::iterator_t<const mutable_badsized_range&>, CI>);
	CONCEPT_ASSERT(models::Same<ns::sentinel_t<const mutable_badsized_range&>, CI>);
	CONCEPT_ASSERT(models::Range<const mutable_badsized_range&>);
	CONCEPT_ASSERT(!models::SizedRange<const mutable_badsized_range&>);
	//CONCEPT_ASSERT(!models::_ContainerLike<const mutable_badsized_range&>);
	CONCEPT_ASSERT(!models::View<const mutable_badsized_range&>);


	CONCEPT_ASSERT(models::Same<ns::iterator_t<mutable_only_badsized_range>, I>);
	CONCEPT_ASSERT(models::Same<ns::sentinel_t<mutable_only_badsized_range>, I>);
	CONCEPT_ASSERT(models::Range<mutable_only_badsized_range>);
	CONCEPT_ASSERT(!models::SizedRange<mutable_only_badsized_range>);
	//CONCEPT_ASSERT(!models::_ContainerLike<mutable_only_badsized_range>);
	CONCEPT_ASSERT(models::View<mutable_only_badsized_range>);

	CONCEPT_ASSERT(models::Same<ns::iterator_t<mutable_only_badsized_range&>, I>);
	CONCEPT_ASSERT(models::Same<ns::sentinel_t<mutable_only_badsized_range&>, I>);
	CONCEPT_ASSERT(models::Range<mutable_only_badsized_range&>);
	CONCEPT_ASSERT(!models::SizedRange<mutable_only_badsized_range&>);
	//CONCEPT_ASSERT(!models::_ContainerLike<mutable_only_badsized_range&>);
	CONCEPT_ASSERT(!models::View<mutable_only_badsized_range&>);

	CONCEPT_ASSERT(!models::Range<const mutable_only_badsized_range>);
	CONCEPT_ASSERT(!models::SizedRange<const mutable_only_badsized_range>);
	//CONCEPT_ASSERT(!models::_ContainerLike<const mutable_only_badsized_range>);
	CONCEPT_ASSERT(!models::View<const mutable_only_badsized_range>);

	CONCEPT_ASSERT(!models::Range<const mutable_only_badsized_range&>);
	CONCEPT_ASSERT(!models::SizedRange<const mutable_only_badsized_range&>);
	//CONCEPT_ASSERT(!models::_ContainerLike<const mutable_only_badsized_range&>);
	CONCEPT_ASSERT(!models::View<const mutable_only_badsized_range&>);


	CONCEPT_ASSERT(models::Same<ns::iterator_t<immutable_badsized_range>, CI>);
	CONCEPT_ASSERT(models::Same<ns::sentinel_t<immutable_badsized_range>, CI>);
	CONCEPT_ASSERT(models::Range<immutable_badsized_range>);
	CONCEPT_ASSERT(!models::SizedRange<immutable_badsized_range>);
	//CONCEPT_ASSERT(!models::_ContainerLike<immutable_badsized_range>);
	CONCEPT_ASSERT(models::View<immutable_badsized_range>);

	CONCEPT_ASSERT(models::Same<ns::iterator_t<immutable_badsized_range&>, CI>);
	CONCEPT_ASSERT(models::Same<ns::sentinel_t<immutable_badsized_range&>, CI>);
	CONCEPT_ASSERT(models::Range<immutable_badsized_range&>);
	CONCEPT_ASSERT(!models::SizedRange<immutable_badsized_range&>);
	//CONCEPT_ASSERT(!models::_ContainerLike<immutable_badsized_range&>);
	CONCEPT_ASSERT(!models::View<immutable_badsized_range&>);

	CONCEPT_ASSERT(models::Same<ns::iterator_t<const immutable_badsized_range>, CI>);
	CONCEPT_ASSERT(models::Same<ns::sentinel_t<const immutable_badsized_range>, CI>);
	CONCEPT_ASSERT(models::Range<const immutable_badsized_range>);
	CONCEPT_ASSERT(!models::SizedRange<const immutable_badsized_range>);
	//CONCEPT_ASSERT(!models::_ContainerLike<const immutable_badsized_range>);
	CONCEPT_ASSERT(!models::View<const immutable_badsized_range>);

	CONCEPT_ASSERT(models::Same<ns::iterator_t<const immutable_badsized_range&>, CI>);
	CONCEPT_ASSERT(models::Same<ns::sentinel_t<const immutable_badsized_range&>, CI>);
	CONCEPT_ASSERT(models::Range<const immutable_badsized_range&>);
	CONCEPT_ASSERT(!models::SizedRange<const immutable_badsized_range&>);
	//CONCEPT_ASSERT(!models::_ContainerLike<const immutable_badsized_range&>);
	CONCEPT_ASSERT(!models::View<const immutable_badsized_range&>);


	CONCEPT_ASSERT(models::Same<ns::iterator_t<std::vector<int>>, std::vector<int>::iterator>);
	CONCEPT_ASSERT(models::Same<ns::sentinel_t<std::vector<int>>, std::vector<int>::iterator>);
	CONCEPT_ASSERT(models::Range<std::vector<int>>);
	CONCEPT_ASSERT(models::SizedRange<std::vector<int>>);
	//CONCEPT_ASSERT(models::_ContainerLike<std::vector<int>>);
	CONCEPT_ASSERT(!models::View<std::vector<int>>);


	CONCEPT_ASSERT(models::Range<strange_view>);
	CONCEPT_ASSERT(models::Range<strange_view&>);
	CONCEPT_ASSERT(models::View<strange_view>);
	CONCEPT_ASSERT(!models::View<strange_view&>);
	CONCEPT_ASSERT(!models::View<const strange_view>);

	CONCEPT_ASSERT(models::Range<strange_view2>);
	CONCEPT_ASSERT(models::Range<strange_view2&>);
	CONCEPT_ASSERT(models::View<strange_view2>);
	CONCEPT_ASSERT(!models::View<strange_view2&>);
	CONCEPT_ASSERT(!models::View<const strange_view2>);

	CONCEPT_ASSERT(models::Range<strange_view3>);
	CONCEPT_ASSERT(models::Range<strange_view3&>);
	CONCEPT_ASSERT(!models::View<strange_view3>);
	CONCEPT_ASSERT(!models::View<strange_view3&>);
	CONCEPT_ASSERT(!models::View<const strange_view3>);

	CONCEPT_ASSERT(models::Range<mutable_only_unsized_range&>);
	CONCEPT_ASSERT(models::Range<mutable_only_unsized_range>);
	CONCEPT_ASSERT(models::View<mutable_only_unsized_range>);
	CONCEPT_ASSERT(!models::View<mutable_only_unsized_range&>);
	CONCEPT_ASSERT(!models::View<mutable_only_unsized_range&&>);
	CONCEPT_ASSERT(!models::Range<const mutable_only_unsized_range&>);
	CONCEPT_ASSERT(!models::View<const mutable_only_unsized_range&>);
}

template <class I, class S,
	      std::enable_if_t<models::InputIterator<I> && models::Sentinel<S, I>, int> = 0>
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

template <class R, typename = std::enable_if_t<models::Range<R>>>
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
	CONCEPT_ASSERT(models::Range<decltype(some_ints)>);
	CONCEPT_ASSERT(models::SizedRange<decltype(some_ints)>);
	//CONCEPT_ASSERT(models::_ContainerLike<decltype(some_ints)>);
	CONCEPT_ASSERT(!models::View<decltype(some_ints)>);
	CHECK(complicated_algorithm(some_ints) == ns::end(some_ints));
	CONCEPT_ASSERT(models::Range<array_view<int>>);
	CONCEPT_ASSERT(models::SizedRange<array_view<int>>);
	//CONCEPT_ASSERT(!models::_ContainerLike<array_view<int>>);
	CONCEPT_ASSERT(models::View<array_view<int>>);
	CHECK(complicated_algorithm(array_view<int>{some_ints}) == ns::end(some_ints));
}

TEST_CASE("concepts.range")
{
	ridiculously_exhaustive_range_property_test();
	complicated_algorithm_test();

	{
		using T = int[2];
		CONCEPT_ASSERT(models::CommonRange<T>);
		CONCEPT_ASSERT(models::OutputRange<T, int>);
		CONCEPT_ASSERT(models::OutputRange<T, const int&>);
		CONCEPT_ASSERT(models::InputRange<T>);
		CONCEPT_ASSERT(models::ForwardRange<T>);
		CONCEPT_ASSERT(models::BidirectionalRange<T>);
		CONCEPT_ASSERT(models::RandomAccessRange<T>);
	}

	CONCEPT_ASSERT(!models::View<std::vector<int>>);
	CONCEPT_ASSERT(!models::View<std::set<int>>);
	CONCEPT_ASSERT(!models::View<std::multiset<int>>);
	CONCEPT_ASSERT(!models::View<std::unordered_set<int>>);
	CONCEPT_ASSERT(!models::View<std::unordered_multiset<int>>);
}

#endif