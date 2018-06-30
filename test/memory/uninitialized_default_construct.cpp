// cmcstl2 - A concept-enabled C++ standard library
//
//  Copyright Casey Carter 2016
//  Copyright Christopher Di Bella 2016
//
//  Use, modification and distribution is subject to the
//  Boost Software License, Version 1.0. (See accompanying
//  file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt)
//
// Project home: https://github.com/caseycarter/cmcstl2
//
#include <stl2/detail/memory/uninitialized_default_construct.hpp>
#include <stl2/concepts.hpp>
#include <stl2/detail/algorithm/find_if.hpp>
#include <stl2/detail/memory/destroy.hpp>
#include <cstdint>
#include <cstring>
#include <deque>
#include <iostream>
#include <list>
#include <string>
#include <vector>
#include "../simple_test.hpp"
#include "common.hpp"

namespace ranges = __stl2;

namespace {
	constexpr int N = 1 << 12;

	template <typename T>
	requires ranges::DefaultConstructible<T> && ranges::EqualityComparable<T>
	void test(const raw_buffer<T>& independent, ranges::iterator_t<const raw_buffer<T>> p)
	{
		T t{};
		CHECK(p == independent.cend());
		CHECK(ranges::find_if(independent.begin(), p, [&t](const T& i){ return i != t; }) == p);
		ranges::destroy(independent.begin(), p);
	}

	template <typename T>
	requires
		ranges::DefaultConstructible<T> &&
		ranges::EqualityComparable<T>
	void uninitialized_default_construct_test()
	{
		auto independent = make_buffer<T>(N);

		test(independent, ranges::uninitialized_default_construct(independent.begin(), independent.end()));
		test(independent, ranges::uninitialized_default_construct(independent.cbegin(), independent.cend()));
		test(independent, ranges::uninitialized_default_construct(independent));
		test(independent, ranges::uninitialized_default_construct_n(independent.begin(), independent.size()));
		test(independent, ranges::uninitialized_default_construct_n(independent.cbegin(), independent.size()));
	}

	template <typename T>
	requires
		ranges::DefaultConstructible<T> &&
		ranges::EqualityComparable<T> &&
		std::is_fundamental<T>::value
	void test(const raw_buffer<T>& independent, ranges::iterator_t<const raw_buffer<T>> p)
	{
		T t;
		std::memset(&t, 0xCC, sizeof(T));
		CHECK(p == independent.cend());
		CHECK(ranges::find_if(independent.begin(), p, [&t](const T& i){ return i != t; }) == p);
		ranges::destroy(independent.begin(), p);
	}

	template <typename T>
	requires
		ranges::DefaultConstructible<T> &&
		ranges::EqualityComparable<T> &&
		std::is_fundamental<T>::value
	void uninitialized_default_construct_test()
	{
		auto independent = make_buffer<T>(N);

		std::memset(independent.begin(), 0xCC, independent.size() * sizeof(T));

		test(independent, ranges::uninitialized_default_construct(independent.begin(), independent.end()));
		test(independent, ranges::uninitialized_default_construct(independent));
		test(independent, ranges::uninitialized_default_construct_n(independent.begin(), independent.size()));
	}

	struct S {
		static constexpr int throw_after = 42;
		static int count;

		static void increment() {
			if (++count >= throw_after) {
				throw exception{};
			}
		}

		struct exception {};

		S() { increment(); }
	};
	constexpr int S::throw_after;
	int S::count;

	void throw_test() {
		constexpr int n = 2 * S::throw_after;
		auto independent = make_buffer<S>(n);
		S::count = 0;
		try {
			ranges::uninitialized_default_construct_n(independent.begin(), n);
			CHECK(false);
		} catch(S::exception&) {
			CHECK(S::count == S::throw_after);
		}
		S::count = 0;

		try {
			ranges::uninitialized_default_construct(independent);
			CHECK(false);
		} catch(S::exception&) {
			CHECK(S::count == S::throw_after);
		}
		S::count = 0;
	}
}

int main()
{
	uninitialized_default_construct_test<char>();
	uninitialized_default_construct_test<int>();
	uninitialized_default_construct_test<long long>();
	uninitialized_default_construct_test<float>();
	uninitialized_default_construct_test<double>();
	uninitialized_default_construct_test<std::vector<char>>();
	uninitialized_default_construct_test<std::string>();
	uninitialized_default_construct_test<std::deque<double>>();
	uninitialized_default_construct_test<std::list<std::vector<std::deque<double>>>>();
	uninitialized_default_construct_test<std::unique_ptr<std::string>>();

	throw_test();

	return ::test_result();
}
