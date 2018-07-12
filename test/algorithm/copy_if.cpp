// cmcstl2 - A concept-enabled C++ standard library
//
//  Copyright Casey Carter 2015
//
//  Use, modification and distribution is subject to the
//  Boost Software License, Version 1.0. (See accompanying
//  file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt)
//
// Project home: https://github.com/caseycarter/cmcstl2
//
#include <nanorange/algorithm/copy.hpp>
#include "../catch.hpp"

namespace ranges = nano;

TEST_CASE("alg.copy_if") {
	static const int source[] = {5,4,3,2,1,0};
	static constexpr std::ptrdiff_t n = sizeof(source)/sizeof(source[0]);

	static const int evens[] = {4,2,0};
	static_assert(sizeof(evens) / sizeof(evens[0]) == n / 2, "");
	auto is_even = [](int i){
		return i % 2 == 0;
	};


	{
		int target[n];
		std::fill_n(target, n, -1);

		static const int evens[] = {4,2,0};
		auto is_even = [](int i){
			return i % 2 == 0;
		};

		auto res = ranges::copy_if(source, source + n, target, is_even);
		REQUIRE(res.in == source + n);
		REQUIRE(res.out == target + n / 2);

		REQUIRE(std::equal(target, target + n / 2, evens));
		REQUIRE(std::count(target + n / 2, target + n, -1) == n / 2);
	}

	{
		int target[n];
		std::fill_n(target, n, -1);

		auto res = ranges::copy_if(source, target, is_even);
		REQUIRE(res.in == source + n);
		REQUIRE(res.out == target + n / 2);

		REQUIRE(std::equal(target, target + n / 2, evens));
		REQUIRE(std::count(target + n / 2, target + n, -1) == n / 2);
	}

	{
		int target[n];
		std::fill_n(target, n, -1);

		auto res = ranges::copy_if(source, target, is_even);
		REQUIRE(res.in == source + n);
		REQUIRE(res.out == target + n / 2);

		REQUIRE(std::equal(target, target + n / 2, evens));
		REQUIRE(std::count(target + n / 2, target + n, -1) == n / 2);
	}

    {
		struct S { int value; };
		S source[n];
		for (auto i = n; i-- > 0;) {
			source[i].value = i;
		}
		S target[n];
		for (auto i = n; i-- > 0;) {
			target[i].value = -1;
		}

		auto res = ranges::copy_if(source, target, is_even, &S::value);
		//REQUIRE(res.in() == source + n);
		REQUIRE(res.in == source + n);
		//REQUIRE(res.out() == target + n / 2);
        REQUIRE(res.out == target + n / 2);

		for (auto i = n / 2; i-- > 0;) {
			REQUIRE(target[i].value == source[2 * i].value);
		}

		REQUIRE(std::count_if(target + n / 2, target + n, [](const S& s){
					return s.value == -1; }) == n / 2);
    }


    {
		int target[n];
		std::fill_n(target, n, -1);

		{
			auto l = {5,4,3,2,1,0};
			auto res = ranges::copy_if(l, target, is_even);
			REQUIRE(res.out == target + n / 2);
		}

		REQUIRE(std::equal(target, target + n / 2, evens));
		REQUIRE(std::count(target + n / 2, target + n, -1) == n / 2);
	}
}
