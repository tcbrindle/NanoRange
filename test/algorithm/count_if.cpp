// Range v3 library
//
//  Copyright Eric Niebler 2014
//
//  Use, modification and distribution is subject to the
//  Boost Software License, Version 1.0. (See accompanying
//  file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt)
//
// Project home: https://github.com/ericniebler/range-v3

#include <nanorange/algorithm/count.hpp>
#include "../catch.hpp"
#include "../test_iterators.hpp"
#include "../test_utils.hpp"

namespace {

struct S {
	int i;
};

struct T {
	bool b;

	bool m() { return b; }
};

}

TEST_CASE("alg.count_if")
{
	using namespace nano;
	using nano::make_range;
	auto equals = [](auto&& i){
	  return [i = std::forward<decltype(i)>(i)](const auto& j) {
		return i == j;
	  };
	};

	int ia[] = {0, 1, 2, 2, 0, 1, 2, 3};
	constexpr unsigned cia = size(ia);

	CHECK(count_if(input_iterator<const int*>(ia),
				   sentinel<const int*>(ia + cia), equals(2)) == 3);
	CHECK(count_if(input_iterator<const int*>(ia),
				   sentinel<const int*>(ia + cia), equals(7)) == 0);
	CHECK(count_if(input_iterator<const int*>(ia),
				   sentinel<const int*>(ia), equals(2)) == 0);

	CHECK(count_if(make_range(input_iterator<const int*>(ia),
						 sentinel<const int*>(ia + cia)), equals(2)) == 3);
	CHECK(count_if(make_range(input_iterator<const int*>(ia),
						 sentinel<const int*>(ia + cia)), equals(7)) == 0);
	CHECK(count_if(make_range(input_iterator<const int*>(ia),
						 sentinel<const int*>(ia)), equals(2)) == 0);

	S sa[] = {{0}, {1}, {2}, {2}, {0}, {1}, {2}, {3}};
	constexpr unsigned csa = size(ia);

	CHECK(count_if(input_iterator<const S*>(sa),
				   sentinel<const S*>(sa + csa), equals(2), &S::i) == 3);
	CHECK(count_if(input_iterator<const S*>(sa),
				   sentinel<const S*>(sa + csa), equals(7), &S::i) == 0);
	CHECK(count_if(input_iterator<const S*>(sa),
				   sentinel<const S*>(sa), equals(2), &S::i) == 0);

	CHECK(count_if(make_range(input_iterator<const S*>(sa),
						 sentinel<const S*>(sa + csa)), equals(2), &S::i) == 3);
	CHECK(count_if(make_range(input_iterator<const S*>(sa),
						 sentinel<const S*>(sa + csa)), equals(7), &S::i) == 0);
	CHECK(count_if(make_range(input_iterator<const S*>(sa),
						 sentinel<const S*>(sa)), equals(2), &S::i) == 0);

	T ta[] = {{true}, {false}, {true}, {false}, {false}, {true}, {false}, {false}, {true}, {false}};
	CHECK(count_if(input_iterator<T*>(ta),
				   sentinel<T*>(ta + size(ta)), &T::m) == 4);
	CHECK(count_if(input_iterator<T*>(ta),
				   sentinel<T*>(ta + size(ta)), &T::b) == 4);
	CHECK(count_if(make_range(input_iterator<T*>(ta),
						 sentinel<T*>(ta + size(ta))), &T::m) == 4);
	CHECK(count_if(make_range(input_iterator<T*>(ta),
						 sentinel<T*>(ta + size(ta))), &T::b) == 4);

	{
		auto l = {0, 1, 2, 2, 0, 1, 2, 3};
		CHECK(count_if(std::move(l), equals(2)) == 3);
	}
	{
		auto l = {0, 1, 2, 2, 0, 1, 2, 3};
		CHECK(count_if(std::move(l), equals(42)) == 0);
	}
}
