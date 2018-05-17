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

//===----------------------------------------------------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is dual licensed under the MIT and the University of Illinois Open
// Source Licenses. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include <stl2/detail/algorithm/generate_n.hpp>
#include <vector>
#include "../simple_test.hpp"
#include "../test_utils.hpp"
#include "../test_iterators.hpp"

namespace stl2 = __stl2;

struct gen_test
{
	int i_;
	gen_test() = default;
	gen_test(int i) : i_(i) {}
	int operator()() {return i_++;}
};

template <class Iter, class Sent = Iter>
void
test()
{
	const unsigned n = 4;
	int ia[n] = {0};
	Iter res = stl2::generate_n(Iter(ia), n, gen_test(1));
	CHECK(ia[0] == 1);
	CHECK(ia[1] == 2);
	CHECK(ia[2] == 3);
	CHECK(ia[3] == 4);
	CHECK(res == Iter(ia + n));
}

void test2()
{
	// Test stl2::generate with a genuine output range
	std::vector<int> v;
	stl2::generate_n(stl2::back_inserter(v), 5, gen_test(1));
	CHECK(v.size() == 5u);
	CHECK(v[0] == 1);
	CHECK(v[1] == 2);
	CHECK(v[2] == 3);
	CHECK(v[3] == 4);
	CHECK(v[4] == 5);
}

int main()
{
	test<forward_iterator<int*> >();
	test<bidirectional_iterator<int*> >();
	test<random_access_iterator<int*> >();
	test<int*>();

	test<forward_iterator<int*>, sentinel<int*> >();
	test<bidirectional_iterator<int*>, sentinel<int*> >();
	test<random_access_iterator<int*>, sentinel<int*> >();

	test2();

	return ::test_result();
}
