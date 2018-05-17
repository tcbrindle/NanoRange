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
//
//  Copyright 2005 - 2007 Adobe Systems Incorporated
//  Distributed under the MIT License(see accompanying file LICENSE_1_0_0.txt
//  or a copy at http://stlab.adobe.com/licenses.html)

//===----------------------------------------------------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is dual licensed under the MIT and the University of Illinois Open
// Source Licenses. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include <stl2/detail/algorithm/remove_copy_if.hpp>
#include <memory>
#include <utility>
#include <functional>
#include "../simple_test.hpp"
#include "../test_utils.hpp"
#include "../test_iterators.hpp"

namespace stl2 = __stl2;

template <class InIter, class OutIter, class Sent = InIter>
void
test_iter()
{
	int ia[] = {0, 1, 2, 3, 4, 2, 3, 4, 2};
	constexpr unsigned sa = stl2::size(ia);
	int ib[sa];
	std::pair<InIter, OutIter> r = stl2::remove_copy_if(InIter(ia), Sent(ia+sa), OutIter(ib), [](int i){return i == 2;});
	CHECK(base(r.first) == ia + sa);
	CHECK(base(r.second) == ib + sa-3);
	CHECK(ib[0] == 0);
	CHECK(ib[1] == 1);
	CHECK(ib[2] == 3);
	CHECK(ib[3] == 4);
	CHECK(ib[4] == 3);
	CHECK(ib[5] == 4);
}

template <class InIter, class OutIter, class Sent = InIter>
void
test_range()
{
	int ia[] = {0, 1, 2, 3, 4, 2, 3, 4, 2};
	constexpr unsigned sa = stl2::size(ia);
	int ib[sa];
	std::pair<InIter, OutIter> r = stl2::remove_copy_if(::as_lvalue(stl2::ext::make_range(InIter(ia), Sent(ia+sa))), OutIter(ib), [](int i){return i == 2;});
	CHECK(base(r.first) == ia + sa);
	CHECK(base(r.second) == ib + sa-3);
	CHECK(ib[0] == 0);
	CHECK(ib[1] == 1);
	CHECK(ib[2] == 3);
	CHECK(ib[3] == 4);
	CHECK(ib[4] == 3);
	CHECK(ib[5] == 4);
}

template <class InIter, class OutIter, class Sent = InIter>
void
test()
{
	test_iter<InIter, OutIter, Sent>();
	test_range<InIter, OutIter, Sent>();
}

struct S
{
	int i;
};

int main()
{
	test<input_iterator<const int*>, output_iterator<int*>>();
	test<input_iterator<const int*>, forward_iterator<int*>>();
	test<input_iterator<const int*>, bidirectional_iterator<int*>>();
	test<input_iterator<const int*>, random_access_iterator<int*>>();
	test<input_iterator<const int*>, int*>();

	test<forward_iterator<const int*>, output_iterator<int*>>();
	test<forward_iterator<const int*>, forward_iterator<int*>>();
	test<forward_iterator<const int*>, bidirectional_iterator<int*>>();
	test<forward_iterator<const int*>, random_access_iterator<int*>>();
	test<forward_iterator<const int*>, int*>();

	test<bidirectional_iterator<const int*>, output_iterator<int*>>();
	test<bidirectional_iterator<const int*>, forward_iterator<int*>>();
	test<bidirectional_iterator<const int*>, bidirectional_iterator<int*>>();
	test<bidirectional_iterator<const int*>, random_access_iterator<int*>>();
	test<bidirectional_iterator<const int*>, int*>();

	test<random_access_iterator<const int*>, output_iterator<int*>>();
	test<random_access_iterator<const int*>, forward_iterator<int*>>();
	test<random_access_iterator<const int*>, bidirectional_iterator<int*>>();
	test<random_access_iterator<const int*>, random_access_iterator<int*>>();
	test<random_access_iterator<const int*>, int*>();

	test<const int*, output_iterator<int*>>();
	test<const int*, forward_iterator<int*>>();
	test<const int*, bidirectional_iterator<int*>>();
	test<const int*, random_access_iterator<int*>>();
	test<const int*, int*>();

	test<input_iterator<const int*>, output_iterator<int*>, sentinel<const int*>>();
	test<input_iterator<const int*>, forward_iterator<int*>, sentinel<const int*>>();
	test<input_iterator<const int*>, bidirectional_iterator<int*>, sentinel<const int*>>();
	test<input_iterator<const int*>, random_access_iterator<int*>, sentinel<const int*>>();
	test<input_iterator<const int*>, int*, sentinel<const int*>>();

	test<forward_iterator<const int*>, output_iterator<int*>, sentinel<const int*>>();
	test<forward_iterator<const int*>, forward_iterator<int*>, sentinel<const int*>>();
	test<forward_iterator<const int*>, bidirectional_iterator<int*>, sentinel<const int*>>();
	test<forward_iterator<const int*>, random_access_iterator<int*>, sentinel<const int*>>();
	test<forward_iterator<const int*>, int*, sentinel<const int*>>();

	test<bidirectional_iterator<const int*>, output_iterator<int*>, sentinel<const int*>>();
	test<bidirectional_iterator<const int*>, forward_iterator<int*>, sentinel<const int*>>();
	test<bidirectional_iterator<const int*>, bidirectional_iterator<int*>, sentinel<const int*>>();
	test<bidirectional_iterator<const int*>, random_access_iterator<int*>, sentinel<const int*>>();
	test<bidirectional_iterator<const int*>, int*, sentinel<const int*>>();

	test<random_access_iterator<const int*>, output_iterator<int*>, sentinel<const int*>>();
	test<random_access_iterator<const int*>, forward_iterator<int*>, sentinel<const int*>>();
	test<random_access_iterator<const int*>, bidirectional_iterator<int*>, sentinel<const int*>>();
	test<random_access_iterator<const int*>, random_access_iterator<int*>, sentinel<const int*>>();
	test<random_access_iterator<const int*>, int*, sentinel<const int*>>();

	// Check projection
	{
		S ia[] = {S{0}, S{1}, S{2}, S{3}, S{4}, S{2}, S{3}, S{4}, S{2}};
		constexpr unsigned sa = stl2::size(ia);
		S ib[sa];
		std::pair<S*, S*> r = stl2::remove_copy_if(ia, ib, [](int i){return i == 2;}, &S::i);
		CHECK(r.first == ia + sa);
		CHECK(r.second == ib + sa-3);
		CHECK(ib[0].i == 0);
		CHECK(ib[1].i == 1);
		CHECK(ib[2].i == 3);
		CHECK(ib[3].i == 4);
		CHECK(ib[4].i == 3);
		CHECK(ib[5].i == 4);
	}

	// Check rvalue range
	{
		S ia[] = {S{0}, S{1}, S{2}, S{3}, S{4}, S{2}, S{3}, S{4}, S{2}};
		constexpr unsigned sa = stl2::size(ia);
		S ib[sa];
		auto r = stl2::remove_copy_if(stl2::move(ia), ib, [](int i){return i == 2;}, &S::i);
		CHECK(r.first.get_unsafe() == ia + sa);
		CHECK(r.second == ib + sa-3);
		CHECK(ib[0].i == 0);
		CHECK(ib[1].i == 1);
		CHECK(ib[2].i == 3);
		CHECK(ib[3].i == 4);
		CHECK(ib[4].i == 3);
		CHECK(ib[5].i == 4);
	}

	return ::test_result();
}
