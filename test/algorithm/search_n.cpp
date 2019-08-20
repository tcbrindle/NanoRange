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

#include <nanorange/algorithm/search_n.hpp>
#include <nanorange/iterator/counted_iterator.hpp>
#include <nanorange/view/subrange.hpp>
#include <array>
#include "../catch.hpp"
#include "../test_iterators.hpp"
#include "../test_utils.hpp"

namespace stl2 = nano;

namespace {

template <typename T>
T& as_lvalue(T&& t)
{
	return t;
}

template<class Iter, typename Sent = Iter>
void
test_iter_impl() {
	int ia[] = {0, 1, 2, 3, 4, 5};
	const unsigned sa = sizeof(ia) / sizeof(ia[0]);
	CHECK(stl2::search_n(Iter(ia), Sent(ia + sa), 0, 0).begin() == Iter(ia));
	CHECK(stl2::search_n(Iter(ia), Sent(ia + sa), 1, 0).begin() == Iter(ia + 0));
	CHECK(stl2::search_n(Iter(ia), Sent(ia + sa), 2, 0).begin() == Iter(ia + sa));
	CHECK(stl2::search_n(Iter(ia), Sent(ia + sa), sa, 0).begin() == Iter(ia + sa));
	CHECK(stl2::search_n(Iter(ia), Sent(ia + sa), 0, 3).begin() == Iter(ia));
	CHECK(stl2::search_n(Iter(ia), Sent(ia + sa), 1, 3).begin() == Iter(ia + 3));
	CHECK(stl2::search_n(Iter(ia), Sent(ia + sa), 2, 3).begin() == Iter(ia + sa));
	CHECK(stl2::search_n(Iter(ia), Sent(ia + sa), sa, 3).begin() == Iter(ia + sa));
	CHECK(stl2::search_n(Iter(ia), Sent(ia + sa), 0, 5).begin() == Iter(ia));
	CHECK(stl2::search_n(Iter(ia), Sent(ia + sa), 1, 5).begin() == Iter(ia + 5));
	CHECK(stl2::search_n(Iter(ia), Sent(ia + sa), 2, 5).begin() == Iter(ia + sa));
	CHECK(stl2::search_n(Iter(ia), Sent(ia + sa), sa, 5).begin() == Iter(ia + sa));

	int ib[] = {0, 0, 1, 1, 2, 2};
	const unsigned sb = sizeof(ib) / sizeof(ib[0]);
	CHECK(stl2::search_n(Iter(ib), Sent(ib + sb), 0, 0).begin() == Iter(ib));
	CHECK(stl2::search_n(Iter(ib), Sent(ib + sb), 1, 0).begin() == Iter(ib + 0));
	CHECK(stl2::search_n(Iter(ib), Sent(ib + sb), 2, 0).begin() == Iter(ib + 0));
	CHECK(stl2::search_n(Iter(ib), Sent(ib + sb), 3, 0).begin() == Iter(ib + sb));
	CHECK(stl2::search_n(Iter(ib), Sent(ib + sb), sb, 0).begin() == Iter(ib + sb));
	CHECK(stl2::search_n(Iter(ib), Sent(ib + sb), 0, 1).begin() == Iter(ib));
	CHECK(stl2::search_n(Iter(ib), Sent(ib + sb), 1, 1).begin() == Iter(ib + 2));
	CHECK(stl2::search_n(Iter(ib), Sent(ib + sb), 2, 1).begin() == Iter(ib + 2));
	CHECK(stl2::search_n(Iter(ib), Sent(ib + sb), 3, 1).begin() == Iter(ib + sb));
	CHECK(stl2::search_n(Iter(ib), Sent(ib + sb), sb, 1).begin() == Iter(ib + sb));
	CHECK(stl2::search_n(Iter(ib), Sent(ib + sb), 0, 2).begin() == Iter(ib));
	CHECK(stl2::search_n(Iter(ib), Sent(ib + sb), 1, 2).begin() == Iter(ib + 4));
	CHECK(stl2::search_n(Iter(ib), Sent(ib + sb), 2, 2).begin() == Iter(ib + 4));
	CHECK(stl2::search_n(Iter(ib), Sent(ib + sb), 3, 2).begin() == Iter(ib + sb));
	CHECK(stl2::search_n(Iter(ib), Sent(ib + sb), sb, 2).begin() == Iter(ib + sb));

	int ic[] = {0, 0, 0};
	const unsigned sc = sizeof(ic) / sizeof(ic[0]);
	CHECK(stl2::search_n(Iter(ic), Sent(ic + sc), 0, 0).begin() == Iter(ic));
	CHECK(stl2::search_n(Iter(ic), Sent(ic + sc), 1, 0).begin() == Iter(ic));
	CHECK(stl2::search_n(Iter(ic), Sent(ic + sc), 2, 0).begin() == Iter(ic));
	CHECK(stl2::search_n(Iter(ic), Sent(ic + sc), 3, 0).begin() == Iter(ic));
	CHECK(stl2::search_n(Iter(ic), Sent(ic + sc), 4, 0).begin() == Iter(ic + sc));
}

template<class Iter, class Iter2>
void
test_iter() {
	using Sent = typename sentinel_type<Iter>::type;
	test_iter_impl<Iter>();
	test_iter_impl<Iter, Sent>();

	using SizedSent1 = typename sentinel_type<Iter, true>::type;
	test_iter_impl<Iter, SizedSent1>();
}

template<class Iter, typename Sent = Iter>
void
test_range_impl() {
	int ia[] = {0, 1, 2, 3, 4, 5};
	const unsigned sa = sizeof(ia) / sizeof(ia[0]);
	CHECK(stl2::search_n(::as_lvalue(stl2::subrange(Iter(ia), Sent(ia + sa))), 0, 0).begin() == Iter(ia));
	CHECK(stl2::search_n(::as_lvalue(stl2::subrange(Iter(ia), Sent(ia + sa))), 1, 0).begin() == Iter(ia + 0));
	CHECK(stl2::search_n(::as_lvalue(stl2::subrange(Iter(ia), Sent(ia + sa))), 2, 0).begin() == Iter(ia + sa));
	CHECK(stl2::search_n(::as_lvalue(stl2::subrange(Iter(ia), Sent(ia + sa))), sa, 0).begin() == Iter(ia + sa));
	CHECK(stl2::search_n(::as_lvalue(stl2::subrange(Iter(ia), Sent(ia + sa))), 0, 3).begin() == Iter(ia));
	CHECK(stl2::search_n(::as_lvalue(stl2::subrange(Iter(ia), Sent(ia + sa))), 1, 3).begin() == Iter(ia + 3));
	CHECK(stl2::search_n(::as_lvalue(stl2::subrange(Iter(ia), Sent(ia + sa))), 2, 3).begin() == Iter(ia + sa));
	CHECK(stl2::search_n(::as_lvalue(stl2::subrange(Iter(ia), Sent(ia + sa))), sa, 3).begin() == Iter(ia + sa));
	CHECK(stl2::search_n(::as_lvalue(stl2::subrange(Iter(ia), Sent(ia + sa))), 0, 5).begin() == Iter(ia));
	CHECK(stl2::search_n(::as_lvalue(stl2::subrange(Iter(ia), Sent(ia + sa))), 1, 5).begin() == Iter(ia + 5));
	CHECK(stl2::search_n(::as_lvalue(stl2::subrange(Iter(ia), Sent(ia + sa))), 2, 5).begin() == Iter(ia + sa));
	CHECK(stl2::search_n(::as_lvalue(stl2::subrange(Iter(ia), Sent(ia + sa))), sa, 5).begin() == Iter(ia + sa));

	int ib[] = {0, 0, 1, 1, 2, 2};
	const unsigned sb = sizeof(ib) / sizeof(ib[0]);
	CHECK(stl2::search_n(::as_lvalue(stl2::subrange(Iter(ib), Sent(ib + sb))), 0, 0).begin() == Iter(ib));
	CHECK(stl2::search_n(::as_lvalue(stl2::subrange(Iter(ib), Sent(ib + sb))), 1, 0).begin() == Iter(ib + 0));
	CHECK(stl2::search_n(::as_lvalue(stl2::subrange(Iter(ib), Sent(ib + sb))), 2, 0).begin() == Iter(ib + 0));
	CHECK(stl2::search_n(::as_lvalue(stl2::subrange(Iter(ib), Sent(ib + sb))), 3, 0).begin() == Iter(ib + sb));
	CHECK(stl2::search_n(::as_lvalue(stl2::subrange(Iter(ib), Sent(ib + sb))), sb, 0).begin() == Iter(ib + sb));
	CHECK(stl2::search_n(::as_lvalue(stl2::subrange(Iter(ib), Sent(ib + sb))), 0, 1).begin() == Iter(ib));
	CHECK(stl2::search_n(::as_lvalue(stl2::subrange(Iter(ib), Sent(ib + sb))), 1, 1).begin() == Iter(ib + 2));
	CHECK(stl2::search_n(::as_lvalue(stl2::subrange(Iter(ib), Sent(ib + sb))), 2, 1).begin() == Iter(ib + 2));
	CHECK(stl2::search_n(::as_lvalue(stl2::subrange(Iter(ib), Sent(ib + sb))), 3, 1).begin() == Iter(ib + sb));
	CHECK(stl2::search_n(::as_lvalue(stl2::subrange(Iter(ib), Sent(ib + sb))), sb, 1).begin() == Iter(ib + sb));
	CHECK(stl2::search_n(::as_lvalue(stl2::subrange(Iter(ib), Sent(ib + sb))), 0, 2).begin() == Iter(ib));
	CHECK(stl2::search_n(::as_lvalue(stl2::subrange(Iter(ib), Sent(ib + sb))), 1, 2).begin() == Iter(ib + 4));
	CHECK(stl2::search_n(::as_lvalue(stl2::subrange(Iter(ib), Sent(ib + sb))), 2, 2).begin() == Iter(ib + 4));
	CHECK(stl2::search_n(::as_lvalue(stl2::subrange(Iter(ib), Sent(ib + sb))), 3, 2).begin() == Iter(ib + sb));
	CHECK(stl2::search_n(::as_lvalue(stl2::subrange(Iter(ib), Sent(ib + sb))), sb, 2).begin() == Iter(ib + sb));

	int ic[] = {0, 0, 0};
	const unsigned sc = sizeof(ic) / sizeof(ic[0]);
	CHECK(stl2::search_n(::as_lvalue(stl2::subrange(Iter(ic), Sent(ic + sc))), 0, 0).begin() == Iter(ic));
	CHECK(stl2::search_n(::as_lvalue(stl2::subrange(Iter(ic), Sent(ic + sc))), 1, 0).begin() == Iter(ic));
	CHECK(stl2::search_n(::as_lvalue(stl2::subrange(Iter(ic), Sent(ic + sc))), 2, 0).begin() == Iter(ic));
	CHECK(stl2::search_n(::as_lvalue(stl2::subrange(Iter(ic), Sent(ic + sc))), 3, 0).begin() == Iter(ic));
	CHECK(stl2::search_n(::as_lvalue(stl2::subrange(Iter(ic), Sent(ic + sc))), 4, 0).begin() == Iter(ic + sc));
}

template<class Iter, class Iter2>
void
test_range() {
	using Sent = typename sentinel_type<Iter>::type;
	test_range_impl<Iter>();
	test_range_impl<Iter, Sent>();

	using SizedSent1 = typename sentinel_type<Iter, true>::type;
	test_range_impl<Iter, SizedSent1>();
}

template<class Iter, class Iter2>
void
test() {
	test_iter<Iter, Iter2>();
	test_range<Iter, Iter2>();
}

struct S {
	int i;
};

}

TEST_CASE("alg.search_n")
{
	test<forward_iterator<const int*>, forward_iterator<const int*> >();
	test<forward_iterator<const int*>, bidirectional_iterator<const int*> >();
	test<forward_iterator<const int*>, random_access_iterator<const int*> >();
	test<bidirectional_iterator<const int*>, forward_iterator<const int*> >();
	test<bidirectional_iterator<const int*>, bidirectional_iterator<const int*> >();
	test<bidirectional_iterator<const int*>, random_access_iterator<const int*> >();
	test<random_access_iterator<const int*>, forward_iterator<const int*> >();
	test<random_access_iterator<const int*>, bidirectional_iterator<const int*> >();
	test<random_access_iterator<const int*>, random_access_iterator<const int*> >();

	// Test projections:
	{
		S const in[] = {{0}, {1}, {2}, {2}, {4}, {5}};

		S const *p = stl2::search_n(in, 2, 2, std::equal_to<int>{}, &S::i).begin();
		CHECK(p == in+2);
	}

	// Test counted ranges
	{
		int in[] = {0,1,2,2,4,5};
		auto rng = stl2::subrange(
					 stl2::make_counted_iterator(bidirectional_iterator<int*>(in), 6),
					 stl2::default_sentinel);
		auto it = stl2::search_n(rng, 2, 2).begin();
		CHECK(base(it.base()) == in+2);
		CHECK(it.count() == 4);

		auto it2 = stl2::search_n(rng, 3, 2).begin();
		CHECK(base(it2.base()) == in+6);
		CHECK(it2.count() == 0);
	}

	// Test rvalue ranges
	{
		std::array<int, 6> ib = {0, 0, 1, 1, 2, 2};
		auto r = stl2::search_n(std::move(ib), 2, 1);
		static_assert(stl2::Same<decltype(r), stl2::dangling>);
	}
}
