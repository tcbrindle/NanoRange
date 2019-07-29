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

#include <nanorange/algorithm/search.hpp>
#include <nanorange/iterator/counted_iterator.hpp>
#include <initializer_list>
#include "../catch.hpp"
#include "../test_iterators.hpp"
#include "../test_utils.hpp"

namespace stl2 = nano;

namespace {

// FIXME: Properly update tests for subrange return type

template <typename T>
T& as_lvalue(T&& t)
{
	return t;
}

template <class Iter1, class Iter2, typename Sent1 = Iter1, typename Sent2 = Iter2>
void
test_iter_impl()
{
	int ia[] = {0, 1, 2, 3, 4, 5};
	const unsigned sa = sizeof(ia) / sizeof(ia[0]);
	CHECK(stl2::search(Iter1(ia), Sent1(ia + sa), Iter2(ia), Sent2(ia)).begin()
				  == Iter1(ia));
	CHECK(stl2::search(Iter1(ia), Sent1(ia + sa), Iter2(ia), Sent2(ia + 1)).begin()
				  == Iter1(ia));
	CHECK(stl2::search(Iter1(ia), Sent1(ia + sa), Iter2(ia + 1), Sent2(ia + 2)).begin()
				  == Iter1(ia + 1));
	CHECK(stl2::search(Iter1(ia), Sent1(ia + sa), Iter2(ia + 2), Sent2(ia + 2)).begin()
				  == Iter1(ia));
	CHECK(stl2::search(Iter1(ia), Sent1(ia + sa), Iter2(ia + 2), Sent2(ia + 3)).begin()
				  == Iter1(ia + 2));
	CHECK(stl2::search(Iter1(ia), Sent1(ia + sa), Iter2(ia + 2), Sent2(ia + 3)).begin()
				  == Iter1(ia + 2));
	CHECK(stl2::search(Iter1(ia), Sent1(ia), Iter2(ia + 2), Sent2(ia + 3)).begin()
				  == Iter1(ia));
	CHECK(stl2::search(Iter1(ia), Sent1(ia + sa), Iter2(ia + sa - 1),
					   Sent2(ia + sa)).begin() == Iter1(ia + sa - 1));
	CHECK(stl2::search(Iter1(ia), Sent1(ia + sa), Iter2(ia + sa - 3),
					   Sent2(ia + sa)).begin() == Iter1(ia + sa - 3));
	CHECK(stl2::search(Iter1(ia), Sent1(ia + sa), Iter2(ia), Sent2(ia + sa)).begin()
				  == Iter1(ia));
	CHECK(stl2::search(Iter1(ia), Sent1(ia + sa - 1), Iter2(ia), Sent2(ia + sa)).begin()
				  == Iter1(ia + sa - 1));
	CHECK(stl2::search(Iter1(ia), Sent1(ia + 1), Iter2(ia), Sent2(ia + sa)).begin()
				  == Iter1(ia + 1));
	int ib[] = {0, 1, 2, 0, 1, 2, 3, 0, 1, 2, 3, 4};
	const unsigned sb = sizeof(ib) / sizeof(ib[0]);
	int ic[] = {1};
	CHECK(stl2::search(Iter1(ib), Sent1(ib + sb), Iter2(ic), Sent2(ic + 1)).begin()
				  == Iter1(ib + 1));
	int id[] = {1, 2};
	CHECK(stl2::search(Iter1(ib), Sent1(ib + sb), Iter2(id), Sent2(id + 2)).begin()
				  == Iter1(ib + 1));
	int ie[] = {1, 2, 3};
	CHECK(stl2::search(Iter1(ib), Sent1(ib + sb), Iter2(ie), Sent2(ie + 3)).begin()
				  == Iter1(ib + 4));
	int ig[] = {1, 2, 3, 4};
	CHECK(stl2::search(Iter1(ib), Sent1(ib + sb), Iter2(ig), Sent2(ig + 4)).begin()
				  == Iter1(ib + 8));
	int ih[] = {0, 1, 1, 1, 1, 2, 3, 0, 1, 2, 3, 4};
	const unsigned sh = sizeof(ih) / sizeof(ih[0]);
	int ii[] = {1, 1, 2};
	CHECK(stl2::search(Iter1(ih), Sent1(ih + sh), Iter2(ii), Sent2(ii + 3)).begin()
				  == Iter1(ih + 3));
	int ij[] = {0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 1, 1, 1, 1, 0, 0};
	const unsigned sj = sizeof(ij) / sizeof(ij[0]);
	int ik[] = {0, 0, 0, 0, 1, 1, 1, 1, 0, 0};
	const unsigned sk = sizeof(ik) / sizeof(ik[0]);
	CHECK(stl2::search(Iter1(ij), Sent1(ij + sj), Iter2(ik), Sent2(ik + sk)).begin()
				  == Iter1(ij + 6));
}

template <class Iter1, class Iter2>
void
test_iter()
{
	using Sent1 = typename sentinel_type<Iter1>::type;
	using Sent2 = typename sentinel_type<Iter2>::type;
	test_iter_impl<Iter1, Iter2>();
	test_iter_impl<Iter1, Iter2, Sent1>();
	test_iter_impl<Iter1, Iter2, Iter1, Sent2>();
	test_iter_impl<Iter1, Iter2, Sent1, Sent2>();

	using SizedSent1 = typename sentinel_type<Iter1, true>::type;
	using SizedSent2 = typename sentinel_type<Iter2, true>::type;
	test_iter_impl<Iter1, Iter2, SizedSent1, SizedSent2>();
}

template <class Iter1, class Iter2, typename Sent1 = Iter1, typename Sent2 = Iter2>
void
test_range_impl()
{
	int ia[] = {0, 1, 2, 3, 4, 5};
	const unsigned sa = sizeof(ia) / sizeof(ia[0]);
	CHECK(stl2::search(::as_lvalue(stl2::subrange(Iter1(ia), Sent1(ia
																				  + sa))), stl2::subrange(
			Iter2(ia), Sent2(ia))).begin() ==Iter1(ia));
	CHECK(stl2::search(::as_lvalue(stl2::subrange(Iter1(ia), Sent1(ia
																				  + sa))), stl2::subrange(
			Iter2(ia), Sent2(ia + 1))).begin() ==Iter1(ia));
	CHECK(stl2::search(::as_lvalue(stl2::subrange(Iter1(ia), Sent1(ia
																				  + sa))), stl2::subrange(
			Iter2(ia + 1), Sent2(ia + 2))).begin() ==Iter1(ia+1));
	CHECK(stl2::search(::as_lvalue(stl2::subrange(Iter1(ia), Sent1(ia
																				  + sa))), stl2::subrange(
			Iter2(ia + 2), Sent2(ia + 2))).begin() ==Iter1(ia));
	CHECK(stl2::search(::as_lvalue(stl2::subrange(Iter1(ia), Sent1(ia
																				  + sa))), stl2::subrange(
			Iter2(ia + 2), Sent2(ia + 3))).begin() ==Iter1(ia+2));
	CHECK(stl2::search(::as_lvalue(stl2::subrange(Iter1(ia), Sent1(ia
																				  + sa))), stl2::subrange(
			Iter2(ia + 2), Sent2(ia + 3))).begin() ==Iter1(ia+2));
	CHECK(stl2::search(::as_lvalue(
			stl2::subrange(Iter1(ia), Sent1(ia))), stl2::subrange(
			Iter2(ia + 2), Sent2(ia + 3))).begin() ==Iter1(ia));
	CHECK(stl2::search(::as_lvalue(stl2::subrange(Iter1(ia), Sent1(ia
																				  + sa))), stl2::subrange(
			Iter2(ia + sa - 1), Sent2(ia + sa))).begin() ==Iter1(ia+sa-1));
	CHECK(stl2::search(::as_lvalue(stl2::subrange(Iter1(ia), Sent1(ia
																				  + sa))), stl2::subrange(
			Iter2(ia + sa - 3), Sent2(ia + sa))).begin() ==Iter1(ia+sa-3));
	CHECK(stl2::search(::as_lvalue(stl2::subrange(Iter1(ia), Sent1(ia
																				  + sa))), stl2::subrange(
			Iter2(ia), Sent2(ia + sa))).begin() ==Iter1(ia));
	CHECK(stl2::search(::as_lvalue(stl2::subrange(Iter1(ia),
														 Sent1(ia + sa
																	   - 1))), stl2::subrange(
			Iter2(ia), Sent2(ia + sa))).begin() ==Iter1(ia+sa-1));
	CHECK(stl2::search(::as_lvalue(stl2::subrange(Iter1(ia), Sent1(ia
																				  + 1))), stl2::subrange(
			Iter2(ia), Sent2(ia + sa))).begin() ==Iter1(ia+1));
	int ib[] = {0, 1, 2, 0, 1, 2, 3, 0, 1, 2, 3, 4};
	const unsigned sb = sizeof(ib) / sizeof(ib[0]);
	int ic[] = {1};
	CHECK(stl2::search(::as_lvalue(stl2::subrange(Iter1(ib), Sent1(ib
																				  + sb))), stl2::subrange(
			Iter2(ic), Sent2(ic + 1))).begin() ==Iter1(ib+1));
	int id[] = {1, 2};
	CHECK(stl2::search(::as_lvalue(stl2::subrange(Iter1(ib), Sent1(ib
																				  + sb))), stl2::subrange(
			Iter2(id), Sent2(id + 2))).begin() ==Iter1(ib+1));
	int ie[] = {1, 2, 3};
	CHECK(stl2::search(::as_lvalue(stl2::subrange(Iter1(ib), Sent1(ib
																				  + sb))), stl2::subrange(
			Iter2(ie), Sent2(ie + 3))).begin() ==Iter1(ib+4));
	int ig[] = {1, 2, 3, 4};
	CHECK(stl2::search(::as_lvalue(stl2::subrange(Iter1(ib), Sent1(ib
																				  + sb))), stl2::subrange(
			Iter2(ig), Sent2(ig + 4))).begin() ==Iter1(ib+8));
	int ih[] = {0, 1, 1, 1, 1, 2, 3, 0, 1, 2, 3, 4};
	const unsigned sh = sizeof(ih) / sizeof(ih[0]);
	int ii[] = {1, 1, 2};
	CHECK(stl2::search(::as_lvalue(stl2::subrange(Iter1(ih), Sent1(ih
																				  + sh))), stl2::subrange(
			Iter2(ii), Sent2(ii + 3))).begin() ==Iter1(ih+3));
	int ij[] = {0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 1, 1, 1, 1, 0, 0};
	const unsigned sj = sizeof(ij) / sizeof(ij[0]);
	int ik[] = {0, 0, 0, 0, 1, 1, 1, 1, 0, 0};
	const unsigned sk = sizeof(ik) / sizeof(ik[0]);
	CHECK(stl2::search(::as_lvalue(stl2::subrange(Iter1(ij), Sent1(ij
																				  + sj))), stl2::subrange(
			Iter2(ik), Sent2(ik + sk))).begin() ==Iter1(ij+6));
}

template <class Iter1, class Iter2>
void
test_range()
{
	using Sent1 = typename sentinel_type<Iter1>::type;
	using Sent2 = typename sentinel_type<Iter2>::type;
	test_range_impl<Iter1, Iter2>();
	test_range_impl<Iter1, Iter2, Sent1>();
	test_range_impl<Iter1, Iter2, Iter1, Sent2>();
	test_range_impl<Iter1, Iter2, Sent1, Sent2>();

	using SizedSent1 = typename sentinel_type<Iter1, true>::type;
	using SizedSent2 = typename sentinel_type<Iter2, true>::type;
	test_range_impl<Iter1, Iter2, SizedSent1, SizedSent2>();
}

template <class Iter1, class Iter2>
void
test()
{
	test_iter<Iter1, Iter2>();
	test_range<Iter1, Iter2>();
}

struct S {
	int i;
};

struct T {
	int i;
};

}

TEST_CASE("alg.search")
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
		S const in[] = {{0}, {1}, {2}, {3}, {4}, {5}};
		T const pat[] = {{2}, {3}};
		S const *p = stl2::search(in, pat, stl2::equal_to<>{}, &S::i, &T::i).begin();
		CHECK(p == in+2);
	}

	// Test counted ranges
	{
		int in[] = {0,1,2,3,4,5};
		auto rng = stl2::subrange(
					 stl2::make_counted_iterator(
					   bidirectional_iterator<int*>(in), 6),
					 stl2::default_sentinel);
		auto it = stl2::search(rng, std::initializer_list<int>{2,3}).begin();
		CHECK(base(it.base()) == in+2);
		CHECK(it.count() == 4);

		it = stl2::search(rng, std::initializer_list<int>{5,6}).begin();
		CHECK(base(it.base()) == in+6);
		CHECK(it.count() == 0);
	}

#ifdef HAVE_RVALUE_RANGES
	// Test rvalue ranges
	{
		int ib[] = {0, 1, 2, 0, 1, 2, 3, 0, 1, 2, 3, 4};
		int ie[] = {1, 2, 3};
		// FIXME: MSCV says moved raw array is not an rvalue?
#ifndef _MSC_VER
		CHECK(stl2::search(std::move(ib), ie).get_unsafe() == ib+4);
#else
		CHECK(stl2::search(std::move(ib), ie) == ib+4);
#endif
	}
#endif
}
