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

#include <nanorange.hpp>
#include <nanorange_extras.hpp>
#include "../catch.hpp"
#include "../test_iterators.hpp"

namespace ranges = nanorange;

TEST_CASE("alg.equal")
{
	using namespace ranges;
	int ia[] = {0, 1, 2, 3, 4, 5};
	constexpr unsigned s = sizeof(ia);
	//int ib[s] = {0, 1, 2, 5, 4, 5};
	int *ib = new int[s]{0, 1, 2, 5, 4, 5};
#ifdef HAVE_MISMATCHED_ITERS
	REQUIRE(equal(input_iterator<const int*>(ia),
							sentinel<const int*>(ia+s),
							input_iterator<const int*>(ia)));
	REQUIRE(equal(input_iterator<const int*>(ia),
							sentinel<const int*>(ia+s),
							input_iterator<const int*>(ia),
							sentinel<const int*>(ia+s)));
#endif
    using ra_t = random_access_iterator<const int*>;
    static_assert(InputIterator<ra_t>, "");
    static_assert(IndirectlyComparable<ra_t, ra_t, equal_to<>>, "");
	REQUIRE(equal(random_access_iterator<const int*>(ia),
							random_access_iterator<const int*>(ia+s),
							random_access_iterator<const int*>(ia),
							random_access_iterator<const int*>(ia + s)));
#ifdef HAVE_MISMATCHED_ITERS
	REQUIRE(equal(random_access_iterator<const int*>(ia),
							sentinel<const int*>(ia+s),
							random_access_iterator<const int*>(ia),
							sentinel<const int*>(ia + s)));
	REQUIRE(!equal(input_iterator<const int*>(ia),
							 sentinel<const int*>(ia+s),
							 input_iterator<const int*>(ib)));
	REQUIRE(!equal(input_iterator<const int*>(ia),
							 sentinel<const int*>(ia+s),
							 input_iterator<const int*>(ib),
							 sentinel<const int*>(ib + s)));
#endif
	REQUIRE(!equal(random_access_iterator<const int*>(ia),
							 random_access_iterator<const int*>(ia+s),
							 random_access_iterator<const int*>(ib),
							 random_access_iterator<const int*>(ib+s)));
#ifdef HAVE_MISMATCHED_ITERS
	REQUIRE(!equal(random_access_iterator<const int*>(ia),
							 sentinel<const int*>(ia+s),
							 random_access_iterator<const int*>(ib),
							 sentinel<const int*>(ib + s)));
	REQUIRE(!equal(input_iterator<const int*>(ia),
							 sentinel<const int*>(ia+s),
							 input_iterator<const int*>(ia),
							 sentinel<const int*>(ia + s - 1)));
#endif
	REQUIRE(!equal(random_access_iterator<const int*>(ia),
							 random_access_iterator<const int*>(ia+s),
							 random_access_iterator<const int*>(ia),
							 random_access_iterator<const int*>(ia+s-1)));
#ifdef HAVE_MISMATCHED_ITERS
	REQUIRE(!equal(random_access_iterator<const int*>(ia),
							 sentinel<const int*>(ia+s),
							 random_access_iterator<const int*>(ia),
							 sentinel<const int*>(ia + s - 1)));
#endif
	delete[] ib;
}

void test_rng()
{
	using namespace ranges;
	using ranges::ext::make_range;

	int ia[] = {0, 1, 2, 3, 4, 5};
	constexpr unsigned s = size(ia);
	int ib[s] = {0, 1, 2, 5, 4, 5};
	/*REQUIRE(equal(make_range(input_iterator<const int*>(ia),
												 sentinel<const int*>(ia+s)),
							input_iterator<const int*>(ia)));
	REQUIRE(equal(make_range(input_iterator<const int*>(ia),
												 sentinel<const int*>(ia+s)),
							make_range(input_iterator<const int*>(ia),
												 sentinel<const int*>(ia+s))));*/
	REQUIRE(equal(make_range(random_access_iterator<const int*>(ia),
												 random_access_iterator<const int*>(ia+s)),
							make_range(random_access_iterator<const int*>(ia),
												 random_access_iterator<const int*>(ia + s))));
	/*REQUIRE(equal(make_range(random_access_iterator<const int*>(ia),
												 sentinel<const int*>(ia+s)),
							make_range(random_access_iterator<const int*>(ia),
												 sentinel<const int*>(ia + s))));*/
	REQUIRE(!equal(make_range(input_iterator<const int*>(ia),
													input_iterator<const int*>(ia+s)),
							 input_iterator<const int*>(ib)));
	REQUIRE(!equal(make_range(input_iterator<const int*>(ia),
							  input_iterator<const int*>(ia+s)),
							 make_range(input_iterator<const int*>(ib),
													input_iterator<const int*>(ib + s))));
	REQUIRE(!equal(make_range(random_access_iterator<const int*>(ia),
													random_access_iterator<const int*>(ia+s)),
							 make_range(random_access_iterator<const int*>(ib),
													random_access_iterator<const int*>(ib+s))));
	REQUIRE(!equal(make_range(random_access_iterator<const int*>(ia),
													sentinel<const int*>(ia+s)),
							 make_range(random_access_iterator<const int*>(ib),
													sentinel<const int*>(ib + s))));
	REQUIRE(!equal(make_range(input_iterator<const int*>(ia),
													sentinel<const int*>(ia+s)),
							 make_range(input_iterator<const int*>(ia),
													sentinel<const int*>(ia + s - 1))));
	REQUIRE(!equal(make_range(random_access_iterator<const int*>(ia),
													random_access_iterator<const int*>(ia+s)),
							 make_range(random_access_iterator<const int*>(ia),
													random_access_iterator<const int*>(ia+s-1))));
	REQUIRE(!equal(make_range(random_access_iterator<const int*>(ia),
													sentinel<const int*>(ia+s)),
							 make_range(random_access_iterator<const int*>(ia),
													sentinel<const int*>(ia + s - 1))));
}

int comparison_count = 0;

template <typename T>
bool counting_equals(const T &a, const T &b)
{
	++comparison_count;
	return a == b;
}

void test_pred()
{
	using namespace ranges;

	int ia[] = {0, 1, 2, 3, 4, 5};
	constexpr unsigned s = size(ia);
	int ib[s] = {0, 1, 2, 5, 4, 5};
	REQUIRE(equal(input_iterator<const int*>(ia),
							sentinel<const int*>(ia+s),
							input_iterator<const int*>(ia),
							std::equal_to<int>()));
	REQUIRE(equal(input_iterator<const int*>(ia),
							sentinel<const int*>(ia+s),
							input_iterator<const int*>(ia),
							sentinel<const int*>(ia + s),
							std::equal_to<int>()));
	REQUIRE(equal(random_access_iterator<const int*>(ia),
							random_access_iterator<const int*>(ia+s),
							random_access_iterator<const int*>(ia),
							random_access_iterator<const int*>(ia+s),
							std::equal_to<int>()));
	REQUIRE(equal(random_access_iterator<const int*>(ia),
							sentinel<const int*>(ia+s),
							random_access_iterator<const int*>(ia),
							sentinel<const int*>(ia + s),
							std::equal_to<int>()));

	comparison_count = 0;
	REQUIRE(!equal(input_iterator<const int*>(ia),
							 sentinel<const int*>(ia+s),
							 input_iterator<const int*>(ia),
							 sentinel<const int*>(ia + s - 1),
							 counting_equals<int>));
	REQUIRE(comparison_count > 0);
	comparison_count = 0;
	REQUIRE(!equal(random_access_iterator<const int*>(ia),
							 random_access_iterator<const int*>(ia+s),
							 random_access_iterator<const int*>(ia),
							 random_access_iterator<const int*>(ia+s-1),
							 counting_equals<int>));
	REQUIRE(comparison_count == 0);
	comparison_count = 0;
	REQUIRE(!equal(random_access_iterator<const int*>(ia),
							 sentinel<const int*>(ia+s),
							 random_access_iterator<const int*>(ia),
							 sentinel<const int*>(ia + s - 1),
							 counting_equals<int>));
	REQUIRE(comparison_count > 0);
	REQUIRE(!equal(input_iterator<const int*>(ia),
							 sentinel<const int*>(ia+s),
							 input_iterator<const int*>(ib),
							 std::equal_to<int>()));
	REQUIRE(!equal(input_iterator<const int*>(ia),
							 sentinel<const int*>(ia+s),
							 input_iterator<const int*>(ib),
							 sentinel<const int*>(ib + s),
							 std::equal_to<int>()));
	REQUIRE(!equal(random_access_iterator<const int*>(ia),
							 random_access_iterator<const int*>(ia+s),
							 random_access_iterator<const int*>(ib),
							 random_access_iterator<const int*>(ib+s),
							 std::equal_to<int>()));
	REQUIRE(!equal(random_access_iterator<const int*>(ia),
							 sentinel<const int*>(ia+s),
							 random_access_iterator<const int*>(ib),
							 sentinel<const int*>(ib + s),
							 std::equal_to<int>()));
}

void test_rng_pred()
{
	using namespace ranges;
	using ranges::ext::make_range;

	int ia[] = {0, 1, 2, 3, 4, 5};
	constexpr unsigned s = size(ia);
	int ib[s] = {0, 1, 2, 5, 4, 5};
	REQUIRE(ranges::equal(make_range(input_iterator<const int*>(ia),
																 sentinel<const int*>(ia+s)),
											input_iterator<const int*>(ia),
											std::equal_to<int>()));
	REQUIRE(ranges::equal(make_range(input_iterator<const int*>(ia),
																 sentinel<const int*>(ia+s)),
											make_range(input_iterator<const int*>(ia),
																 sentinel<const int*>(ia + s)),
											std::equal_to<int>()));
	REQUIRE(ranges::equal(make_range(random_access_iterator<const int*>(ia),
																 random_access_iterator<const int*>(ia+s)),
											make_range(random_access_iterator<const int*>(ia),
																 random_access_iterator<const int*>(ia+s)),
											std::equal_to<int>()));
	REQUIRE(ranges::equal(make_range(random_access_iterator<const int*>(ia),
																 sentinel<const int*>(ia+s)),
											make_range(random_access_iterator<const int*>(ia),
																 sentinel<const int*>(ia + s)),
											std::equal_to<int>()));

	comparison_count = 0;
	REQUIRE(!ranges::equal(make_range(input_iterator<const int*>(ia),
																	sentinel<const int*>(ia+s)),
											 make_range(input_iterator<const int*>(ia),
																	sentinel<const int*>(ia + s - 1)),
											 counting_equals<int>));
	REQUIRE(comparison_count > 0);
	comparison_count = 0;
	REQUIRE(!ranges::equal(make_range(random_access_iterator<const int*>(ia),
																	random_access_iterator<const int*>(ia+s)),
											 make_range(random_access_iterator<const int*>(ia),
																	random_access_iterator<const int*>(ia+s-1)),
											 counting_equals<int>));
	REQUIRE(comparison_count == 0);
	comparison_count = 0;
	REQUIRE(!ranges::equal(make_range(random_access_iterator<const int*>(ia),
																	sentinel<const int*>(ia+s)),
											 make_range(random_access_iterator<const int*>(ia),
																	sentinel<const int*>(ia + s - 1)),
											 counting_equals<int>));
	REQUIRE(comparison_count > 0);
	REQUIRE(!ranges::equal(make_range(input_iterator<const int*>(ia),
																	sentinel<const int*>(ia+s)),
											 input_iterator<const int*>(ib),
											 std::equal_to<int>()));
	REQUIRE(!ranges::equal(make_range(input_iterator<const int*>(ia),
																	sentinel<const int*>(ia+s)),
											 make_range(input_iterator<const int*>(ib),
																	sentinel<const int*>(ib + s)),
											 std::equal_to<int>()));
	REQUIRE(!ranges::equal(make_range(random_access_iterator<const int*>(ia),
																	random_access_iterator<const int*>(ia+s)),
											 make_range(random_access_iterator<const int*>(ib),
																	random_access_iterator<const int*>(ib+s)),
											 std::equal_to<int>()));
	REQUIRE(!ranges::equal(make_range(random_access_iterator<const int*>(ia),
																	sentinel<const int*>(ia+s)),
											 make_range(random_access_iterator<const int*>(ib),
																	sentinel<const int*>(ib + s)),
											 std::equal_to<int>()));
}

int main()
{
	::test();
	::test_rng();
	::test_pred();
	::test_rng_pred();

	{
		int const a[] = {1,2,3};
		int const b[] = {1,2,3};
		REQUIRE(ranges::equal(a, ranges::begin(b)));
		REQUIRE(ranges::equal(ranges::begin(a), ranges::end(a), ranges::begin(b)));
	}

	return ::test_result();
}
