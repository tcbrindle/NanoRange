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

#include <stl2/utility.hpp>
#include <stl2/detail/algorithm/is_permutation.hpp>
#include "../simple_test.hpp"
#include "../test_utils.hpp"
#include "../test_iterators.hpp"

namespace stl2 = __stl2;

int comparison_count = 0;

template <typename T>
bool counting_equals( T const &a, T const &b )
{
	++comparison_count;
	return a == b;
}

struct S
{
	int i;
};

struct T
{
	int i;
};

int main()
{
	{
		const int ia[] = {0};
		const int ib[] = {0};
		const unsigned sa = sizeof(ia)/sizeof(ia[0]);
		CHECK(stl2::is_permutation(forward_iterator<const int*>(ia),
								   forward_iterator<const int*>(ia + 0),
								   forward_iterator<const int*>(ib),
								   std::equal_to<const int>()) == true);
		CHECK(stl2::is_permutation(forward_iterator<const int*>(ia),
								   forward_iterator<const int*>(ia + sa),
								   forward_iterator<const int*>(ib),
								   std::equal_to<const int>()) == true);

		CHECK(stl2::is_permutation(forward_iterator<const int*>(ia),
								   forward_iterator<const int*>(ia + sa),
								   forward_iterator<const int*>(ib),
								   forward_iterator<const int*>(ib + sa),
								   std::equal_to<const int>()) == true);
		CHECK(stl2::is_permutation(forward_iterator<const int*>(ia),
								   forward_iterator<const int*>(ia + sa),
								   forward_iterator<const int*>(ib),
								   forward_iterator<const int*>(ib + sa - 1),
								   std::equal_to<const int>()) == false);
	}
	{
		const int ia[] = {0};
		const int ib[] = {1};
		const unsigned sa = sizeof(ia)/sizeof(ia[0]);
		CHECK(stl2::is_permutation(forward_iterator<const int*>(ia),
								   forward_iterator<const int*>(ia + sa),
								   forward_iterator<const int*>(ib),
								   std::equal_to<const int>()) == false);

		CHECK(stl2::is_permutation(forward_iterator<const int*>(ia),
								   forward_iterator<const int*>(ia + sa),
								   forward_iterator<const int*>(ib),
								   forward_iterator<const int*>(ib + sa),
								   std::equal_to<const int>()) == false);
	}

	{
		const int ia[] = {0, 0};
		const int ib[] = {0, 0};
		const unsigned sa = sizeof(ia)/sizeof(ia[0]);
		CHECK(stl2::is_permutation(forward_iterator<const int*>(ia),
								   forward_iterator<const int*>(ia + sa),
								   forward_iterator<const int*>(ib),
								   std::equal_to<const int>()) == true);

		CHECK(stl2::is_permutation(forward_iterator<const int*>(ia),
								   forward_iterator<const int*>(ia + sa),
								   forward_iterator<const int*>(ib),
								   forward_iterator<const int*>(ib + sa),
								   std::equal_to<const int>()) == true);
		CHECK(stl2::is_permutation(forward_iterator<const int*>(ia),
								   forward_iterator<const int*>(ia + sa),
								   forward_iterator<const int*>(ib),
								   forward_iterator<const int*>(ib + sa - 1),
								   std::equal_to<const int>()) == false);
	}
	{
		const int ia[] = {0, 0};
		const int ib[] = {0, 1};
		const unsigned sa = sizeof(ia)/sizeof(ia[0]);
		CHECK(stl2::is_permutation(forward_iterator<const int*>(ia),
								   forward_iterator<const int*>(ia + sa),
								   forward_iterator<const int*>(ib),
								   std::equal_to<const int>()) == false);

		CHECK(stl2::is_permutation(forward_iterator<const int*>(ia),
								   forward_iterator<const int*>(ia + sa),
								   forward_iterator<const int*>(ib),
								   forward_iterator<const int*>(ib + sa),
								   std::equal_to<const int>()) == false);
	}
	{
		const int ia[] = {0, 0};
		const int ib[] = {1, 0};
		const unsigned sa = sizeof(ia)/sizeof(ia[0]);
		CHECK(stl2::is_permutation(forward_iterator<const int*>(ia),
								   forward_iterator<const int*>(ia + sa),
								   forward_iterator<const int*>(ib),
								   std::equal_to<const int>()) == false);

		CHECK(stl2::is_permutation(forward_iterator<const int*>(ia),
								   forward_iterator<const int*>(ia + sa),
								   forward_iterator<const int*>(ib),
								   forward_iterator<const int*>(ib + sa),
								   std::equal_to<const int>()) == false);
	}
	{
		const int ia[] = {0, 0};
		const int ib[] = {1, 1};
		const unsigned sa = sizeof(ia)/sizeof(ia[0]);
		CHECK(stl2::is_permutation(forward_iterator<const int*>(ia),
								   forward_iterator<const int*>(ia + sa),
								   forward_iterator<const int*>(ib),
								   std::equal_to<const int>()) == false);

		CHECK(stl2::is_permutation(forward_iterator<const int*>(ia),
								   forward_iterator<const int*>(ia + sa),
								   forward_iterator<const int*>(ib),
								   forward_iterator<const int*>(ib + sa),
								   std::equal_to<const int>()) == false);
	}
	{
		const int ia[] = {0, 1};
		const int ib[] = {0, 0};
		const unsigned sa = sizeof(ia)/sizeof(ia[0]);
		CHECK(stl2::is_permutation(forward_iterator<const int*>(ia),
								   forward_iterator<const int*>(ia + sa),
								   forward_iterator<const int*>(ib),
								   std::equal_to<const int>()) == false);

		CHECK(stl2::is_permutation(forward_iterator<const int*>(ia),
								   forward_iterator<const int*>(ia + sa),
								   forward_iterator<const int*>(ib),
								   forward_iterator<const int*>(ib + sa),
								   std::equal_to<const int>()) == false);
	}
	{
		const int ia[] = {0, 1};
		const int ib[] = {0, 1};
		const unsigned sa = sizeof(ia)/sizeof(ia[0]);
		CHECK(stl2::is_permutation(forward_iterator<const int*>(ia),
								   forward_iterator<const int*>(ia + sa),
								   forward_iterator<const int*>(ib),
								   std::equal_to<const int>()) == true);

		CHECK(stl2::is_permutation(forward_iterator<const int*>(ia),
								   forward_iterator<const int*>(ia + sa),
								   forward_iterator<const int*>(ib),
								   forward_iterator<const int*>(ib + sa),
								   std::equal_to<const int>()) == true);
		CHECK(stl2::is_permutation(forward_iterator<const int*>(ia),
								   forward_iterator<const int*>(ia + sa),
								   forward_iterator<const int*>(ib),
								   forward_iterator<const int*>(ib + sa - 1),
								   std::equal_to<const int>()) == false);
	}
	{
		const int ia[] = {0, 1};
		const int ib[] = {1, 0};
		const unsigned sa = sizeof(ia)/sizeof(ia[0]);
		CHECK(stl2::is_permutation(forward_iterator<const int*>(ia),
								   forward_iterator<const int*>(ia + sa),
								   forward_iterator<const int*>(ib),
								   std::equal_to<const int>()) == true);

		CHECK(stl2::is_permutation(forward_iterator<const int*>(ia),
								   forward_iterator<const int*>(ia + sa),
								   forward_iterator<const int*>(ib),
								   forward_iterator<const int*>(ib + sa),
								   std::equal_to<const int>()) == true);
		CHECK(stl2::is_permutation(forward_iterator<const int*>(ia),
								   forward_iterator<const int*>(ia + sa),
								   forward_iterator<const int*>(ib),
								   forward_iterator<const int*>(ib + sa - 1),
								   std::equal_to<const int>()) == false);
	}
	{
		const int ia[] = {0, 1};
		const int ib[] = {1, 1};
		const unsigned sa = sizeof(ia)/sizeof(ia[0]);
		CHECK(stl2::is_permutation(forward_iterator<const int*>(ia),
								   forward_iterator<const int*>(ia + sa),
								   forward_iterator<const int*>(ib),
								   std::equal_to<const int>()) == false);

		CHECK(stl2::is_permutation(forward_iterator<const int*>(ia),
								   forward_iterator<const int*>(ia + sa),
								   forward_iterator<const int*>(ib),
								   forward_iterator<const int*>(ib + sa),
								   std::equal_to<const int>()) == false);
	}
	{
		const int ia[] = {1, 0};
		const int ib[] = {0, 0};
		const unsigned sa = sizeof(ia)/sizeof(ia[0]);
		CHECK(stl2::is_permutation(forward_iterator<const int*>(ia),
								   forward_iterator<const int*>(ia + sa),
								   forward_iterator<const int*>(ib),
								   std::equal_to<const int>()) == false);

		CHECK(stl2::is_permutation(forward_iterator<const int*>(ia),
								   forward_iterator<const int*>(ia + sa),
								   forward_iterator<const int*>(ib),
								   forward_iterator<const int*>(ib + sa),
								   std::equal_to<const int>()) == false);
	}
	{
		const int ia[] = {1, 0};
		const int ib[] = {0, 1};
		const unsigned sa = sizeof(ia)/sizeof(ia[0]);
		CHECK(stl2::is_permutation(forward_iterator<const int*>(ia),
								   forward_iterator<const int*>(ia + sa),
								   forward_iterator<const int*>(ib),
								   std::equal_to<const int>()) == true);

		CHECK(stl2::is_permutation(forward_iterator<const int*>(ia),
								   forward_iterator<const int*>(ia + sa),
								   forward_iterator<const int*>(ib),
								   forward_iterator<const int*>(ib + sa),
								   std::equal_to<const int>()) == true);
		CHECK(stl2::is_permutation(forward_iterator<const int*>(ia),
								   forward_iterator<const int*>(ia + sa),
								   forward_iterator<const int*>(ib),
								   forward_iterator<const int*>(ib + sa - 1),
								   std::equal_to<const int>()) == false);
	}
	{
		const int ia[] = {1, 0};
		const int ib[] = {1, 0};
		const unsigned sa = sizeof(ia)/sizeof(ia[0]);
		CHECK(stl2::is_permutation(forward_iterator<const int*>(ia),
								   forward_iterator<const int*>(ia + sa),
								   forward_iterator<const int*>(ib),
								   std::equal_to<const int>()) == true);

		CHECK(stl2::is_permutation(forward_iterator<const int*>(ia),
								   forward_iterator<const int*>(ia + sa),
								   forward_iterator<const int*>(ib),
								   forward_iterator<const int*>(ib + sa),
								   std::equal_to<const int>()) == true);
		CHECK(stl2::is_permutation(forward_iterator<const int*>(ia),
								   forward_iterator<const int*>(ia + sa),
								   forward_iterator<const int*>(ib),
								   forward_iterator<const int*>(ib + sa - 1),
								   std::equal_to<const int>()) == false);
	}
	{
		const int ia[] = {1, 0};
		const int ib[] = {1, 1};
		const unsigned sa = sizeof(ia)/sizeof(ia[0]);
		CHECK(stl2::is_permutation(forward_iterator<const int*>(ia),
								   forward_iterator<const int*>(ia + sa),
								   forward_iterator<const int*>(ib),
								   std::equal_to<const int>()) == false);

		CHECK(stl2::is_permutation(forward_iterator<const int*>(ia),
								   forward_iterator<const int*>(ia + sa),
								   forward_iterator<const int*>(ib),
								   forward_iterator<const int*>(ib + sa),
								   std::equal_to<const int>()) == false);
	}
	{
		const int ia[] = {1, 1};
		const int ib[] = {0, 0};
		const unsigned sa = sizeof(ia)/sizeof(ia[0]);
		CHECK(stl2::is_permutation(forward_iterator<const int*>(ia),
								   forward_iterator<const int*>(ia + sa),
								   forward_iterator<const int*>(ib),
								   std::equal_to<const int>()) == false);

		CHECK(stl2::is_permutation(forward_iterator<const int*>(ia),
								   forward_iterator<const int*>(ia + sa),
								   forward_iterator<const int*>(ib),
								   forward_iterator<const int*>(ib + sa),
								   std::equal_to<const int>()) == false);
	}
	{
		const int ia[] = {1, 1};
		const int ib[] = {0, 1};
		const unsigned sa = sizeof(ia)/sizeof(ia[0]);
		CHECK(stl2::is_permutation(forward_iterator<const int*>(ia),
								   forward_iterator<const int*>(ia + sa),
								   forward_iterator<const int*>(ib),
								   std::equal_to<const int>()) == false);

		CHECK(stl2::is_permutation(forward_iterator<const int*>(ia),
								   forward_iterator<const int*>(ia + sa),
								   forward_iterator<const int*>(ib),
								   forward_iterator<const int*>(ib + sa),
								   std::equal_to<const int>()) == false);
	}
	{
		const int ia[] = {1, 1};
		const int ib[] = {1, 0};
		const unsigned sa = sizeof(ia)/sizeof(ia[0]);
		CHECK(stl2::is_permutation(forward_iterator<const int*>(ia),
								   forward_iterator<const int*>(ia + sa),
								   forward_iterator<const int*>(ib),
								   std::equal_to<const int>()) == false);

		CHECK(stl2::is_permutation(forward_iterator<const int*>(ia),
								   forward_iterator<const int*>(ia + sa),
								   forward_iterator<const int*>(ib),
								   forward_iterator<const int*>(ib + sa),
								   std::equal_to<const int>()) == false);
	}
	{
		const int ia[] = {1, 1};
		const int ib[] = {1, 1};
		const unsigned sa = sizeof(ia)/sizeof(ia[0]);
		CHECK(stl2::is_permutation(forward_iterator<const int*>(ia),
								   forward_iterator<const int*>(ia + sa),
								   forward_iterator<const int*>(ib),
								   std::equal_to<const int>()) == true);

		CHECK(stl2::is_permutation(forward_iterator<const int*>(ia),
								   forward_iterator<const int*>(ia + sa),
								   forward_iterator<const int*>(ib),
								   forward_iterator<const int*>(ib + sa),
								   std::equal_to<const int>()) == true);
		CHECK(stl2::is_permutation(forward_iterator<const int*>(ia),
								   forward_iterator<const int*>(ia + sa),
								   forward_iterator<const int*>(ib),
								   forward_iterator<const int*>(ib + sa - 1),
								   std::equal_to<const int>()) == false);
	}

	{
		const int ia[] = {0, 0, 0};
		const int ib[] = {1, 0, 0};
		const unsigned sa = sizeof(ia)/sizeof(ia[0]);
		CHECK(stl2::is_permutation(forward_iterator<const int*>(ia),
								   forward_iterator<const int*>(ia + sa),
								   forward_iterator<const int*>(ib),
								   std::equal_to<const int>()) == false);

		CHECK(stl2::is_permutation(forward_iterator<const int*>(ia),
								   forward_iterator<const int*>(ia + sa),
								   forward_iterator<const int*>(ib),
								   forward_iterator<const int*>(ib + sa),
								   std::equal_to<const int>()) == false);
	}
	{
		const int ia[] = {0, 0, 0};
		const int ib[] = {1, 0, 1};
		const unsigned sa = sizeof(ia)/sizeof(ia[0]);
		CHECK(stl2::is_permutation(forward_iterator<const int*>(ia),
								   forward_iterator<const int*>(ia + sa),
								   forward_iterator<const int*>(ib),
								   std::equal_to<const int>()) == false);

		CHECK(stl2::is_permutation(forward_iterator<const int*>(ia),
								   forward_iterator<const int*>(ia + sa),
								   forward_iterator<const int*>(ib),
								   forward_iterator<const int*>(ib + sa),
								   std::equal_to<const int>()) == false);
	}
	{
		const int ia[] = {0, 0, 0};
		const int ib[] = {1, 0, 2};
		const unsigned sa = sizeof(ia)/sizeof(ia[0]);
		CHECK(stl2::is_permutation(forward_iterator<const int*>(ia),
								   forward_iterator<const int*>(ia + sa),
								   forward_iterator<const int*>(ib),
								   std::equal_to<const int>()) == false);

		CHECK(stl2::is_permutation(forward_iterator<const int*>(ia),
								   forward_iterator<const int*>(ia + sa),
								   forward_iterator<const int*>(ib),
								   forward_iterator<const int*>(ib + sa),
								   std::equal_to<const int>()) == false);
	}
	{
		const int ia[] = {0, 0, 0};
		const int ib[] = {1, 1, 0};
		const unsigned sa = sizeof(ia)/sizeof(ia[0]);
		CHECK(stl2::is_permutation(forward_iterator<const int*>(ia),
								   forward_iterator<const int*>(ia + sa),
								   forward_iterator<const int*>(ib),
								   std::equal_to<const int>()) == false);

		CHECK(stl2::is_permutation(forward_iterator<const int*>(ia),
								   forward_iterator<const int*>(ia + sa),
								   forward_iterator<const int*>(ib),
								   forward_iterator<const int*>(ib + sa),
								   std::equal_to<const int>()) == false);
	}
	{
		const int ia[] = {0, 0, 0};
		const int ib[] = {1, 1, 1};
		const unsigned sa = sizeof(ia)/sizeof(ia[0]);
		CHECK(stl2::is_permutation(forward_iterator<const int*>(ia),
								   forward_iterator<const int*>(ia + sa),
								   forward_iterator<const int*>(ib),
								   std::equal_to<const int>()) == false);

		CHECK(stl2::is_permutation(forward_iterator<const int*>(ia),
								   forward_iterator<const int*>(ia + sa),
								   forward_iterator<const int*>(ib),
								   forward_iterator<const int*>(ib + sa),
								   std::equal_to<const int>()) == false);
	}
	{
		const int ia[] = {0, 0, 0};
		const int ib[] = {1, 1, 2};
		const unsigned sa = sizeof(ia)/sizeof(ia[0]);
		CHECK(stl2::is_permutation(forward_iterator<const int*>(ia),
								   forward_iterator<const int*>(ia + sa),
								   forward_iterator<const int*>(ib),
								   std::equal_to<const int>()) == false);

		CHECK(stl2::is_permutation(forward_iterator<const int*>(ia),
								   forward_iterator<const int*>(ia + sa),
								   forward_iterator<const int*>(ib),
								   forward_iterator<const int*>(ib + sa),
								   std::equal_to<const int>()) == false);
	}
	{
		const int ia[] = {0, 0, 0};
		const int ib[] = {1, 2, 0};
		const unsigned sa = sizeof(ia)/sizeof(ia[0]);
		CHECK(stl2::is_permutation(forward_iterator<const int*>(ia),
								   forward_iterator<const int*>(ia + sa),
								   forward_iterator<const int*>(ib),
								   std::equal_to<const int>()) == false);

		CHECK(stl2::is_permutation(forward_iterator<const int*>(ia),
								   forward_iterator<const int*>(ia + sa),
								   forward_iterator<const int*>(ib),
								   forward_iterator<const int*>(ib + sa),
								   std::equal_to<const int>()) == false);
	}
	{
		const int ia[] = {0, 0, 0};
		const int ib[] = {1, 2, 1};
		const unsigned sa = sizeof(ia)/sizeof(ia[0]);
		CHECK(stl2::is_permutation(forward_iterator<const int*>(ia),
								   forward_iterator<const int*>(ia + sa),
								   forward_iterator<const int*>(ib),
								   std::equal_to<const int>()) == false);

		CHECK(stl2::is_permutation(forward_iterator<const int*>(ia),
								   forward_iterator<const int*>(ia + sa),
								   forward_iterator<const int*>(ib),
								   forward_iterator<const int*>(ib + sa),
								   std::equal_to<const int>()) == false);
	}
	{
		const int ia[] = {0, 0, 0};
		const int ib[] = {1, 2, 2};
		const unsigned sa = sizeof(ia)/sizeof(ia[0]);
		CHECK(stl2::is_permutation(forward_iterator<const int*>(ia),
								   forward_iterator<const int*>(ia + sa),
								   forward_iterator<const int*>(ib),
								   std::equal_to<const int>()) == false);

		CHECK(stl2::is_permutation(forward_iterator<const int*>(ia),
								   forward_iterator<const int*>(ia + sa),
								   forward_iterator<const int*>(ib),
								   forward_iterator<const int*>(ib + sa),
								   std::equal_to<const int>()) == false);
	}
	{
		const int ia[] = {0, 0, 1};
		const int ib[] = {1, 0, 0};
		const unsigned sa = sizeof(ia)/sizeof(ia[0]);
		CHECK(stl2::is_permutation(forward_iterator<const int*>(ia),
								   forward_iterator<const int*>(ia + sa),
								   forward_iterator<const int*>(ib),
								   std::equal_to<const int>()) == true);

		CHECK(stl2::is_permutation(forward_iterator<const int*>(ia),
								   forward_iterator<const int*>(ia + sa),
								   forward_iterator<const int*>(ib),
								   forward_iterator<const int*>(ib + sa),
								   std::equal_to<const int>()) == true);
		CHECK(stl2::is_permutation(forward_iterator<const int*>(ia),
								   forward_iterator<const int*>(ia + sa),
								   forward_iterator<const int*>(ib),
								   forward_iterator<const int*>(ib + sa - 1),
								   std::equal_to<const int>()) == false);
	}
	{
		const int ia[] = {0, 0, 1};
		const int ib[] = {1, 0, 1};
		const unsigned sa = sizeof(ia)/sizeof(ia[0]);
		CHECK(stl2::is_permutation(forward_iterator<const int*>(ia),
								   forward_iterator<const int*>(ia + sa),
								   forward_iterator<const int*>(ib),
								   std::equal_to<const int>()) == false);

		CHECK(stl2::is_permutation(forward_iterator<const int*>(ia),
								   forward_iterator<const int*>(ia + sa),
								   forward_iterator<const int*>(ib),
								   forward_iterator<const int*>(ib + sa),
								   std::equal_to<const int>()) == false);
	}
	{
		const int ia[] = {0, 1, 2};
		const int ib[] = {1, 0, 2};
		const unsigned sa = sizeof(ia)/sizeof(ia[0]);
		CHECK(stl2::is_permutation(forward_iterator<const int*>(ia),
								   forward_iterator<const int*>(ia + sa),
								   forward_iterator<const int*>(ib),
								   std::equal_to<const int>()) == true);

		CHECK(stl2::is_permutation(forward_iterator<const int*>(ia),
								   forward_iterator<const int*>(ia + sa),
								   forward_iterator<const int*>(ib),
								   forward_iterator<const int*>(ib + sa),
								   std::equal_to<const int>()) == true);
		CHECK(stl2::is_permutation(forward_iterator<const int*>(ia),
								   forward_iterator<const int*>(ia + sa),
								   forward_iterator<const int*>(ib),
								   forward_iterator<const int*>(ib + sa - 1),
								   std::equal_to<const int>()) == false);
	}
	{
		const int ia[] = {0, 1, 2};
		const int ib[] = {1, 2, 0};
		const unsigned sa = sizeof(ia)/sizeof(ia[0]);
		CHECK(stl2::is_permutation(forward_iterator<const int*>(ia),
								   forward_iterator<const int*>(ia + sa),
								   forward_iterator<const int*>(ib),
								   std::equal_to<const int>()) == true);

		CHECK(stl2::is_permutation(forward_iterator<const int*>(ia),
								   forward_iterator<const int*>(ia + sa),
								   forward_iterator<const int*>(ib),
								   forward_iterator<const int*>(ib + sa),
								   std::equal_to<const int>()) == true);
		CHECK(stl2::is_permutation(forward_iterator<const int*>(ia),
								   forward_iterator<const int*>(ia + sa),
								   forward_iterator<const int*>(ib),
								   forward_iterator<const int*>(ib + sa - 1),
								   std::equal_to<const int>()) == false);
	}
	{
		const int ia[] = {0, 1, 2};
		const int ib[] = {2, 1, 0};
		const unsigned sa = sizeof(ia)/sizeof(ia[0]);
		CHECK(stl2::is_permutation(forward_iterator<const int*>(ia),
								   forward_iterator<const int*>(ia + sa),
								   forward_iterator<const int*>(ib),
								   std::equal_to<const int>()) == true);

		CHECK(stl2::is_permutation(forward_iterator<const int*>(ia),
								   forward_iterator<const int*>(ia + sa),
								   forward_iterator<const int*>(ib),
								   forward_iterator<const int*>(ib + sa),
								   std::equal_to<const int>()) == true);
		CHECK(stl2::is_permutation(forward_iterator<const int*>(ia),
								   forward_iterator<const int*>(ia + sa),
								   forward_iterator<const int*>(ib),
								   forward_iterator<const int*>(ib + sa - 1),
								   std::equal_to<const int>()) == false);
	}
	{
		const int ia[] = {0, 1, 2};
		const int ib[] = {2, 0, 1};
		const unsigned sa = sizeof(ia)/sizeof(ia[0]);
		CHECK(stl2::is_permutation(forward_iterator<const int*>(ia),
								   forward_iterator<const int*>(ia + sa),
								   forward_iterator<const int*>(ib),
								   std::equal_to<const int>()) == true);

		CHECK(stl2::is_permutation(forward_iterator<const int*>(ia),
								   forward_iterator<const int*>(ia + sa),
								   forward_iterator<const int*>(ib),
								   forward_iterator<const int*>(ib + sa),
								   std::equal_to<const int>()) == true);
		CHECK(stl2::is_permutation(forward_iterator<const int*>(ia),
								   forward_iterator<const int*>(ia + sa),
								   forward_iterator<const int*>(ib),
								   forward_iterator<const int*>(ib + sa - 1),
								   std::equal_to<const int>()) == false);
	}
	{
		const int ia[] = {0, 0, 1};
		const int ib[] = {1, 0, 1};
		const unsigned sa = sizeof(ia)/sizeof(ia[0]);
		CHECK(stl2::is_permutation(forward_iterator<const int*>(ia),
								   forward_iterator<const int*>(ia + sa),
								   forward_iterator<const int*>(ib),
								   std::equal_to<const int>()) == false);

		CHECK(stl2::is_permutation(forward_iterator<const int*>(ia),
								   forward_iterator<const int*>(ia + sa),
								   forward_iterator<const int*>(ib),
								   forward_iterator<const int*>(ib + sa),
								   std::equal_to<const int>()) == false);
	}
	{
		const int ia[] = {0, 0, 1};
		const int ib[] = {1, 0, 0};
		const unsigned sa = sizeof(ia)/sizeof(ia[0]);
		CHECK(stl2::is_permutation(forward_iterator<const int*>(ia),
								   forward_iterator<const int*>(ia + sa),
								   forward_iterator<const int*>(ib),
								   std::equal_to<const int>()) == true);

		CHECK(stl2::is_permutation(forward_iterator<const int*>(ia),
								   forward_iterator<const int*>(ia + sa),
								   forward_iterator<const int*>(ib),
								   forward_iterator<const int*>(ib + sa),
								   std::equal_to<const int>()) == true);
		CHECK(stl2::is_permutation(forward_iterator<const int*>(ia),
								   forward_iterator<const int*>(ia + sa),
								   forward_iterator<const int*>(ib + 1),
								   forward_iterator<const int*>(ib + sa),
								   std::equal_to<const int>()) == false);
		CHECK(stl2::is_permutation(forward_iterator<const int*>(ia),
								   forward_iterator<const int*>(ia + sa),
								   forward_iterator<const int*>(ib),
								   forward_iterator<const int*>(ib + sa - 1),
								   std::equal_to<const int>()) == false);
	}
	{
		const int ia[] = {0, 1, 2, 3, 0, 5, 6, 2, 4, 4};
		const int ib[] = {4, 2, 3, 0, 1, 4, 0, 5, 6, 2};
		const unsigned sa = sizeof(ia)/sizeof(ia[0]);
		CHECK(stl2::is_permutation(forward_iterator<const int*>(ia),
								   forward_iterator<const int*>(ia + sa),
								   forward_iterator<const int*>(ib),
								   std::equal_to<const int>()) == true);

		CHECK(stl2::is_permutation(forward_iterator<const int*>(ia),
								   forward_iterator<const int*>(ia + sa),
								   forward_iterator<const int*>(ib),
								   forward_iterator<const int*>(ib + sa),
								   std::equal_to<const int>()) == true);
		CHECK(stl2::is_permutation(forward_iterator<const int*>(ia),
								   forward_iterator<const int*>(ia + sa),
								   forward_iterator<const int*>(ib + 1),
								   forward_iterator<const int*>(ib + sa),
								   std::equal_to<const int>()) == false);
		CHECK(stl2::is_permutation(forward_iterator<const int*>(ia),
								   forward_iterator<const int*>(ia + sa),
								   forward_iterator<const int*>(ib),
								   forward_iterator<const int*>(ib + sa - 1),
								   std::equal_to<const int>()) == false);
		comparison_count = 0;
		CHECK(stl2::is_permutation(forward_iterator<const int*>(ia),
								   forward_iterator<const int*>(ia + sa),
								   forward_iterator<const int*>(ib),
								   forward_iterator<const int*>(ib + sa - 1),
								   counting_equals<const int>) == false);
		CHECK( comparison_count > 0 );
		comparison_count = 0;
		CHECK(stl2::is_permutation(random_access_iterator<const int*>(ia),
								   random_access_iterator<const int*>(ia + sa),
								   random_access_iterator<const int*>(ib),
								   random_access_iterator<const int*>(ib + sa - 1),
								   counting_equals<const int>) == false);
		CHECK ( comparison_count == 0 );
	}
	{
		const int ia[] = {0, 1, 2, 3, 0, 5, 6, 2, 4, 4};
		const int ib[] = {4, 2, 3, 0, 1, 4, 0, 5, 6, 0};
		const unsigned sa = sizeof(ia)/sizeof(ia[0]);
		CHECK(stl2::is_permutation(forward_iterator<const int*>(ia),
								   forward_iterator<const int*>(ia + sa),
								   forward_iterator<const int*>(ib),
								   std::equal_to<const int>()) == false);

		CHECK(stl2::is_permutation(forward_iterator<const int*>(ia),
								   forward_iterator<const int*>(ia + sa),
								   forward_iterator<const int*>(ib),
								   forward_iterator<const int*>(ib + sa),
								   std::equal_to<const int>()) == false);
	}

	// Iterator tests, without predicate:
	{
		const int ia[] = {0, 1, 2, 3, 0, 5, 6, 2, 4, 4};
		const int ib[] = {4, 2, 3, 0, 1, 4, 0, 5, 6, 2};
		const unsigned sa = sizeof(ia)/sizeof(ia[0]);
		CHECK(stl2::is_permutation(forward_iterator<const int*>(ia),
								   forward_iterator<const int*>(ia + sa),
								   forward_iterator<const int*>(ib)) == true);

		CHECK(stl2::is_permutation(forward_iterator<const int*>(ia),
								   forward_iterator<const int*>(ia + sa),
								   forward_iterator<const int*>(ib),
								   forward_iterator<const int*>(ib + sa)) == true);
		CHECK(stl2::is_permutation(forward_iterator<const int*>(ia),
								   forward_iterator<const int*>(ia + sa),
								   forward_iterator<const int*>(ib + 1),
								   forward_iterator<const int*>(ib + sa)) == false);
		CHECK(stl2::is_permutation(forward_iterator<const int*>(ia),
								   forward_iterator<const int*>(ia + sa),
								   forward_iterator<const int*>(ib),
								   forward_iterator<const int*>(ib + sa - 1)) == false);
	}

	// Iterator tests, with sentinels:
	{
		const int ia[] = {0, 1, 2, 3, 0, 5, 6, 2, 4, 4};
		const int ib[] = {4, 2, 3, 0, 1, 4, 0, 5, 6, 2};
		const unsigned sa = sizeof(ia)/sizeof(ia[0]);
		CHECK(stl2::is_permutation(forward_iterator<const int*>(ia),
								   sentinel<const int*>(ia + sa),
								   forward_iterator<const int*>(ib)) == true);

		CHECK(stl2::is_permutation(forward_iterator<const int*>(ia),
								   sentinel<const int*>(ia + sa),
								   forward_iterator<const int*>(ib),
								   sentinel<const int*>(ib + sa)) == true);
		CHECK(stl2::is_permutation(forward_iterator<const int*>(ia),
								   sentinel<const int*>(ia + sa),
								   forward_iterator<const int*>(ib + 1),
								   sentinel<const int*>(ib + sa)) == false);
		CHECK(stl2::is_permutation(forward_iterator<const int*>(ia),
								   sentinel<const int*>(ia + sa),
								   forward_iterator<const int*>(ib),
								   sentinel<const int*>(ib + sa - 1)) == false);
	}

	// BoundedView tests, with sentinels:
	{
		const int ia[] = {0, 1, 2, 3, 0, 5, 6, 2, 4, 4};
		const int ib[] = {4, 2, 3, 0, 1, 4, 0, 5, 6, 2};
		const unsigned sa = sizeof(ia)/sizeof(ia[0]);
		CHECK(stl2::is_permutation(stl2::ext::make_range(forward_iterator<const int*>(ia),
								   sentinel<const int*>(ia + sa)),
								   forward_iterator<const int*>(ib)) == true);

		CHECK(stl2::is_permutation(stl2::ext::make_range(forward_iterator<const int*>(ia),
								   sentinel<const int*>(ia + sa)),
								   stl2::ext::make_range(forward_iterator<const int*>(ib),
								   sentinel<const int*>(ib + sa))) == true);
		CHECK(stl2::is_permutation(stl2::ext::make_range(forward_iterator<const int*>(ia),
								   sentinel<const int*>(ia + sa)),
								   stl2::ext::make_range(forward_iterator<const int*>(ib + 1),
								   sentinel<const int*>(ib + sa))) == false);
		CHECK(stl2::is_permutation(stl2::ext::make_range(forward_iterator<const int*>(ia),
								   sentinel<const int*>(ia + sa)),
								   stl2::ext::make_range(forward_iterator<const int*>(ib),
								   sentinel<const int*>(ib + sa - 1))) == false);
	}

	// BoundedView tests, with sentinels, with predicate:
	{
		const int ia[] = {0, 1, 2, 3, 0, 5, 6, 2, 4, 4};
		const int ib[] = {4, 2, 3, 0, 1, 4, 0, 5, 6, 2};
		const unsigned sa = sizeof(ia)/sizeof(ia[0]);
		CHECK(stl2::is_permutation(stl2::ext::make_range(forward_iterator<const int*>(ia),
								   sentinel<const int*>(ia + sa)),
								   forward_iterator<const int*>(ib),
								   std::equal_to<int const>()) == true);

		CHECK(stl2::is_permutation(stl2::ext::make_range(forward_iterator<const int*>(ia),
								   sentinel<const int*>(ia + sa)),
								   stl2::ext::make_range(forward_iterator<const int*>(ib),
								   sentinel<const int*>(ib + sa)),
								   std::equal_to<int const>()) == true);
		CHECK(stl2::is_permutation(stl2::ext::make_range(forward_iterator<const int*>(ia),
								   sentinel<const int*>(ia + sa)),
								   stl2::ext::make_range(forward_iterator<const int*>(ib + 1),
								   sentinel<const int*>(ib + sa)),
								   std::equal_to<int const>()) == false);
		CHECK(stl2::is_permutation(stl2::ext::make_range(forward_iterator<const int*>(ia),
								   sentinel<const int*>(ia + sa)),
								   stl2::ext::make_range(forward_iterator<const int*>(ib),
								   sentinel<const int*>(ib + sa - 1)),
								   std::equal_to<int const>()) == false);
	}

	// BoundedView tests, with sentinels, with predicate and projections:
	{
		const S ia[] = {{0}, {1}, {2}, {3}, {0}, {5}, {6}, {2}, {4}, {4}};
		const T ib[] = {{4}, {2}, {3}, {0}, {1}, {4}, {0}, {5}, {6}, {2}};
		const unsigned sa = sizeof(ia)/sizeof(ia[0]);
		CHECK(stl2::is_permutation(ia, &ib[0], std::equal_to<int const>(), &S::i, &T::i) == true);
		CHECK(stl2::is_permutation(ia, ib, std::equal_to<int const>(), &S::i, &T::i) == true);
		CHECK(stl2::is_permutation(stl2::ext::make_range(forward_iterator<const S*>(ia),
								   sentinel<const S*>(ia + sa)),
								   stl2::ext::make_range(forward_iterator<const T*>(ib + 1),
								   sentinel<const T*>(ib + sa)),
								   std::equal_to<int const>(), &S::i, &T::i) == false);
		CHECK(stl2::is_permutation(stl2::ext::make_range(forward_iterator<const S*>(ia),
								   sentinel<const S*>(ia + sa)),
								   stl2::ext::make_range(forward_iterator<const T*>(ib),
								   sentinel<const T*>(ib + sa - 1)),
								   std::equal_to<int const>(), &S::i, &T::i) == false);
	}

	// Iterator tests, with sentinels, with predicate and projections:
	{
		const S ia[] = {{0}, {1}, {2}, {3}, {0}, {5}, {6}, {2}, {4}, {4}};
		const T ib[] = {{4}, {2}, {3}, {0}, {1}, {4}, {0}, {5}, {6}, {2}};
		const unsigned sa = sizeof(ia)/sizeof(ia[0]);
		CHECK(stl2::is_permutation(forward_iterator<const S*>(ia),
								   sentinel<const S*>(ia + sa),
								   forward_iterator<const T*>(ib),
								   std::equal_to<int const>(), &S::i, &T::i) == true);

		CHECK(stl2::is_permutation(forward_iterator<const S*>(ia),
								   sentinel<const S*>(ia + sa),
								   forward_iterator<const T*>(ib),
								   sentinel<const T*>(ib + sa),
								   std::equal_to<int const>(), &S::i, &T::i) == true);
		CHECK(stl2::is_permutation(forward_iterator<const S*>(ia),
								   sentinel<const S*>(ia + sa),
								   forward_iterator<const T*>(ib + 1),
								   sentinel<const T*>(ib + sa),
								   std::equal_to<int const>(), &S::i, &T::i) == false);
		CHECK(stl2::is_permutation(forward_iterator<const S*>(ia),
								   sentinel<const S*>(ia + sa),
								   forward_iterator<const T*>(ib),
								   sentinel<const T*>(ib + sa - 1),
								   std::equal_to<int const>(), &S::i, &T::i) == false);
	}

	{
		int const a[] = {0,1,2,3};
		int const b[] = {2,3,1,0};
		CHECK(stl2::is_permutation(a, stl2::begin(b)));
		CHECK(stl2::is_permutation(stl2::begin(a), stl2::end(a), stl2::begin(b)));
	}

	return ::test_result();
}
