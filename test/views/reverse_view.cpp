// cmcstl2 - A concept-enabled C++ standard library
//
//  Copyright Eric Niebler 2017
//
//  Use, modification and distribution is subject to the
//  Boost Software License, Version 1.0. (See accompanying
//  file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt)
//
// Project home: https://github.com/caseycarter/cmcstl2
//
#include <nanorange/views/reverse.hpp>
#include <nanorange/views/counted.hpp>
#include <nanorange/views/iota.hpp>
#include <nanorange/views/filter.hpp>
#include "../catch.hpp"
#include "../test_iterators.hpp"
#include "../test_utils.hpp"

namespace ranges = nano::ranges;

TEST_CASE("views.reverse") {
	using namespace ranges;
	{
		int rg[] = {0,1,2,3,4,5,6,7,8,9};
		auto x = rg | views::reverse;
		::check_equal(x, {9,8,7,6,5,4,3,2,1,0});
		static_assert(view<decltype(x)>, "");
		static_assert(range<const decltype(x)>, "");
		static_assert(sized_range<decltype(x)>, "");
		static_assert(common_range<decltype(x)>, "");
		static_assert(random_access_range<decltype(x)>, "");
	}
	{
		int rg[] = {0,1,2,3,4,5,6,7,8,9};
		auto x = views::counted(::bidirectional_iterator(rg), 5) | views::reverse;
		//auto x = views::reverse(views::counted(bidirectional_iterator<int*>(rg), 5));
		::check_equal(x, {4,3,2,1,0});
		static_assert(view<decltype(x)>, "");
		static_assert(!range<const decltype(x)>, "");
		static_assert(sized_range<decltype(x)>, "");
		static_assert(common_range<decltype(x)>, "");
		static_assert(bidirectional_range<decltype(x)>, "");
		static_assert(!random_access_range<decltype(x)>, "");
	}
    {
        auto ints = views::iota(0, 5)
                    | views::filter([] (auto) { return true; })
                    | views::reverse;
        ::check_equal(ints, {4, 3, 2, 1, 0});
    }
    {
        int rg[] = {0,1,2,3,4,5,6,7,8,9};
        auto x = rg | views::reverse | views::reverse;
        static_assert(std::is_same_v<decltype(x), ref_view<int[10]>>);
        ::check_equal(x, rg);
    }
    {
        int rg[] = {0,1,2,3,4,5,6,7,8,9};
        auto s = subrange{reverse_iterator{rg + 10}, reverse_iterator{rg}};
        auto x = s | views::reverse;
        static_assert(std::is_same_v<decltype(x), subrange<int*, int*, subrange_kind::sized>>);
        ::check_equal(rg, x);
    }
}
