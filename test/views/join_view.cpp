// cmcstl2 - A concept-enabled C++ standard library
//
//  Copyright Casey Carter 2016
//
//  Use, modification and distribution is subject to the
//  Boost Software License, Version 1.0. (See accompanying
//  file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt)
//
// Project home: https://github.com/caseycarter/cmcstl2
//
#include <nanorange/views/join.hpp>
#include <nanorange/views/iota.hpp>
#include <nanorange/views/transform.hpp>
#include <nanorange/views/filter.hpp>
#include <nanorange/algorithm/count.hpp>
#include <nanorange/algorithm/transform.hpp>

#include <memory>
#include <vector>
#include <string>
#include "../catch.hpp"
#include "../test_utils.hpp"

namespace ranges = nano::ranges;

TEST_CASE("views.join")
{
	using namespace ranges;

	{
		std::vector<std::string> vs{"this","is","his","face"};
		join_view jv{vs};
		::check_equal(jv, {'t','h','i','s','i','s','h','i','s','f','a','c','e'});
		static_assert(bidirectional_range<decltype(jv)>);
		static_assert(bidirectional_range<const decltype(jv)>);
		static_assert(common_range<decltype(jv)>);
		static_assert(common_range<const decltype(jv)>);
	}

	{
		auto rng = views::iota(0,4)
			| views::transform([](int i) {return views::iota(0,i);})
			| views::join;
		::check_equal(rng, {0,0,1,0,1,2});
		static_assert(input_range<decltype(rng)>);
		static_assert(!range<const decltype(rng)>);
		static_assert(!forward_range<decltype(rng)>);
		static_assert(!common_range<decltype(rng)>);
	}

	{
		auto rng = views::iota(0,4)
			| views::transform([](int i) {return views::iota(0,i);})
			| views::filter([](auto){ return true; })
			| views::join;
		::check_equal(rng, {0,0,1,0,1,2});
		static_assert(input_range<decltype(rng)>);
		static_assert(!range<const decltype(rng)>);
		static_assert(!forward_range<decltype(rng)>);
		static_assert(!common_range<decltype(rng)>);
	}

	{
		// https://github.com/ericniebler/stl2/issues/604
		auto rng0 = views::iota(0, 4)
			| views::transform([](int i) { return views::iota(0, i); });
		auto rng1 = ref_view{rng0};
		static_assert(random_access_range<decltype(rng1)>);
		static_assert(range<const decltype(rng1)>);
		static_assert(common_range<decltype(rng1)>);
		static_assert(random_access_range<range_reference_t<decltype(rng1)>>);
		static_assert(detail::simple_view<decltype(rng1)>);
		static_assert(!std::is_reference_v<range_reference_t<decltype(rng1)>>);
		auto rng2 = rng1 | views::join;
		::check_equal(rng2, {0,0,1,0,1,2});
		static_assert(input_range<decltype(rng2)>);
		static_assert(!range<const decltype(rng2)>);
		static_assert(!forward_range<decltype(rng2)>);
		static_assert(!common_range<decltype(rng2)>);
	}

	// Check conversion to string
	{
	    const std::vector<std::string> vec{"the", "quick", "brown", "fox"};
            auto rng = vec | views::join;
            static_assert(common_range<decltype(rng)>);
            const std::string out(rng.begin(), rng.end());
            CHECK(out == "thequickbrownfox");
	}
}
