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
#include <stl2/view/join.hpp>
#include <stl2/view/iota.hpp>
#include <stl2/view/transform.hpp>
#include <stl2/view/filter.hpp>
#include <stl2/detail/algorithm/count.hpp>
#include <stl2/detail/algorithm/transform.hpp>
#include <stl2/detail/iterator/insert_iterators.hpp>
#include <memory>
#include <vector>
#include <string>
#include "../simple_test.hpp"

namespace ranges = __stl2;

int main()
{
	using namespace ranges;

	{
		std::vector<std::string> vs{"this","is","his","face"};
		ext::join_view jv{vs};
		::check_equal(jv, {'t','h','i','s','i','s','h','i','s','f','a','c','e'});
		static_assert(models::BidirectionalRange<decltype(jv)>);
		static_assert(models::BidirectionalRange<const decltype(jv)>);
		static_assert(models::BoundedRange<decltype(jv)>);
		static_assert(models::BoundedRange<const decltype(jv)>);
	}

	{
		auto rng = view::iota(0,4)
			| view::transform([](int i) {return view::iota(0,i);})
			| view::join;
		::check_equal(rng, {0,0,1,0,1,2});
		static_assert(models::InputRange<decltype(rng)>);
		static_assert(!models::Range<const decltype(rng)>);
		static_assert(!models::ForwardRange<decltype(rng)>);
		static_assert(!models::BoundedRange<decltype(rng)>);
	}

	{
		auto rng = view::iota(0,4)
			| view::transform([](int i) {return view::iota(0,i);})
			| view::filter([](auto){ return true; })
			| view::join;
		::check_equal(rng, {0,0,1,0,1,2});
		static_assert(models::InputRange<decltype(rng)>);
		static_assert(!models::Range<const decltype(rng)>);
		static_assert(!models::ForwardRange<decltype(rng)>);
		static_assert(!models::BoundedRange<decltype(rng)>);
	}

	return ::test_result();
}
