// cmcstl2 - A concept-enabled C++ standard library
//
//  Copyright Casey Carter 2015
//
//  Use, modification and distribution is subject to the
//  Boost Software License, Version 1.0. (See accompanying
//  file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt)
//
// Project home: https://github.com/caseycarter/cmcstl2
//
#include <nanorange/iterator/ostreambuf_iterator.hpp>
#include <nanorange/iterator/unreachable.hpp>
//#include <stl2/iterator.hpp>
//#include <stl2/type_traits.hpp>
#include <nanorange/ranges.hpp>
#include <nanorange/algorithm/equal.hpp>
#include <sstream>
#include "../catch.hpp"
#include "../test_utils.hpp"

namespace __stl2 = nano::ranges;
using namespace __stl2;

namespace {
	template <typename I, typename S, typename O>
	//tagged_pair<tag::in(I), tag::out(O)>
	std::pair<I, O>
	constexpr copy(I first, S last, O out) {
		for (; first != last; ++first, void(), ++out) {
			*out = *first;
		}
		return {first, out};
	}

	template <typename R, typename O>
	constexpr
	//tagged_pair<tag::in(safe_iterator_t<R>), tag::out(O)>
	std::pair<safe_iterator_t<R>, O>
	copy(R&& range, O out) {
		return ::copy(std::begin(range), std::end(range), std::move(out));
	}
}

TEST_CASE("iter.ostreambuf_iterator") {
	using I = ostreambuf_iterator<char>;
	static_assert(OutputIterator<I, const char&>, "");
	// FIXME: ostreambuf_iterator isn't EqualityComparableWith default_sentinel?
	//static_assert(Sentinel<default_sentinel, I>);
	//static_assert(Common<I, default_sentinel>);
	//static_assert(std::is_same<I, common_type_t<I, default_sentinel>>());
	// We'll use unreachable instead
	static_assert(Sentinel<unreachable, I>, "");

	{
		static const char hw[] = "Hello, world!";
		auto hw_range = make_range(__stl2::begin(hw), __stl2::end(hw) - 1);
		std::ostringstream os;
		auto r = ::copy(hw_range, I{os});
		//CHECK(r.out() != default_sentinel{});
		CHECK(r.second != unreachable{});
		CHECK(equal(os.str(), hw_range));
		//::check_equal(os.str(), hw_range);
	}
}
