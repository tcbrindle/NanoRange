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
#include <stl2/view/split.hpp>
#include <stl2/view/empty.hpp>
#include <stl2/detail/iterator/istreambuf_iterator.hpp>
#include "../simple_test.hpp"
#include "../test_iterators.hpp"

#include <list>
#include <sstream>

namespace ranges = __stl2;

int main() {
	using namespace ranges;
	std::string greeting = "now is the time";
	std::string pattern = " ";

	{
		split_view sv{greeting, pattern};
		auto i = sv.begin();
		CHECK_EQUAL(*i, {'n','o','w'});
		++i;
		CHECK(i != sv.end());
		CHECK_EQUAL(*i, {'i','s'});
		++i;
		CHECK(i != sv.end());
		CHECK_EQUAL(*i, {'t','h','e'});
		++i;
		CHECK(i != sv.end());
		CHECK_EQUAL(*i, {'t','i','m','e'});
		++i;
		CHECK(i == sv.end());
	}

	{
		split_view sv{greeting, ' '};
		auto i = sv.begin();
		CHECK(i != sv.end());
		CHECK_EQUAL(*i, {'n','o','w'});
		++i;
		CHECK(i != sv.end());
		CHECK_EQUAL(*i, {'i','s'});
		++i;
		CHECK(i != sv.end());
		CHECK_EQUAL(*i, {'t','h','e'});
		++i;
		CHECK(i != sv.end());
		CHECK_EQUAL(*i, {'t','i','m','e'});
		++i;
		CHECK(i == sv.end());
	}

	{
		std::stringstream sin{greeting};
		auto rng = subrange{
			istreambuf_iterator<char>{sin},
			default_sentinel{}};

		split_view sv{rng, ' '};
		auto i = sv.begin();
		CHECK(i != sv.end());
		CHECK_EQUAL(*i, {'n','o','w'});
		++i;
		CHECK(i != sv.end());
		CHECK_EQUAL(*i, {'i','s'});
		++i;
		CHECK(i != sv.end());
		CHECK_EQUAL(*i, {'t','h','e'});
		++i;
		CHECK(i != sv.end());
		CHECK_EQUAL(*i, {'t','i','m','e'});
		++i;
		CHECK(i == sv.end());
	}

	{
		std::string list{"eggs,milk,,butter"};
		split_view sv{list, ','};
		auto i = sv.begin();
		CHECK(i != sv.end());
		CHECK_EQUAL(*i, {'e','g','g','s'});
		++i;
		CHECK(i != sv.end());
		CHECK_EQUAL(*i, {'m','i','l','k'});
		++i;
		CHECK(i != sv.end());
		CHECK_EQUAL(*i, view::empty<char>);
		++i;
		CHECK(i != sv.end());
		CHECK_EQUAL(*i, {'b','u','t','t','e','r'});
		++i;
		CHECK(i == sv.end());
	}

	{
		std::string list{"eggs,milk,,butter"};
		std::stringstream sin{list};
		auto rng = subrange{
			istreambuf_iterator<char>{sin},
			default_sentinel{}};
		auto sv = rng | view::split(',');
		auto i = sv.begin();
		CHECK(i != sv.end());
		CHECK_EQUAL(*i, {'e','g','g','s'});
		++i;
		CHECK(i != sv.end());
		CHECK_EQUAL(*i, {'m','i','l','k'});
		++i;
		CHECK(i != sv.end());
		CHECK_EQUAL(*i, view::empty<char>);
		++i;
		CHECK(i != sv.end());
		CHECK_EQUAL(*i, {'b','u','t','t','e','r'});
		++i;
		CHECK(i == sv.end());
	}

	{
		std::string hello("hello");
		split_view sv{hello, view::empty<char>};
		auto i = sv.begin();
		CHECK(i != sv.end());
		CHECK_EQUAL(*i, single_view{'h'});
		++i;
		CHECK(i != sv.end());
		CHECK_EQUAL(*i, single_view{'e'});
		++i;
		CHECK(i != sv.end());
		CHECK_EQUAL(*i, single_view{'l'});
		++i;
		CHECK(i != sv.end());
		CHECK_EQUAL(*i, single_view{'l'});
		++i;
		CHECK(i != sv.end());
		CHECK_EQUAL(*i, single_view{'o'});
		++i;
		CHECK(i == sv.end());
	}

	{
		std::string hello{"hello"};
		std::stringstream sin{hello};
		auto rng = subrange{
			istreambuf_iterator<char>{sin},
			default_sentinel{}};
		auto sv = view::split(rng, view::empty<char>);
		auto i = sv.begin();
		CHECK(i != sv.end());
		CHECK_EQUAL(*i, single_view{'h'});
		++i;
		CHECK(i != sv.end());
		CHECK_EQUAL(*i, single_view{'e'});
		++i;
		CHECK(i != sv.end());
		CHECK_EQUAL(*i, single_view{'l'});
		++i;
		CHECK(i != sv.end());
		CHECK_EQUAL(*i, single_view{'l'});
		++i;
		CHECK(i != sv.end());
		CHECK_EQUAL(*i, single_view{'o'});
		++i;
		CHECK(i == sv.end());
	}

	{
		std::string hello{"hello"};
		auto sv = view::split(hello, view::empty<char>);
		auto i = sv.begin();
		CHECK(i != sv.end());
		++i;
		CHECK(i != sv.end());
		++i;
		CHECK(i != sv.end());
		CHECK_EQUAL(*i, single_view{'l'});
		++i;
		CHECK(i != sv.end());
		CHECK_EQUAL(*i, single_view{'l'});
		++i;
		CHECK(i != sv.end());
		++i;
		CHECK(i == sv.end());
	}

	{
		std::string hello{"hello"};
		std::stringstream sin{hello};
		auto rng = subrange{
			istreambuf_iterator<char>{sin},
			default_sentinel{}};
		auto sv = view::split(rng, view::empty<char>);
		auto i = sv.begin();
		CHECK(i != sv.end());
		++i;
		CHECK(i != sv.end());
		++i;
		CHECK(i != sv.end());
		CHECK_EQUAL(*i, single_view{'l'});
		++i;
		CHECK(i != sv.end());
		CHECK_EQUAL(*i, single_view{'l'});
		++i;
		CHECK(i != sv.end());
		++i;
		CHECK(i == sv.end());
	}

	return test_result();
}
