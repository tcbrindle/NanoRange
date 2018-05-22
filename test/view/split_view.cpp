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

#include <sstream>

namespace ranges = __stl2;

namespace {
}

#include <list>

int main() {
	using namespace ranges;
	std::string greeting = "now is the time";
	std::string pattern = " ";

	{
		ext::split_view sv{greeting, pattern};
		auto i = sv.begin();
		check_equal(*i, {'n','o','w'});
		++i;
		CHECK(i != sv.end());
		check_equal(*i, {'i','s'});
		++i;
		CHECK(i != sv.end());
		check_equal(*i, {'t','h','e'});
		++i;
		CHECK(i != sv.end());
		check_equal(*i, {'t','i','m','e'});
		++i;
		CHECK(i == sv.end());
	}

	{
		ext::split_view sv{greeting, ' '};
		auto i = sv.begin();
		CHECK(i != sv.end());
		check_equal(*i, {'n','o','w'});
		++i;
		CHECK(i != sv.end());
		check_equal(*i, {'i','s'});
		++i;
		CHECK(i != sv.end());
		check_equal(*i, {'t','h','e'});
		++i;
		CHECK(i != sv.end());
		check_equal(*i, {'t','i','m','e'});
		++i;
		CHECK(i == sv.end());
	}

	{
		std::stringstream sin{greeting};
		auto rng = ext::subrange{
			istreambuf_iterator<char>{sin},
			istreambuf_iterator<char>{}};

		ext::split_view sv{rng, ' '};
		auto i = sv.begin();
		CHECK(i != sv.end());
		check_equal(*i, {'n','o','w'});
		++i;
		CHECK(i != sv.end());
		check_equal(*i, {'i','s'});
		++i;
		CHECK(i != sv.end());
		check_equal(*i, {'t','h','e'});
		++i;
		CHECK(i != sv.end());
		check_equal(*i, {'t','i','m','e'});
		++i;
		CHECK(i == sv.end());
	}

	{
		std::string list{"eggs,milk,,butter"};
		ext::split_view sv{list, ','};
		auto i = sv.begin();
		CHECK(i != sv.end());
		check_equal(*i, {'e','g','g','s'});
		++i;
		CHECK(i != sv.end());
		check_equal(*i, {'m','i','l','k'});
		++i;
		CHECK(i != sv.end());
		check_equal(*i, view::empty<char>);
		++i;
		CHECK(i != sv.end());
		check_equal(*i, {'b','u','t','t','e','r'});
		++i;
		CHECK(i == sv.end());
	}

	{
		std::string list{"eggs,milk,,butter"};
		std::stringstream sin{list};
		auto rng = ext::subrange{
			istreambuf_iterator<char>{sin},
			istreambuf_iterator<char>{}};
		auto sv = rng | view::split(',');
		auto i = sv.begin();
		CHECK(i != sv.end());
		check_equal(*i, {'e','g','g','s'});
		++i;
		CHECK(i != sv.end());
		check_equal(*i, {'m','i','l','k'});
		++i;
		CHECK(i != sv.end());
		check_equal(*i, view::empty<char>);
		++i;
		CHECK(i != sv.end());
		check_equal(*i, {'b','u','t','t','e','r'});
		++i;
		CHECK(i == sv.end());
	}

	{
		std::string hello("hello");
		ext::split_view sv{hello, view::empty<char>};
		auto i = sv.begin();
		CHECK(i != sv.end());
		check_equal(*i, ext::single_view{'h'});
		++i;
		CHECK(i != sv.end());
		check_equal(*i, ext::single_view{'e'});
		++i;
		CHECK(i != sv.end());
		check_equal(*i, ext::single_view{'l'});
		++i;
		CHECK(i != sv.end());
		check_equal(*i, ext::single_view{'l'});
		++i;
		CHECK(i != sv.end());
		check_equal(*i, ext::single_view{'o'});
		++i;
		CHECK(i == sv.end());
	}

	{
		std::string hello{"hello"};
		std::stringstream sin{hello};
		auto rng = ext::subrange{
			istreambuf_iterator<char>{sin},
			istreambuf_iterator<char>{}};
		auto sv = view::split(rng, view::empty<char>);
		auto i = sv.begin();
		CHECK(i != sv.end());
		check_equal(*i, ext::single_view{'h'});
		++i;
		CHECK(i != sv.end());
		check_equal(*i, ext::single_view{'e'});
		++i;
		CHECK(i != sv.end());
		check_equal(*i, ext::single_view{'l'});
		++i;
		CHECK(i != sv.end());
		check_equal(*i, ext::single_view{'l'});
		++i;
		CHECK(i != sv.end());
		check_equal(*i, ext::single_view{'o'});
		++i;
		CHECK(i == sv.end());
	}

	return test_result();
}
