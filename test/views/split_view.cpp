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
#include <nanorange/views/split.hpp>
#include <nanorange/views/empty.hpp>
#include <nanorange/iterator/istreambuf_iterator.hpp>
#include <nanorange/views/join.hpp>
#include <nanorange/views/common.hpp>
#include <nanorange/views/transform.hpp>
#include <nanorange/algorithm/equal.hpp>
#include "../catch.hpp"
#include "../test_utils.hpp"

#include <list>
#include <sstream>

namespace ranges = nano::ranges;

namespace {

constexpr bool test_split_join()
{
    const std::string_view str = "The quick brown fox";
    const std::string_view out = "Thequickbrownfox";

    return nano::equal(str | nano::views::split(' ') | nano::views::join, out);
}


}

TEST_CASE("views.split") {
	using namespace ranges;
	std::string greeting = "now is the time";
	std::string pattern = " ";

	{
		split_view sv{greeting, pattern};
		auto i = sv.begin();
		::check_equal(*i, {'n','o','w'});
		++i;
		CHECK(i != sv.end());
		::check_equal(*i, {'i','s'});
		++i;
		CHECK(i != sv.end());
		::check_equal(*i, {'t','h','e'});
		++i;
		CHECK(i != sv.end());
		::check_equal(*i, {'t','i','m','e'});
		++i;
		CHECK(i == sv.end());
	}

	{
		split_view sv{greeting, ' '};
		auto i = sv.begin();
		CHECK(i != sv.end());
		::check_equal(*i, {'n','o','w'});
		++i;
		CHECK(i != sv.end());
		::check_equal(*i, {'i','s'});
		++i;
		CHECK(i != sv.end());
		::check_equal(*i, {'t','h','e'});
		++i;
		CHECK(i != sv.end());
		::check_equal(*i, {'t','i','m','e'});
		++i;
		CHECK(i == sv.end());
	}

	{
		std::stringstream sin{greeting};
		auto rng = subrange{
			istreambuf_iterator<char>{sin},
			default_sentinel};

		split_view sv{rng, ' '};
		auto i = sv.begin();
		CHECK(i != sv.end());
		::check_equal(*i, {'n','o','w'});
		++i;
		CHECK(i != sv.end());
		::check_equal(*i, {'i','s'});
		++i;
		CHECK(i != sv.end());
		::check_equal(*i, {'t','h','e'});
		++i;
		CHECK(i != sv.end());
		::check_equal(*i, {'t','i','m','e'});
		++i;
		CHECK(i == sv.end());
	}

	{
		std::string list{"eggs,milk,,butter"};
		split_view sv{list, ','};
		auto i = sv.begin();
		CHECK(i != sv.end());
		::check_equal(*i, {'e','g','g','s'});
		++i;
		CHECK(i != sv.end());
		::check_equal(*i, {'m','i','l','k'});
		++i;
		CHECK(i != sv.end());
		::check_equal(*i, views::empty<char>);
		++i;
		CHECK(i != sv.end());
		::check_equal(*i, {'b','u','t','t','e','r'});
		++i;
		CHECK(i == sv.end());
	}

	{
		std::string list{"eggs,milk,,butter"};
		std::stringstream sin{list};
		auto rng = subrange{
			istreambuf_iterator<char>{sin},
			default_sentinel};
		auto sv = rng | views::split(',');
		auto i = sv.begin();
		CHECK(i != sv.end());
		::check_equal(*i, {'e','g','g','s'});
		++i;
		CHECK(i != sv.end());
		::check_equal(*i, {'m','i','l','k'});
		++i;
		CHECK(i != sv.end());
		::check_equal(*i, views::empty<char>);
		++i;
		CHECK(i != sv.end());
		::check_equal(*i, {'b','u','t','t','e','r'});
		++i;
		CHECK(i == sv.end());
	}

	{
		std::string hello("hello");
		split_view sv{hello, views::empty<char>};
		auto i = sv.begin();
		CHECK(i != sv.end());
		::check_equal(*i, single_view{'h'});
		++i;
		CHECK(i != sv.end());
		::check_equal(*i, single_view{'e'});
		++i;
		CHECK(i != sv.end());
		::check_equal(*i, single_view{'l'});
		++i;
		CHECK(i != sv.end());
		::check_equal(*i, single_view{'l'});
		++i;
		CHECK(i != sv.end());
		::check_equal(*i, single_view{'o'});
		++i;
		CHECK(i == sv.end());
	}

	{
		std::string hello{"hello"};
		std::stringstream sin{hello};
		auto rng = subrange{
			istreambuf_iterator<char>{sin},
			default_sentinel};
		auto sv = views::split(rng, views::empty<char>);
		auto i = sv.begin();
		CHECK(i != sv.end());
		::check_equal(*i, single_view{'h'});
		++i;
		CHECK(i != sv.end());
		::check_equal(*i, single_view{'e'});
		++i;
		CHECK(i != sv.end());
		::check_equal(*i, single_view{'l'});
		++i;
		CHECK(i != sv.end());
		::check_equal(*i, single_view{'l'});
		++i;
		CHECK(i != sv.end());
		::check_equal(*i, single_view{'o'});
		++i;
		CHECK(i == sv.end());
	}

	{
		std::string hello{"hello"};
		auto sv = views::split(hello, views::empty<char>);
		auto i = sv.begin();
		CHECK(i != sv.end());
		++i;
		CHECK(i != sv.end());
		++i;
		CHECK(i != sv.end());
		::check_equal(*i, single_view{'l'});
		++i;
		CHECK(i != sv.end());
		::check_equal(*i, single_view{'l'});
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
			default_sentinel};
		auto sv = rng | views::split(views::empty<char>);
		auto i = sv.begin();
		CHECK(i != sv.end());
		++i;
		CHECK(i != sv.end());
		++i;
		CHECK(i != sv.end());
		::check_equal(*i, single_view{'l'});
		++i;
		CHECK(i != sv.end());
		::check_equal(*i, single_view{'l'});
		++i;
		CHECK(i != sv.end());
		++i;
		CHECK(i == sv.end());
	}

	// Check conversion to vector
	{
	    const auto to_string = [](auto&& r) {
	        auto c = std::forward<decltype(r)>(r) | views::common;
	        return std::string(begin(c), end(c));
	    };
	    const std::string hello = "Hello World";
	    auto rng = hello
	        | views::split(' ')
	        | views::transform(to_string);
	    static_assert(common_range<decltype(rng)>);
	    const auto vec = std::vector<std::string>(rng.begin(), rng.end());
	    ::check_equal(vec, {"Hello", "World"});
	}

#ifdef _MSC_VER
	// MSVC is insufficiently constexpr-y
	CHECK(test_split_join());
#else
	static_assert(test_split_join());
#endif
}
