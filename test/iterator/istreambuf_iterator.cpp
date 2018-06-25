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
#include <nanorange/iterator/istreambuf_iterator.hpp>
#include <nanorange/iterator.hpp>
#include <sstream>
#include "../catch.hpp"
#include "../test_utils.hpp"

using namespace nano;

namespace {
	template <class charT, class traits = std::char_traits<charT>>
	void validate_one() {
		//using C = __istreambuf_iterator::cursor<charT, traits>;
		//static_assert(cursor::Cursor<C>);
		//static_assert(nano::Same<typename traits::off_type, cursor::difference_type_t<C>>);
		//static_assert(cursor::Next<C>);
		//static_assert(nano::Same<charT, cursor::iter_value_t<C>>);
		//static_assert(cursor::Readable<C>);
		//static_assert(nano::Same<charT, cursor::reference_t<C>>);
		//static_assert(cursor::Input<C>);
		//static_assert(cursor::Sentinel<C, C>);
		//static_assert(cursor::Sentinel<default_sentinel, C>);
		//static_assert(!cursor::Forward<C>);
		//static_assert(cursor::PostIncrement<C>);

		using I = istreambuf_iterator<charT, traits>;
		static_assert(nano::WeaklyIncrementable<I>, "");
		static_assert(nano::Same<typename traits::off_type, iter_difference_t<I>>, "");
		static_assert(nano::Same<charT, iter_value_t<I>>, "");
		static_assert(nano::Readable<I>, "");
		static_assert(nano::Same<charT, reference_t<I>>, "");
		static_assert(nano::Same<charT, rvalue_reference_t<I>>, "");
		static_assert(nano::Iterator<I>, "");
		static_assert(nano::Same<input_iterator_tag, iterator_category_t<I>>, "");
		static_assert(nano::InputIterator<I>, "");
		static_assert(!nano::ForwardIterator<I>, "");
		static_assert(nano::Sentinel<I, I>, "");
		static_assert(nano::Sentinel<default_sentinel, I>, "");
		static_assert(nano::Common<I, default_sentinel>, "");
		static_assert(nano::Same<I, common_type_t<I, default_sentinel>>, "");

		static_assert(nano::Same<iter_value_t<I>, typename I::value_type>, "");
		static_assert(nano::Same<iter_difference_t<I>, typename I::difference_type>, "");
		static_assert(nano::Same<input_iterator_tag, typename I::iterator_category>, "");
		static_assert(nano::Same<charT, typename I::reference>, "");
		static_assert(nano::Same<traits, typename I::traits_type>, "");
		static_assert(nano::Same<typename traits::int_type, typename I::int_type>, "");
		static_assert(nano::Same<std::basic_streambuf<charT, traits>, typename I::streambuf_type>, "");
		static_assert(nano::Same<std::basic_istream<charT, traits>, typename I::istream_type>, "");

		auto i = I{};
		auto ci = const_cast<const I&>(i);
		CHECK(ci.equal(i));
		CHECK(ci == i);
		CHECK(!(ci != i));
		CHECK(ci == default_sentinel{});
		CHECK(i == default_sentinel{});
		CHECK(!(ci != default_sentinel{}));
		CHECK(!(i != default_sentinel{}));

		//static_assert(nano::Same<decltype(i.operator->()), typename C::pointer>, "");
		//static_assert(nano::Same<decltype(i.operator++(0)), typename C::__proxy>, "");

		static_assert(nano::Constructible<I, default_sentinel>, "");
		static_assert(nano::ConvertibleTo<default_sentinel, I>, "");
		static_assert(nano::Constructible<I, std::basic_istream<charT, traits>&>, "");
		static_assert(nano::Constructible<I, std::basic_streambuf<charT, traits>*>, "");
		static_assert(nano::Constructible<I, decltype(i++)>, "");
	}

	template <class... Cs>
	void validate() {
		//(validate_one<Cs>(), ...);
        using arr_t = int[];
        (void) arr_t{ 0, (validate_one<Cs>(), 0)...};
	}
}

TEST_CASE("iter.istreambuf_iterator") {
	validate<char, wchar_t, char16_t, char32_t>();

	using I = istreambuf_iterator<char>;
	{
		static const char hw[] = "Hello, world!";
		std::istringstream is(hw);
		::check_equal(make_subrange(I{is}, default_sentinel{}),
									make_subrange(hw + 0, hw + size(hw) - 1));
	}

	{
		// Test the postincrement proxy type.
		std::istringstream is("123");
		auto i = I{is};
		CHECK(*i++ == '1');
		auto j = i++;
		CHECK(*j == '2');
		auto k = I{j};
		CHECK(*k++ == '3');
		CHECK(k == I{});
	}

	// P0898's version of istreambuf_iterator doesn't have operator->()
#if 0
	{
		// Test the operator-> proxy type.
		std::istringstream is("123");
		auto i = I{is};
		CHECK(*i.operator->().operator->() == '1');
		++i;
		CHECK(*i.operator->().operator->() == '2');
	}
#endif
}
