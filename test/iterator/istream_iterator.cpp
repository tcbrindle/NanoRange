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
#include <stl2/detail/iterator/istream_iterator.hpp>
#include <stl2/iterator.hpp>
#include <stl2/type_traits.hpp>
#include <sstream>
#include "../simple_test.hpp"

using namespace __stl2;

struct Int {
	int value_;

	friend std::istream& operator>>(std::istream& is, Int& i) {
		return is >> i.value_;
	}
};

int main() {
	{
		using I = istream_iterator<int>;
		static_assert(models::WeaklyIncrementable<I>);
		static_assert(models::Same<difference_type_t<I>, std::ptrdiff_t>);
		static_assert(models::Readable<I>);
		static_assert(models::Same<value_type_t<I>, int>);
		static_assert(models::Same<reference_t<I>, const int&>);
		static_assert(models::Same<rvalue_reference_t<I>, const int&&>);
		static_assert(models::Iterator<I>);
		static_assert(models::InputIterator<I>);
		static_assert(!models::ForwardIterator<I>);

		static_assert(models::Sentinel<I, I>);
		static_assert(models::Sentinel<default_sentinel, I>);
		static_assert(models::Common<default_sentinel, I>);
		static_assert(models::Same<I, common_type_t<I, default_sentinel>>);

		static_assert(models::Same<I::difference_type, std::ptrdiff_t>);
		static_assert(models::Same<I::iterator_category, input_iterator_tag>);
		static_assert(models::Same<I::value_type, int>);
		static_assert(models::Same<I::reference, const int&>);
		static_assert(models::Same<I::pointer, const int*>);
		static_assert(models::Same<I::char_type, char>);
		static_assert(models::Same<I::traits_type, std::char_traits<char>>);
		static_assert(models::Same<I::istream_type, std::istream>);

		I{};
		I{default_sentinel{}};
		std::istringstream is("42 13");
		I i{is};
		I{i};
		static_assert(std::is_trivially_copy_constructible<I>());

		static_assert(models::Same<const int&, decltype(*i)>);
		CHECK(*i == 42);
		static_assert(models::Same<const int*, decltype(i.operator->())>);
		CHECK(&*i == i.operator->());
		static_assert(models::Same<I&, decltype(++i)>);
		CHECK(&++i == &i);
		CHECK(*i == 13);
		static_assert(models::Same<I, decltype(i++)>);
		{ I j{i}; CHECK(j == i++); }

		static_assert(models::Same<bool, decltype(i == i)>);
		CHECK(i == i);
		static_assert(models::Same<bool, decltype(default_sentinel{} == i)>);
		CHECK(default_sentinel{} == i);
		static_assert(models::Same<bool, decltype(i == default_sentinel{})>);
		CHECK(i == default_sentinel{});
		static_assert(models::Same<bool, decltype(i != i)>);
		CHECK(!(i != i));
		static_assert(models::Same<bool, decltype(default_sentinel{} != i)>);
		CHECK(!(default_sentinel{} != i));
		static_assert(models::Same<bool, decltype(i != default_sentinel{})>);
		CHECK(!(i != default_sentinel{}));
	}
	{
		std::istringstream is("5 4 3 2 1 0");
		::check_equal(
			ext::subrange(istream_iterator<int>{is}, default_sentinel{}),
				{5, 4, 3, 2, 1, 0});
	}
	{
		std::istringstream is("0.9 1.8 2.4 3.3");
		::check_equal(
			ext::subrange(istream_iterator<double>{is}, default_sentinel{}),
				{0.9, 1.8, 2.4, 3.3});
	}

	{
		std::istringstream is("5 4 3 2 1 0");
		auto i = istream_iterator<Int>{is};
		for (auto n = 5; i != default_sentinel{} && n >= 0; --n, ++i) {
			CHECK(i->value_ == n);
		}
	}

	return ::test_result();
}
