// cmcstl2 - A concept-enabled C++ standard library
//
//  Copyright Casey Carter 2015
//  Copyright Eric Niebler 2015
//
//  Use, modification and distribution is subject to the
//  Boost Software License, Version 1.0. (See accompanying
//  file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt)
//
// Project home: https://github.com/caseycarter/cmcstl2
//
#include <nanorange/range.hpp>
#include <iostream>
#include <vector>
#include <nanorange/view/subrange.hpp>

#ifdef NANO_HAVE_CPP17
#include <string_view>
#endif

#include "catch.hpp"

namespace {

namespace ranges = nano;

void test_range_access_ambiguity() {
//	std::vector<ranges::reverse_iterator<int*>> vri{};
    std::vector<std::reverse_iterator<int*>> vri{};
	using namespace ranges;
	(void)begin(vri);
	(void)end(vri);
	(void)cbegin(vri);
	(void)cend(vri);
	(void)rbegin(vri);
	(void)rend(vri);
	(void)crbegin(vri);
	(void)crend(vri);
}

void test_initializer_list() {
	std::initializer_list<int> il = {0,1,2};
	{
		int count = 0;
		for (auto p = ranges::begin(il), e = ranges::end(il); p != e; ++p) {
			CHECK(*p == count++);
		}
	}
	{
		int count = 3;
		for (auto p = ranges::rbegin(il), e = ranges::rend(il); p != e; ++p) {
			CHECK(*p == --count);
		}
	}
	CHECK(ranges::size(il) == std::size_t{3});
	CHECK(ranges::data(il) == &*il.begin());
	CHECK(ranges::empty(il) == false);
}

template <class T, T... Is>
void test_array(std::integer_sequence<T, Is...>) {
	T a[sizeof...(Is)] = { Is... };
	{
		int count = 0;
		for (auto p = ranges::begin(a), e = ranges::end(a); p != e; ++p) {
			CHECK(*p == count++);
		}
	}
	{
		int count = sizeof...(Is);
		for (auto p = ranges::rbegin(a), e = ranges::rend(a); p != e; ++p) {
			CHECK(*p == --count);
		}
	}
	CHECK(ranges::size(a) == sizeof...(Is));
	CHECK(ranges::data(a) == a + 0);
	CHECK(ranges::empty(a) == false);
}

namespace begin_testing {
	//template <class R>
	//concept bool CanBegin =
	//	requires(R&& r) {
	//		ranges::begin((R&&)r);
	//	};
	struct CanBegin_r {
		template <class R>
		auto requires_(R&& r) -> decltype(ranges::begin((R&&)r));
	};

	template <class R>
	constexpr bool can_begin = ranges::detail::requires_<CanBegin_r, R>;

	struct A {
		int* begin();
		int* end();
		const int* begin() const;
		const int* end() const;
	};

	struct B : A {};
	void* begin(B&) { return nullptr; }

	struct C : A {};
	void begin(C&) {}

	struct D : A {};
	char* begin(D&) { return 0; }

	void test() {
		namespace models = ranges;

		// Valid
		static_assert(can_begin<int(&)[2]>, "");
		static_assert(models::Same<decltype(ranges::begin(std::declval<int(&)[2]>())), int*>, "");
		static_assert(can_begin<const int(&)[2]>, "");
		static_assert(models::Same<decltype(ranges::begin(std::declval<const int(&)[2]>())), const int*>, "");

		// Ill-formed: array rvalue
		// FIXME: MSVC
#ifndef _MSC_VER
		static_assert(!can_begin<int(&&)[2]>, "");
#endif

		// Valid: only member begin
		static_assert(can_begin<A&>, "");
		static_assert(!can_begin<A>, "");
		static_assert(models::Same<decltype(ranges::begin(std::declval<A&>())), int*>, "");
		static_assert(can_begin<const A&>, "");
		static_assert(models::Same<decltype(ranges::begin(std::declval<const A&>())), const int*>, "");

		// Valid: Both member and non-member begin, but non-member returns non-Iterator.
		static_assert(can_begin<B&>, "");
		static_assert(models::Same<decltype(ranges::begin(std::declval<B&>())), int*>, "");
		static_assert(can_begin<const B&>, "");
		static_assert(models::Same<decltype(ranges::begin(std::declval<const B&>())), const int*>, "");

		// Valid: Both member and non-member begin, but non-member returns non-Iterator.
		static_assert(can_begin<C&>, "");
		static_assert(can_begin<const C&>, "");

		// Valid: Prefer member begin
		static_assert(can_begin<D&>, "");
		static_assert(models::Same<int*, decltype(ranges::begin(std::declval<D&>()))>, "");
		static_assert(can_begin<const D&>, "");
		static_assert(models::Same<const int*, decltype(ranges::begin(std::declval<const D&>()))>, "");

		{
			using T = std::initializer_list<int>;
			// Valid: begin accepts lvalue initializer_list
			static_assert(models::Same<const int*, decltype(ranges::begin(std::declval<T&>()))>, "");
			static_assert(models::Same<const int*, decltype(ranges::begin(std::declval<const T&>()))>, "");
		}

		static_assert(can_begin<ranges::subrange<int*, int*>&>, "");
		// FIXME: P0970
		//static_assert(can_begin<ranges::subrange<int*, int*>&&>);
	}
} // namespace begin_testing

namespace X {
	template <class T, std::size_t N>
//		requires N != 0
	struct array {
		T elements_[N];

		constexpr bool empty() const noexcept { return N == 0; }
		constexpr T* data() noexcept { return elements_; }
		constexpr const T* data() const noexcept { return elements_; }
	};

	template <class T, std::size_t N>
	constexpr T* begin(array<T, N>& a) noexcept { return a.elements_; }
	template <class T, std::size_t N>
	constexpr T* end(array<T, N>& a) noexcept { return a.elements_ + N; }
	template <class T, std::size_t N>
	constexpr const T* begin(const array<T, N>& a) noexcept { return a.elements_; }
	template <class T, std::size_t N>
	constexpr const T* end(const array<T, N>& a) noexcept { return a.elements_ + N; }
} // namespace X

using I = int*;
using CI = const int*;
static_assert(ranges::Iterator<I>, "");
static_assert(ranges::Iterator<CI>, "");

void test_string_view_p0970() {
	// FIXME: P0970
#if 0 && defined(NANO_HAVE_CPP17)
	// basic_string_views are non-dangling
	using I = ranges::iterator_t<std::string_view>;
	static_assert(ranges::Same<I, decltype(ranges::begin(std::declval<std::string_view>()))>);
	static_assert(ranges::Same<I, decltype(ranges::end(std::declval<std::string_view>()))>);
	static_assert(ranges::Same<I, decltype(ranges::begin(std::declval<const std::string_view>()))>);
	static_assert(ranges::Same<I, decltype(ranges::end(std::declval<const std::string_view>()))>);

	{
		const char hw[] = "Hello, World!";
		auto result = ranges::find(std::string_view{hw}, 'W');
		static_assert(ranges::Same<I, decltype(result)>);
		CHECK(result == std::string_view{hw}.begin() + 7);
	}
#endif
}

}

TEST_CASE("range_access") {
	using namespace ranges;

	test_range_access_ambiguity();

	static constexpr X::array<int, 4> some_ints = {{0,1,2,3}};

	constexpr auto first = begin(some_ints);
	constexpr auto last = end(some_ints);
	static_assert(Same<const CI, decltype(first)>, "");
	static_assert(Same<const CI, decltype(last)>, "");
	static_assert(first == cbegin(some_ints), "");
	static_assert(last == cend(some_ints), "");

	static_assert(noexcept(begin(some_ints)), "");
	static_assert(noexcept(end(some_ints)), "");
	static_assert(noexcept(cbegin(some_ints)), "");
	static_assert(noexcept(cend(some_ints)), "");
	static_assert(noexcept(empty(some_ints)), "");
	static_assert(noexcept(data(some_ints)), "");

	constexpr bool output = false;
	static_assert(!empty(some_ints), "");
	if (output) std::cout << '{';
	auto is_first = true;
	auto count = 0;
	for (auto&& i : some_ints) {
		CHECK(i == count++);
		if (is_first) {
			is_first = false;
		} else {
			if (output) std::cout << ", ";
		}
		if (output) std::cout << i;
	}
	if (output) std::cout << "}\n";

	test_initializer_list();
	test_array(std::make_integer_sequence<int, 3>{});
	test_array(std::make_integer_sequence<const int, 3>{});
	begin_testing::test();

	test_string_view_p0970();
}
