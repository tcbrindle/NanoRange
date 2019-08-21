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
#include <stl2/view/repeat.hpp>
#include <stl2/view/take.hpp>
#include <array>
#include <vector>
#include "../simple_test.hpp"

namespace ranges = __stl2;

int main() {
	{
		auto v = ranges::ext::repeat_view{42};
		using R = decltype(v);
		static_assert(ranges::View<R>);
		static_assert(ranges::RandomAccessRange<R>);
		static_assert(!ranges::ContiguousRange<R>);
		static_assert(!ranges::CommonRange<R>);
		static_assert(sizeof(v) == sizeof(int));
		CHECK(v.value() == 42);

		auto first = v.begin();
		static_assert(ranges::Same<decltype(*first), int&>);
		static_assert(ranges::Same<decltype(first.operator->()), int*>);
		CHECK(*first == 42);
		CHECK(std::addressof(*first) == std::addressof(v.value()));
		CHECK(ranges::next(first) == first);

		auto const& cv = v;
		static_assert(ranges::RandomAccessRange<const R>);
		static_assert(!ranges::ContiguousRange<const R>);
		static_assert(!ranges::CommonRange<const R>);
		CHECK(cv.value() == 42);
		CHECK(std::addressof(v.value()) == std::addressof(cv.value()));

		auto cfirst = cv.begin();
		static_assert(ranges::Same<decltype(*cfirst), const int&>);
		static_assert(ranges::Same<decltype(cfirst.operator->()), const int*>);
		CHECK(*cfirst == 42);
		CHECK(std::addressof(*cfirst) == std::addressof(cv.value()));
		CHECK(ranges::next(cfirst) == cfirst);

		CHECK(first == cfirst);
		cfirst = first;
		CHECK(first == cfirst + 1729);

		CHECK(first == first + 42);

		first[999999999] = 13;
		CHECK(*cfirst == 13);
	}
	{
		auto v = ranges::view::ext::repeat(9) | ranges::view::take(10);
		static_assert(ranges::View<decltype(v)>);
		static_assert(ranges::RandomAccessIterator<decltype(v.begin())>);
		CHECK_EQUAL(v, {9, 9, 9, 9, 9, 9, 9, 9, 9, 9});
	}
	{
		struct empty {
			bool operator==(empty const&) const noexcept { return true; }
			bool operator!=(empty const&) const noexcept { return false; }
		};

		auto e = empty{};
		auto v2 = ranges::view::ext::repeat(e) | ranges::view::take(3);
		CHECK_EQUAL(v2, {e, e, e});

		auto v3 = ranges::view::ext::repeat(std::move(e)) | ranges::view::take(3);
		CHECK_EQUAL(v2, v3);
	}

	return test_result();
}
