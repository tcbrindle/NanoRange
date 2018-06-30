// cmcstl2 - A concept-enabled C++ standard library
//
//  Copyright Casey Carter 2016
//  Copyright Christopher Di Bella 2016
//
//  Use, modification and distribution is subject to the
//  Boost Software License, Version 1.0. (See accompanying
//  file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt)
//
// Project home: https://github.com/caseycarter/cmcstl2
//
#include <stl2/detail/memory/uninitialized_copy.hpp>

#include <stl2/concepts.hpp>
#include <stl2/detail/algorithm/equal.hpp>
#include <stl2/detail/memory/destroy.hpp>
#include <stl2/iterator.hpp>
#include <stl2/view/repeat.hpp>
#include <stl2/view/take_exactly.hpp>
#include <algorithm>
#include <cstdint>
#include <vector>
#include "../simple_test.hpp"
#include "common.hpp"

namespace ranges = __stl2;

namespace {
	template <typename T>
	requires
		ranges::CopyConstructible<T> &&
		ranges::EqualityComparable<T>
	void uninitialized_copy_test(const Array<T>& control)
	{
		auto independent = make_buffer<T>(control.size());
		auto test = [](const auto& control, const auto& independent, const auto p) {
			const auto distance_traversed =
				std::min(
					static_cast<std::ptrdiff_t>(control.size()),
					static_cast<std::ptrdiff_t>(independent.size()));
			CHECK(p.in() == ranges::next(control.begin(), distance_traversed));
			CHECK(p.out() == ranges::next(independent.begin(), distance_traversed));
			CHECK(ranges::equal(control.begin(), p.in(), independent.begin(), p.out()));
			ranges::destroy(independent.begin(), p.out());
		};

		test(control, independent,
			ranges::uninitialized_copy(control.begin(), control.end(), independent.begin()));
		test(control, independent,
			ranges::uninitialized_copy(control.cbegin(), control.cend(), independent.cbegin()));
		test(control, independent,
			ranges::uninitialized_copy(control, independent.begin()));
		test(control, independent,
			ranges::uninitialized_copy(control, independent.cbegin()));

		auto driver = [&test](const auto& in, auto& out) {
			test(in, out, ranges::uninitialized_copy(in.begin(), in.end(), out.begin(), out.end()));
			test(in, out, ranges::uninitialized_copy(in.cbegin(), in.cend(), out.cbegin(), out.cend()));
			test(in, out, ranges::uninitialized_copy(in, out));
			test(in, out, ranges::uninitialized_copy(in, static_cast<const std::remove_reference_t<decltype(out)>&>(out)));
		};

		// check range-based when distance(rng1) == distance(rng2)
		driver(control, independent);

		// check double range-based when distance(rng1) < distance(rng2)
		auto small_input = std::array<T, 1>{control[0]};
		driver(small_input, independent);
		// // check double range-based when distance(rng1) < distance(rng2)
		auto small_output = make_buffer<T>(1);
		driver(control, small_output);

		test(control, independent,
			ranges::uninitialized_copy_n(control.begin(), control.size(), independent.begin()));
		test(control, independent,
			ranges::uninitialized_copy_n(control.cbegin(), control.size(), independent.cbegin()));
	}

	struct S {
		static constexpr int throw_after = 42;
		static int count;

		static void increment() {
			if (++count >= throw_after) {
				throw exception{};
			}
		}

		struct exception {};

		S() = default;
		S(const S&) { increment(); }
		S& operator=(const S&) & {
			increment();
			return *this;
		}
		S(S&&) = default;
		S& operator=(S&&) & = default;
	};
	constexpr int S::throw_after;
	int S::count;

	void throw_test() {
		constexpr int n = 2 * S::throw_after;
		auto control = ranges::ext::repeat_view<S>{S{}};
		auto independent = make_buffer<S>(n);
		S::count = 0;
		try {
			ranges::uninitialized_copy_n(control.begin(), n, independent.begin());
			CHECK(false);
		} catch(S::exception&) {
			CHECK(S::count == S::throw_after);
		}

		auto control2 = ranges::ext::take_exactly_view<ranges::ext::repeat_view<S>>{
			std::move(control), n
		};
		S::count = 0;
		try {
			ranges::uninitialized_copy(control2, independent.begin());
			CHECK(false);
		} catch(S::exception&) {
			CHECK(S::count == S::throw_after);
		}
	}
}

/**
 * Testing framework:
 * - test an array of fundamentals
 * - test an array of standard containers
 * - test an array of non-standard structures
 *
 * - initial array: using the default constructor
 * - second array:  using a non-default constructor
 */

int main()
{
	using Test_type_one = Array<int>;
	using Test_type_two = Array<std::vector<double>>;

	uninitialized_copy_test(Test_type_one{});
	uninitialized_copy_test(Test_type_two{});
	uninitialized_copy_test(Array<Book>{});

	uninitialized_copy_test(Test_type_one{0, 1, 2, 3, 4, 5, 6, 7});
	uninitialized_copy_test(Test_type_two{{
		{0.0, 0.1, 0.2},
		{1.0, 1.1, 1.2, 1.3, 1.4},
		{2.0, 2.1, 2.2, 2.3},
		{3.01, 3.20, 3.33, 3.4},
		{4.101, 4.102, 4.201, 4.202, 4.311},
		{5.},
		{6.1, 3.02, 6.3, 6.4, 6.5, 6.6, 6.7, 6.8, 6.9},
		std::vector<double>(1 << 12, 7.0)}});

	throw_test();

	return ::test_result();
}
