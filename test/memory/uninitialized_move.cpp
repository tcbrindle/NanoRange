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
#include <stl2/detail/memory/uninitialized_move.hpp>
#include <stl2/concepts.hpp>
#include <stl2/detail/algorithm/all_of.hpp>
#include <stl2/detail/algorithm/count_if.hpp>
#include <stl2/detail/algorithm/equal.hpp>
#include <stl2/detail/memory/destroy.hpp>
#include <stl2/detail/span.hpp>
#include <stl2/iterator.hpp>
#include <stl2/view/repeat.hpp>
#include <stl2/view/take_exactly.hpp>
#include <algorithm>
#include <cstdint>
#include <memory>
#include <vector>
#include "../simple_test.hpp"
#include "common.hpp"

namespace ranges = __stl2;
using ranges::ext::span;

namespace {
	template <ranges::InputRange Rng>
	requires requires {
		typename ranges::value_type_t<Rng>;
		&ranges::value_type_t<Rng>::empty;
		requires ranges::Invocable<
			decltype(&ranges::value_type_t<Rng>::empty),
			ranges::reference_t<ranges::iterator_t<const Rng>>>;
	}
	bool empty(const Rng& rng, const std::ptrdiff_t n) {
		return ranges::all_of(ranges::make_counted_iterator(rng.begin(), n), ranges::default_sentinel{},
			&ranges::value_type_t<Rng>::empty);
	}

	template <ranges::InputRange Rng>
	requires requires {
		typename ranges::value_type_t<Rng>;
		requires std::is_fundamental<ranges::value_type_t<Rng>>::value;
	}
	bool empty(const Rng&, const std::ptrdiff_t) {
		return true;
	}

	template <ranges::Copyable T>
	void uninitialized_move_test(const Array<T>& control)
	{
		auto independent = make_buffer<T>(control.size());
		auto to_move = control;
		auto test = [&control](const auto& to_move, const auto& independent, const auto& p) {
			const auto distance_traversed =
				std::min(
					static_cast<std::ptrdiff_t>(to_move.size()),
					static_cast<std::ptrdiff_t>(independent.size()));
			CHECK(::empty(to_move, distance_traversed));
			CHECK(p.in() == ranges::next(to_move.begin(), distance_traversed));
			CHECK(p.out() == ranges::next(independent.begin(), distance_traversed));

			CHECK(ranges::equal(control.begin(), control.begin() + distance_traversed,
					independent.begin(), p.out()));
			ranges::destroy(independent.begin(), p.out());
		};

		test(to_move, independent,
			ranges::uninitialized_move(to_move.begin(), to_move.end(), independent.begin()));

		to_move = control; // to_move.begin(), not to_move.cbegin()
		test(to_move, independent,
			ranges::uninitialized_move(to_move.begin(), to_move.end(), independent.cbegin()));

		to_move = control;
		test(to_move, independent,
			ranges::uninitialized_move(to_move, independent.begin()));

		to_move = control;
		test(to_move, independent,
			ranges::uninitialized_move(to_move, independent.cbegin()));

		auto driver = [&test](auto& in, auto& out) {
			auto to_move = in;
			test(to_move, out,
				ranges::uninitialized_move(to_move.begin(), to_move.end(), out.begin(), out.end()));

			to_move = in; // to_move.begin(), not to_move.cbegin()
			test(to_move, out,
				ranges::uninitialized_move(to_move.begin(), to_move.end(), out.cbegin(), out.cend()));

			to_move = in;
			test(to_move, out,
				ranges::uninitialized_move(to_move, out));

			to_move = in;
			test(to_move, out,
				ranges::uninitialized_move(to_move, static_cast<const raw_buffer<T>&>(out)));
		};

		// check range-based when distance(rng1) == distance(rng2)
		driver(control, independent);

		// check range-based when distance(rng1) < distance(rng2)
		auto small_input = std::array<T, 1>{control[0]};
		driver(small_input, independent);

		// check range-based when distance(rng1) > distance(rng2)
		auto small_output = make_buffer<T>(1);
		driver(control, small_output);

		to_move = control;
		test(to_move, independent,
			ranges::uninitialized_move_n(to_move.begin(), to_move.size(), independent.begin()));

		to_move = control; // to_move.begin(), not to_move.cbegin()
		test(to_move, independent,
			ranges::uninitialized_move_n(to_move.begin(), to_move.size(), independent.cbegin()));
	}

	using Move_only_t = Array<std::unique_ptr<std::string>>;
	void uninitialized_move_test(Move_only_t first)
	{
		auto test = [](const auto& s, const auto& d, const auto& p) {
			CHECK(p.in() == s.end());
			CHECK(p.out() == d.end());
			auto n = ranges::count_if(s.begin(), p.in(), [](const auto& i){ return !i; });
			CHECK(static_cast<std::size_t>(n) == static_cast<std::size_t>(s.size()));
		};

		auto second = make_buffer<Move_only_t::value_type>(first.size());
		test(first, second, ranges::uninitialized_move(first.begin(), first.end(), second.begin()));
		test(second, first, ranges::uninitialized_move(second.begin(), second.end(), first.cbegin()));
		test(first, second, ranges::uninitialized_move(first, second.begin()));
		test(second, first, ranges::uninitialized_move(second, first.cbegin()));
		test(first, second, ranges::uninitialized_move(first.begin(), first.end(),
			second.begin(), second.end()));
		test(second, first, ranges::uninitialized_move(second.begin(), second.end(),
			first.cbegin(), first.cend()));
		test(first, second, ranges::uninitialized_move(first, second));
		test(second, first, ranges::uninitialized_move(second, static_cast<const Move_only_t&>(first)));
		test(first, second, ranges::uninitialized_move_n(first.begin(), first.size(), second.cbegin()));
		test(second, first, ranges::uninitialized_move_n(second.begin(), second.size(), first.begin()));
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
		S(const S&) = default;
		S& operator=(const S&) & = default;
		S(const S&&) { increment(); }
		S& operator=(const S&&) & {
			increment();
			return *this;
		}
	};
	constexpr int S::throw_after;
	int S::count;

	void throw_test() {
		constexpr int n = 2 * S::throw_after;
		auto control = ranges::ext::repeat_view<S>{S{}};
		auto independent = make_buffer<S>(n);
		S::count = 0;
		try {
			ranges::uninitialized_move_n(control.begin(), n, independent.begin());
			CHECK(false);
		} catch(S::exception&) {
			CHECK(S::count == S::throw_after);
		}
		S::count = 0;

		auto control2 = ranges::ext::take_exactly_view<ranges::ext::repeat_view<S>>{
			std::move(control), n
		};
		S::count = 0;
		try {
			ranges::uninitialized_move(control2, independent.begin());
			CHECK(false);
		} catch(S::exception&) {
			CHECK(S::count == S::throw_after);
		}
		S::count = 0;
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
	uninitialized_move_test(Test_type_one{});
	uninitialized_move_test(Test_type_two{});
	uninitialized_move_test(Array<Book>{});

	uninitialized_move_test(Test_type_one{0, 1, 2, 3, 4, 5, 6, 7});
	uninitialized_move_test(Test_type_two{{
		{0.0, 0.1, 0.2},
		{1.0, 1.1, 1.2, 1.3, 1.4},
		{2.0, 2.1, 2.2, 2.3},
		{3.01, 3.20, 3.33, 3.4},
		{4.101, 4.102, 4.201, 4.202, 4.311},
		{5.},
		{6.1, 3.02, 6.3, 6.4, 6.5, 6.6, 6.7, 6.8, 6.9},
		std::vector<double>(1 << 12, 7.0)}});

	uninitialized_move_test(Move_only_t{
		std::make_unique<std::string>("0"),
		std::make_unique<std::string>("0"),
		std::make_unique<std::string>("0"),
		std::make_unique<std::string>("0"),
		std::make_unique<std::string>("0"),
		std::make_unique<std::string>("0"),
		std::make_unique<std::string>("0"),
		std::make_unique<std::string>("0")});

	throw_test();

	return ::test_result();
}
