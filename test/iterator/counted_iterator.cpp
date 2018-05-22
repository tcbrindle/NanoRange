// cmcstl2 - A concept-enabled C++ standard library
//
//  Copyright Eric Niebler 2014
//  Copyright Casey Carter 2015
//
//  Use, modification and distribution is subject to the
//  Boost Software License, Version 1.0. (See accompanying
//  file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt)
//
// Project home: https://github.com/caseycarter/cmcstl2
//
#include <stl2/iterator.hpp>
#include <stl2/type_traits.hpp>
#include <stl2/algorithm.hpp>
#include <list>
#include "../test_iterators.hpp"
#include "../simple_test.hpp"

namespace ranges = std::experimental::ranges;

constexpr bool test_constexpr() {
	int some_ints[] = {0,1,2,3};
	constexpr int n = ranges::size(some_ints);
	static_assert(n >= 4);

	{ ranges::counted_iterator<int*> unused{}; (void)unused; }
	auto first = ranges::make_counted_iterator(ranges::begin(some_ints), n);
	if (first.base() != ranges::begin(some_ints)) return false;
	if (first.count() != n) return false;
	auto last = ranges::make_counted_iterator(ranges::end(some_ints), 0);
	if (last.base() != ranges::end(some_ints)) return false;

	if (first == last) return false;
	if (!(first != last)) return false;
	if (!(first < last)) return false;
	if (first > last) return false;
	if (!(first <= last)) return false;
	if (first >= last) return false;
	if (last - first != n) return false;
	if (first - last != -n) return false;
	if (*(first + 2) != 2) return false;

	{
		ranges::counted_iterator<int const*> tmp{first};
		tmp = first;
		if (tmp == last) return false;
		if (!(tmp != last)) return false;
		if (!(tmp < last)) return false;
		if (tmp > last) return false;
		if (!(tmp <= last)) return false;
		if (tmp >= last) return false;
		if (last - tmp != n) return false;
		if (tmp - last != -n) return false;
	}

	auto end = ranges::default_sentinel{};

	if (first == end) return false;
	if (end == first) return false;
	if (last != end) return false;
	if (end != last) return false;
	if (end - first != n) return false;
	if (first - end != -n) return false;

	auto pos = first;
	++pos;
	if (pos != first + 1) return false;
	--pos;
	if (pos != last - n) return false;
	pos += n;
	if (pos != last) return false;
	pos -= n;
	if (pos != first) return false;
	if (pos + 2 != 2 + pos) return false;

	if (first[3] != 3) return false;

	ranges::advance(pos, 2);
	if (pos != first + 2) return false;

	return true;
}
static_assert(test_constexpr());

int main()
{
	using namespace std::experimental::ranges;

	{
		int rgi[] = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10};
		auto i = make_counted_iterator(forward_iterator<int*>{rgi}, size(rgi));
		static_assert(std::is_same<decltype(i),counted_iterator<forward_iterator<int*>>>());
		static_assert(models::SizedSentinel<default_sentinel, decltype(i)>);
		CHECK(static_cast<std::size_t>(default_sentinel{} - i) == size(rgi));
		CHECK(&*i.base() == begin(rgi));
		CHECK(std::size_t(i.count()) == size(rgi));
		CHECK(std::size_t(distance(i, default_sentinel{})) == size(rgi));

		counted_iterator<forward_iterator<const int*>> j{i};
		using C = common_iterator<decltype(i), default_sentinel>;
		CHECK(std::equal(C{i}, C{default_sentinel{}}, rgi));
	}

	{
		std::list<int> l;
		auto a = make_counted_iterator(l.begin(), 0);
		auto b = make_counted_iterator(l.cbegin(), 0);
		static_assert(std::is_same<common_type_t<decltype(a), decltype(b)>, decltype(b)>());
		CHECK((a - a) == 0);
		CHECK((b - b) == 0);
		CHECK((a - b) == 0);
		CHECK((b - a) == 0);
	}

	{
		counted_iterator<char*> c{nullptr, 0};
		counted_iterator<char const*> d{c};
		static_assert(!models::Assignable<decltype(c)&, decltype(d)>);
		CHECK((c - c) == 0);
		CHECK((d - d) == 0);
		CHECK((c - d) == 0);
		CHECK((d - c) == 0);
	}

	{
		int rgi[] = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10};
		counted_iterator<output_iterator<int*>> e{output_iterator<int*>{rgi}, 10};
		fill(e, default_sentinel{}, 0);
		int expected[] = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10};
		CHECK(std::equal(rgi, rgi + size(rgi), expected));
		// Make sure advance compiles
		advance(e, 4);
		CHECK(e.base().base() == rgi + 4);
		CHECK(e.count() == 10 - 4);
	}

	return ::test_result();
}
