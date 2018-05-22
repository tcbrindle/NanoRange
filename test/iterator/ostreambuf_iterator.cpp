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
#include <stl2/detail/iterator/ostreambuf_iterator.hpp>
#include <stl2/iterator.hpp>
#include <stl2/type_traits.hpp>
#include <sstream>
#include "../simple_test.hpp"

using namespace __stl2;

namespace {
	template <InputIterator I, Sentinel<I> S, OutputIterator<reference_t<I>> O>
	tagged_pair<tag::in(I), tag::out(O)>
	constexpr copy(I first, S last, O out) {
		for (; first != last; ++first, void(), ++out) {
			*out = *first;
		}
		return {first, out};
	}

	template <InputRange R, OutputIterator<reference_t<iterator_t<R>>> O>
	constexpr tagged_pair<tag::in(safe_iterator_t<R>), tag::out(O)>
	copy(R&& range, O out) {
		return ::copy(__stl2::begin(range), __stl2::end(range), __stl2::move(out));
	}
}

int main() {
	using I = ostreambuf_iterator<char>;
	static_assert(models::OutputIterator<I, const char&>);
	static_assert(models::Sentinel<default_sentinel, I>);
	static_assert(models::Common<I, default_sentinel>);
	static_assert(std::is_same<I, common_type_t<I, default_sentinel>>());

	{
		static const char hw[] = "Hello, world!";
		auto hw_range = ext::subrange(__stl2::begin(hw), __stl2::end(hw) - 1);
		std::ostringstream os;
		auto r = ::copy(hw_range, I{os});
		CHECK(r.out() != default_sentinel{});
		::check_equal(os.str(), hw_range);
	}

	return ::test_result();
}
