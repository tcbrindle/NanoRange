#include <stl2/iterator.hpp>
#include "../simple_test.hpp"

using namespace __stl2;

int main() {
	{
		using I = ext::subrange<int*, int*>;
		using CI = ext::subrange<const int*, const int*>;
		static_assert(models::View<I>);
		static_assert(models::SizedRange<I>);
		static_assert(models::ContiguousRange<I>);
		static_assert(models::BoundedRange<I>);
		static_assert(models::View<CI>);
		static_assert(models::SizedRange<CI>);
		static_assert(models::ContiguousRange<CI>);
		static_assert(models::BoundedRange<CI>);
	}

	{
		static constexpr int some_ints[] = {2, 3, 5, 7, 11, 13};
		static constexpr std::size_t n = size(some_ints);
		auto r = ext::subrange(some_ints + 0, some_ints + n);
		using R = decltype(r);
		static_assert(models::View<R>);
		static_assert(models::SizedRange<R>);
		static_assert(models::ContiguousRange<R>);
		static_assert(models::BoundedRange<R>);

		CHECK(begin(r) == some_ints + 0);
		CHECK(end(r) == some_ints + n);
		CHECK(!empty(r));
		CHECK(std::size_t(size(r)) == n);
		CHECK(data(r) == some_ints);
	}

	return test_result();
}
