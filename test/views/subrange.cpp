#include <nanorange/views/subrange.hpp>
#include "../catch.hpp"

using namespace nano;
namespace models = nano;

TEST_CASE("views.subrange") {
	{
		using I = nano::subrange<int*, int*>;
		using CI = nano::subrange<const int*, const int*>;
		static_assert(models::view<I>, "");
		static_assert(models::sized_range<I>, "");
		static_assert(models::contiguous_range<I>, "");
		static_assert(models::random_access_range<I>, "");
		static_assert(models::common_range<I>, "");
		static_assert(models::view<CI>, "");
		static_assert(models::sized_range<CI>, "");
		static_assert(models::contiguous_range<CI>, "");
		static_assert(models::random_access_range<CI>, "");
		static_assert(models::common_range<CI>, "");
	}

	{
		static constexpr int some_ints[] = {2, 3, 5, 7, 11, 13};
		static constexpr std::size_t n = size(some_ints);
		auto r = subrange(some_ints + 0, some_ints + n);
		using R = decltype(r);
		static_assert(models::view<R>, "");
		static_assert(models::sized_range<R>, "");
		static_assert(models::contiguous_range<R>, "");
		static_assert(models::random_access_range<R>, "");
		static_assert(models::common_range<R>, "");

		CHECK(begin(r) == some_ints + 0);
		CHECK(end(r) == some_ints + n);
		CHECK(!empty(r));
		CHECK(std::size_t(size(r)) == n);
		CHECK(data(r) == some_ints);
	}

	{
        std::vector<int> v1{1, 2, 3, 4, 5};
        auto r = subrange(v1);

        CHECK(!r.empty());
        CHECK(size(r) == size(v1));

//        std::vector<int> v2 = r;

//        CHECK(v1 == v2);
	}

	// Deduction guides and tuple interface
	{
		std::vector vec{1, 2, 3, 4, 5};
		auto r = subrange{vec};
		auto [i, s] = r;
		CHECK(i == vec.begin());
		CHECK(s == vec.end());
	}
}
