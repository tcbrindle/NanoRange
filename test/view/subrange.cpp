#include <nanorange/view/subrange.hpp>
#include "../catch.hpp"

using namespace nano;
namespace models = nano;

TEST_CASE("view.subrange") {
	{
		using I = nano::subrange<int*, int*>;
		using CI = nano::subrange<const int*, const int*>;
		static_assert(models::View<I>, "");
		static_assert(models::SizedRange<I>, "");
		// FIXME: ContiguousRange
		//static_assert(models::ContiguousRange<I>, "");
		static_assert(models::RandomAccessRange<I>, "");
		static_assert(models::CommonRange<I>, "");
		static_assert(models::View<CI>, "");
		static_assert(models::SizedRange<CI>, "");
		// FIXME: ContiguousRange
		//static_assert(models::ContiguousRange<CI>, "");
		static_assert(models::RandomAccessRange<CI>, "");
		static_assert(models::CommonRange<CI>, "");
	}

	{
		static constexpr int some_ints[] = {2, 3, 5, 7, 11, 13};
		static constexpr std::size_t n = size(some_ints);
		auto r = make_subrange(some_ints + 0, some_ints + n);
		using R = decltype(r);
		static_assert(models::View<R>, "");
		static_assert(models::SizedRange<R>, "");
		// FIXME: ContiguousRange
		//static_assert(models::ContiguousRange<R>);
		static_assert(models::RandomAccessRange<R>, "");
		static_assert(models::CommonRange<R>, "");

		CHECK(begin(r) == some_ints + 0);
		CHECK(end(r) == some_ints + n);
		CHECK(!empty(r));
		CHECK(std::size_t(size(r)) == n);
		// FIXME: Range acccessors
//		CHECK(data(r) == some_ints);
		CHECK(r.data() == some_ints);
	}

	{
        std::vector<int> v1{1, 2, 3, 4, 5};
        auto r = make_subrange(v1);

        CHECK(!r.empty());
        CHECK(size(r) == size(v1));

        std::vector<int> v2 = r;

        CHECK(v1 == v2);
	}

	// Deduction guides and tuple interface
#ifdef NANO_HAVE_CPP17
	{
		std::vector vec{1, 2, 3, 4, 5};
		auto r = subrange{vec};
		auto [i, s] = r;
		CHECK(i == vec.begin());
		CHECK(s == vec.end());
	}
#endif
}
