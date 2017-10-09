
#include "catch.hpp"

#include <nanorange.hpp>

namespace rng = nanorange;

TEST_CASE("is_partitioned()")
{
    const auto is_even = [](int i) { return i % 2 == 0; };
    const auto vec = [&] {
        std::vector<int> vec{1, 2, 3, 4, 5, 6};
        std::partition(vec.begin(), vec.end(), is_even);
        return vec;
    }();

    SECTION("with iterators") {
        REQUIRE(rng::is_partitioned(vec.begin(), vec.end(), is_even));
    }

    SECTION("with range") {
        REQUIRE(rng::is_partitioned(vec, is_even));
    }
}

TEST_CASE("partition()")
{
    std::vector<int> vec{1, 2, 3, 4, 5, 6};
    const auto is_even = [](int i) { return i % 2 == 0; };

    SECTION("with iterators") {
        rng::partition(vec.begin(), vec.end(), is_even);
    }

    SECTION("witn range") {
        rng::partition(vec, is_even);
    }

    REQUIRE(std::is_partitioned(vec.begin(), vec.end(), is_even));
}

TEST_CASE("partition_copy()")
{
    const std::vector<int> src{1, 2, 3, 4, 5, 6, 7, 8};
    std::ostringstream evens;
    std::ostringstream odds;
    const auto is_even = [] (int i) { return i % 2 == 0; };

    SECTION("with iterators") {
        rng::partition_copy(src.begin(), src.end(),
                            rng::ostream_iterator<int>(evens), rng::ostream_iterator<int>(odds),
                            is_even);
    }

    SECTION("with range") {
        rng::partition_copy(src,
                            rng::ostream_iterator<int>(evens), rng::ostream_iterator<int>(odds),
                            is_even);
    }

    REQUIRE(evens.str() == "2468");
    REQUIRE(odds.str() == "1357");
}

TEST_CASE("stable_partition()")
{
    std::vector<int> src{1, 2, 3, 4, 5, 6, 7, 8};
    const auto is_even = [] (int i) { return i % 2 == 0; };

    const auto test = [&] {
        std::vector<int> v(src);
        std::stable_partition(v.begin(), v.end(), is_even);
        return v;
    }();

    SECTION("with iterators") {
        rng::stable_partition(src.begin(), src.end(), is_even);
    }

    SECTION("with range") {
        rng::stable_partition(src, is_even);
    }

    REQUIRE(src == test);
}

TEST_CASE("partition_point()")
{
    const auto is_even = [] (int i) { return i % 2 == 0; };

    const auto src = [&] {
        std::vector<int> v{1, 2, 3, 4, 5, 6, 7, 8};
        std::partition(v.begin(), v.end(), is_even);
        return v;
    }();

    const auto test_pt = std::partition_point(src.begin(), src.end(), is_even);


    SECTION("with iterators") {
        const auto it = rng::partition_point(src.begin(), src.end(), is_even);
        REQUIRE(it == test_pt);
    }

    SECTION("with range") {
        const auto it = rng::partition_point(src, is_even);
        REQUIRE(it == test_pt);
    }

}