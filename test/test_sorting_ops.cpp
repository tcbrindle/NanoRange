
#include "catch.hpp"

#include <nanorange.hpp>

#include <list>

namespace rng = nanorange;

TEST_CASE("is_sorted()")
{
    const std::vector<int> sorted{1, 2, 3, 4, 5};
    const std::vector<int> unsorted{5, 3, 4, 2, 1};

    SECTION("with iterators") {
        REQUIRE(rng::is_sorted(sorted.begin(), sorted.end()));
        REQUIRE_FALSE(rng::is_sorted(unsorted.begin(), unsorted.end()));
    }

    SECTION("with ranges") {
        REQUIRE(rng::is_sorted(sorted));
        REQUIRE_FALSE(rng::is_sorted(unsorted));
    }
}

TEST_CASE("is_sorted() (with comparator)")
{
    const std::vector<int> sorted{5, 4, 3, 2, 1};
    const std::vector<int> unsorted{5, 3, 4, 2, 1};

    SECTION("with iterators") {
        REQUIRE(rng::is_sorted(sorted.begin(), sorted.end(), std::greater<>{}));
        REQUIRE_FALSE(rng::is_sorted(unsorted.begin(), unsorted.end()));
    }

    SECTION("with ranges") {
        REQUIRE(rng::is_sorted(sorted, std::greater<>{}));
        REQUIRE_FALSE(rng::is_sorted(unsorted));
    }
}

TEST_CASE("is_sorted_until()")
{
    const std::vector<int> vec{1, 2, 3, 4, 3};

    SECTION("with iterators") {
        const auto it = rng::is_sorted_until(vec.begin(), vec.end());
        REQUIRE(it == vec.end() - 1);
    }

    SECTION("with range") {
        const auto it = rng::is_sorted_until(vec);
        REQUIRE(it == vec.end() - 1);
    }
}

TEST_CASE("is_sorted_until() (with comparator)")
{
    const std::vector<int> vec{5, 4, 3, 2, 3};

    SECTION("with iterators") {
        const auto it = rng::is_sorted_until(vec.begin(), vec.end(), std::greater<>{});
        REQUIRE(it == vec.end() - 1);
    }

    SECTION("with range") {
        const auto it = rng::is_sorted_until(vec, std::greater<>{});
        REQUIRE(it == vec.end() - 1);
    }
}

TEST_CASE("sort()")
{
    std::vector<int> vec{5, 3, 2, 4, 1};

    SECTION("with iterators") {
        rng::sort(vec.begin(), vec.end());
    }

    SECTION("with range") {
        rng::sort(vec);
    }

    REQUIRE(std::is_sorted(vec.begin(), vec.end()));
}

TEST_CASE("partial_sort()")
{

}

TEST_CASE("partial_sort() (with comparator)")
{
}

TEST_CASE("partial_sort_copy()")
{
}

TEST_CASE("partial_sort_copy() (with comparator)")
{
}

TEST_CASE("stable_sort()")
{
}

TEST_CASE("stable_sort() (with comparator)")
{
}

TEST_CASE("nth_element()")
{}

TEST_CASE("nth_element() (with comparator)")
{}