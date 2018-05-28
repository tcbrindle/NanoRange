
#include <nanorange/algorithm.hpp>

#include "../catch.hpp"

#include <vector>

TEST_CASE("alg.stl.lower_bound")
{
    std::vector<int> vec{1, 2, 3, 4, 5};

    SECTION("with iters") {
        auto it = nano::lower_bound(vec.begin(), vec.end(), 3.5);
        CHECK(it == vec.begin() + 3);
    }

    SECTION("with range") {
        auto it = nano::lower_bound(vec, 3.5);
        CHECK(it == vec.begin() + 3);
    }
}

TEST_CASE("alg.stl.upper_bound")
{
    std::vector<int> vec{1, 2, 3, 4, 5};

    SECTION("with iters") {
        auto it = nano::upper_bound(vec.begin(), vec.end(), 3.5);
        CHECK(it == vec.begin() + 3);
    }

    SECTION("with range") {
        auto it = nano::upper_bound(vec, 3.5);
        CHECK(it == vec.begin() + 3);
    }
}

TEST_CASE("alg.stl.equal_range")
{
    std::vector<int> vec{1, 2, 2, 2, 3};

    SECTION("with iters") {
        auto rng = nano::equal_range(vec.begin(), vec.end(), 2);
        CHECK(rng.begin() == vec.begin() + 1);
        CHECK(rng.end() == vec.begin() + 4);
    }

    SECTION("with range") {
        auto rng = nano::equal_range(vec, 2);
        CHECK(rng.begin() == vec.begin() + 1);
        CHECK(rng.end() == vec.begin() + 4);
    }
}

TEST_CASE("alg.stl.binary_search")
{
    std::vector<int> vec{1, 2, 3, 4, 5};

    SECTION("with iters") {
       CHECK(nano::binary_search(vec.begin(), vec.end(), 3));
       CHECK_FALSE((nano::binary_search(vec.begin(), vec.end(), 42)));
    }

    SECTION("with range") {
        CHECK(nano::binary_search(vec, 3));
        CHECK_FALSE((nano::binary_search(vec, 42)));
    }
}