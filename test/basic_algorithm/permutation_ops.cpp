
#include <nanorange/algorithm.hpp>

#include "../catch.hpp"

#include <list>
#include <vector>

TEST_CASE("alg.basic.is_permutation")
{
    const std::vector<int> vec{1, 2, 3, 4, 5};
    const std::vector<int> vec2{5, 4, 3, 2, 1};

    SECTION("with iterators") {
        REQUIRE(nano::is_permutation(vec.begin(), vec.end(), vec2.begin(), vec2.end()));
    }

    SECTION("with iterators (three-legged)") {
        REQUIRE(nano::is_permutation(vec.begin(), vec.end(), vec2.begin()));
    }

    SECTION("with ranges") {
        REQUIRE(nano::is_permutation(vec, vec2));
    }

    SECTION("with range and iterator") {
        REQUIRE(nano::is_permutation(vec, vec2.begin()));
    }

}

TEST_CASE("alg.basic.next_permutation")
{
    // We don't particularly care what the result is,
    // as long as we can call the function and it's the same
    // as what the STL version gives us
    std::list<int> l1{1, 2, 3, 4, 5};
    const auto result = [&] {
        auto temp = l1;
        std::next_permutation(temp.begin(), temp.end());
        return temp;
    }();

    SECTION("with iters") {
        CHECK(nano::next_permutation(l1.begin(), l1.end()));
        CHECK(l1 == result);
    }

    SECTION("with range") {
        CHECK(nano::next_permutation(l1));
        CHECK(l1 == result);
    }
}

TEST_CASE("alg.basic.prev_permutation")
{
    // We don't particularly care what the result is,
    // as long as we can call the function and it's the same
    // as what the STL version gives us
    std::list<int> l1{5, 4, 3, 2, 1};
    const auto result = [&] {
        auto temp = l1;
        std::prev_permutation(temp.begin(), temp.end());
        return temp;
    }();

    SECTION("with iters") {
        CHECK(nano::prev_permutation(l1.begin(), l1.end()));
        CHECK(l1 == result);
    }

    SECTION("with range") {
        CHECK(nano::prev_permutation(l1));
        CHECK(l1 == result);
    }
}