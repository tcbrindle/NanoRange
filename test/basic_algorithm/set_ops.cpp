
#include <nanorange/algorithm.hpp>
#include <nanorange/iterator/back_insert_iterator.hpp>

#include "../catch.hpp"
#include "../test_utils.hpp"

#include <iterator>
#include <list>
#include <sstream>
#include <vector>

TEST_CASE("alg.basic.merge")
{
    std::istringstream oss1{"10 8 6 4 2"};
    std::istringstream oss2{"9 7 5 3 1"};
    std::vector<int> out;


    static_assert(nano::Iterator<
        nano::back_insert_iterator<std::vector<int>>>, "");

    SECTION("with iterators") {
        std::istream_iterator<int> first1{oss1};
        std::istream_iterator<int> last{};
        std::istream_iterator<int> first2{oss2};
        auto result = nano::back_inserter(out);

        nano::merge(first1, last, first2, last, result, nano::greater{});

        CHECK(out == std::vector<int>{10, 9, 8, 7, 6, 5, 4, 3, 2, 1});
    }

    SECTION("with iterators") {
        auto rng1 = nano::subrange(
            std::istream_iterator<int>{oss1},
            std::istream_iterator<int>{});
        auto rng2 = nano::subrange(
            std::istream_iterator<int>{oss2},
            std::istream_iterator<int>{});
        auto result = nano::back_inserter(out);

        nano::merge(rng1, rng2, result, nano::greater{});

        CHECK(out == std::vector<int>{10, 9, 8, 7, 6, 5, 4, 3, 2, 1});
    }
}

TEST_CASE("alg.basic.inplace_merge")
{
    auto list = [] {
        std::vector<int> v1{10, 8, 6, 4, 2};
        std::vector<int> v2{9, 7, 5, 3, 1};
        std::list<int> l(v1.begin(), v1.end());
        l.insert(l.end(), v2.begin(), v2.end());
        return l;
    }();
    const auto it = nano::next(list.begin(), 5);

    SECTION("with iterators") {
        nano::inplace_merge(list.begin(), it, list.end(), nano::greater{});
    }

    SECTION("with range") {
        nano::inplace_merge(list, it, nano::greater{});
    }

    REQUIRE(list == std::list<int>{10, 9, 8, 7, 6, 5, 4, 3, 2, 1});
}

TEST_CASE("alg.basic.set_difference")
{
    std::istringstream iss1{"5 4 3 2 1"};
    std::istringstream iss2{"4 3 2"};
    std::vector<int> vec;

    SECTION("with iterators") {
        nano::set_difference(
            std::istream_iterator<int>{iss1},
            std::istream_iterator<int>{},
            std::istream_iterator<int>{iss2},
            std::istream_iterator<int>{},
            nano::back_inserter(vec),
            std::greater<>{});
    }

    SECTION("with ranges") {
        auto rng1 = nano::subrange(
            std::istream_iterator<int>{iss1},
            std::istream_iterator<int>{});
        auto rng2 = nano::subrange(
            std::istream_iterator<int>{iss2},
            std::istream_iterator<int>{});
        nano::set_difference(rng1, rng2, nano::back_inserter(vec), nano::greater{});
    }

    REQUIRE(vec == std::vector<int>{5, 1});
}

TEST_CASE("alg.basic.set_intersection")
{
    std::istringstream iss1{"5 4 3 2 1"};
    std::istringstream iss2{"4 3 2"};
    std::vector<int> vec;

    SECTION("with iterators") {
        nano::set_intersection(
            std::istream_iterator<int>{iss1},
            std::istream_iterator<int>{},
            std::istream_iterator<int>{iss2},
            std::istream_iterator<int>{},
            nano::back_inserter(vec),
            std::greater<>{});
    }

    SECTION("with ranges") {
        auto rng1 = nano::subrange(
            std::istream_iterator<int>{iss1},
            std::istream_iterator<int>{});
        auto rng2 = nano::subrange(
            std::istream_iterator<int>{iss2},
            std::istream_iterator<int>{});
        nano::set_intersection(rng1, rng2, nano::back_inserter(vec), nano::greater{});
    }

    REQUIRE(vec == std::vector<int>{4, 3, 2});
}

TEST_CASE("alg.basic.set_symmetric_difference")
{
    std::istringstream iss1{"5 4 3 2 1"};
    std::istringstream iss2{"6 4 3 2 0"};
    std::vector<int> vec;

    SECTION("with iterators") {
        nano::set_symmetric_difference(
            std::istream_iterator<int>{iss1},
            std::istream_iterator<int>{},
            std::istream_iterator<int>{iss2},
            std::istream_iterator<int>{},
            nano::back_inserter(vec),
            std::greater<>{});
    }

    SECTION("with ranges") {
        auto rng1 = nano::subrange(
            std::istream_iterator<int>{iss1},
            std::istream_iterator<int>{});
        auto rng2 = nano::subrange(
            std::istream_iterator<int>{iss2},
            std::istream_iterator<int>{});
        nano::set_symmetric_difference(rng1, rng2, nano::back_inserter(vec), nano::greater{});
    }

    REQUIRE(vec == std::vector<int>{6, 5, 1, 0});
}

TEST_CASE("alg.basic.set_union")
{
    std::istringstream iss1{"10 8 6 4 2"};
    std::istringstream iss2{"9 7 5 3 1"};
    std::vector<int> vec;

    SECTION("with iterators") {
        nano::set_union(
            std::istream_iterator<int>{iss1},
            std::istream_iterator<int>{},
            std::istream_iterator<int>{iss2},
            std::istream_iterator<int>{},
            nano::back_inserter(vec),
            std::greater<>{});
    }

    SECTION("with ranges") {
        auto rng1 = nano::subrange(
            std::istream_iterator<int>{iss1},
            std::istream_iterator<int>{});
        auto rng2 = nano::subrange(
            std::istream_iterator<int>{iss2},
            std::istream_iterator<int>{});
        nano::set_union(rng1, rng2, nano::back_inserter(vec), nano::greater{});
    }

    REQUIRE(vec == std::vector<int>{10, 9, 8, 7, 6, 5, 4, 3, 2, 1});
}

TEST_CASE("alg.basic.includes")
{
    std::istringstream iss1{"5 4 3 2 1"};
    std::istringstream iss2{"4 3 2"};
    std::istringstream iss3("6 0");

    SECTION("with iterators") {
        REQUIRE(nano::includes(
            std::istream_iterator<int>{iss1},
            std::istream_iterator<int>{},
            std::istream_iterator<int>{iss2},
            std::istream_iterator<int>{},
            std::greater<>{}));
        REQUIRE_FALSE(nano::includes(
            std::istream_iterator<int>{iss1},
            std::istream_iterator<int>{},
            std::istream_iterator<int>{iss3},
            std::istream_iterator<int>{},
            std::greater<>{}));
    }

    SECTION("with ranges") {
        auto rng1 = nano::subrange(
            std::istream_iterator<int>{iss1},
            std::istream_iterator<int>{});
        auto rng2 = nano::subrange(
            std::istream_iterator<int>{iss2},
            std::istream_iterator<int>{});
        auto rng3 = nano::subrange(
            std::istream_iterator<int>{iss3},
            std::istream_iterator<int>{});
        REQUIRE(nano::includes(rng1, rng2, nano::greater{}));
        REQUIRE_FALSE(nano::includes(rng1, rng3, nano::greater{}));
    }
}