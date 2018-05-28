
#include <nanorange/algorithm.hpp>

#include "../catch.hpp"

#include <vector>

namespace {

// Lambda as comparator, just for testing
const auto greater = [](const auto& lhs, const auto& rhs) {
    return lhs > rhs;
};

}

TEST_CASE("alg.basic.is_heap")
{
    const std::vector<int> vec{5, 1, 2, 4, 3};
    const std::vector<int> heap = [&] {
        auto v = vec;
        std::make_heap(v.begin(), v.end());
        return v;
    }();

    SECTION("with iterators") {
        REQUIRE_FALSE(nano::is_heap(vec.begin(), vec.end()));
        REQUIRE(nano::is_heap(heap.begin(), heap.end()));
    }

    SECTION("with range") {
        REQUIRE_FALSE(nano::is_heap(vec));
        REQUIRE(nano::is_heap(heap));
    }
}

TEST_CASE("alg.basic.is_heap (with comparator)")
{
    const std::vector<int> vec{3, 2, 5, 1, 4};
    const std::vector<int> heap = [&] {
        auto v = vec;
        std::make_heap(v.begin(), v.end(), greater);
        return v;
    }();

    SECTION("with iterators") {
        REQUIRE_FALSE(nano::is_heap(vec.begin(), vec.end(), greater));
        REQUIRE(nano::is_heap(heap.begin(), heap.end(), greater));
    }

    SECTION("with range") {
        REQUIRE_FALSE(nano::is_heap(vec, greater));
        REQUIRE(nano::is_heap(heap, greater));
    }
}

TEST_CASE("alg.basic.is_heap_until")
{
    const std::vector<int> heap = [&] {
        std::vector<int> v{5, 1, 3, 4, 2};
        std::make_heap(v.begin(), v.begin() + 2);
        return v;
    }();

    std::vector<int>::const_iterator it;

    SECTION("with iterators") {
        it = nano::is_heap_until(heap.begin(), heap.end());
    }

    SECTION("with range") {
        it = nano::is_heap_until(heap);
    }

    REQUIRE(it == std::is_heap_until(heap.begin(), heap.end()));
}

TEST_CASE("alg.basic.is_heap_until (with comparator)")
{
    const std::vector<int> heap = [&] {
        std::vector<int> v{5, 1, 3, 4, 2};
        std::make_heap(v.begin(), v.begin() + 2, greater);
        return v;
    }();

    std::vector<int>::const_iterator it;

    SECTION("with iterators") {
        it = nano::is_heap_until(heap.begin(), heap.end(), greater);
    }

    SECTION("with range") {
        it = nano::is_heap_until(heap, greater);
    }

    REQUIRE(it == std::is_heap_until(heap.begin(), heap.end(), greater));
}

TEST_CASE("alg.basic.make_heap")
{
    std::vector<int> vec{3, 2, 5, 1, 4};

    SECTION("with iterators") {
        nano::make_heap(vec.begin(), vec.end());
    }

    SECTION("with range") {
        nano::make_heap(vec);
    }

    REQUIRE(std::is_heap(vec.begin(), vec.end()));
}

TEST_CASE("alg.basic.make_heap (with comparator)")
{
    std::vector<int> vec{3, 2, 5, 1, 4};

    SECTION("with iterators") {
        nano::make_heap(vec.begin(), vec.end(), greater);
    }

    SECTION("with range") {
        nano::make_heap(vec, greater);
    }

    REQUIRE(std::is_heap(vec.begin(), vec.end(), greater));
}

TEST_CASE("alg.basic.sort_heap")
{
    std::vector<int> vec{3, 5, 2, 1, 4};
    std::make_heap(vec.begin(), vec.end());

    SECTION("with iterators") {
        nano::sort_heap(vec.begin(), vec.end());
    }

    SECTION("with range") {
        nano::sort_heap(vec);
    }

    REQUIRE(std::is_sorted(vec.begin(), vec.end()));
}

TEST_CASE("alg.basic.sort_heap (with comparator)")
{
    std::vector<int> vec{3, 5, 2, 1, 4};
    std::make_heap(vec.begin(), vec.end(), greater);

    SECTION("with iterators") {
        nano::sort_heap(vec.begin(), vec.end(), greater);
    }

    SECTION("with range") {
        nano::sort_heap(vec, greater);
    }

    REQUIRE(std::is_sorted(vec.begin(), vec.end(), greater));
}

TEST_CASE("alg.basic.push_heap")
{
    std::vector<int> vec{3, 5, 2, 1, 4};
    std::make_heap(vec.begin(), vec.end());
    vec.push_back(6);

    SECTION("with iterators") {
        nano::push_heap(vec.begin(), vec.end());
    }

    SECTION("with range") {
        nano::push_heap(vec);
    }

    REQUIRE(std::is_heap(vec.begin(), vec.end()));
}

TEST_CASE("alg.basic.push_heap (with comparator)")
{
    std::vector<int> vec{3, 5, 2, 1, 4};
    std::make_heap(vec.begin(), vec.end(), greater);
    vec.push_back(6);

    SECTION("with iterators") {
        nano::push_heap(vec.begin(), vec.end(), greater);
    }

    SECTION("with range") {
        nano::push_heap(vec, greater);
    }

    REQUIRE(std::is_heap(vec.begin(), vec.end(), greater));
}

TEST_CASE("alg.basic.pop_heap")
{
    std::vector<int> vec{3, 5, 2, 1, 4};
    std::make_heap(vec.begin(), vec.end());

    SECTION("with iterators") {
        nano::pop_heap(vec.begin(), vec.end());
    }

    SECTION("with range") {
        nano::pop_heap(vec);
    }

    REQUIRE(vec.back() == 5);
}

TEST_CASE("alg.basic.pop_heap (with comparator)")
{
    std::vector<int> vec{3, 5, 2, 1, 4};
    std::make_heap(vec.begin(), vec.end(), greater);

    SECTION("with iterators") {
        nano::pop_heap(vec.begin(), vec.end(), greater);
    }

    SECTION("with range") {
        nano::pop_heap(vec, greater);
    }

    REQUIRE(vec.back() == 1);
}