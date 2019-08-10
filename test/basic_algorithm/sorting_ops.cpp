
#include "../catch.hpp"

#include <nanorange/algorithm.hpp>

#include "../test_utils.hpp"

#include <iterator>
#include <list>
#include <sstream>

namespace rng = nano;

TEST_CASE("alg.basic.is_sorted")
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

TEST_CASE("alg.basic.is_sorted (with comparator)")
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

TEST_CASE("alg.basic.is_sorted_until")
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

TEST_CASE("alg.basic.is_sorted_until (with comparator)")
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

TEST_CASE("alg.basic.sort")
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

TEST_CASE("alg.basic.sort (with comparator)")
{
    std::vector<int> vec{1, 2, 3, 4, 5};

    SECTION("with iterators") {
        rng::sort(vec.begin(), vec.end(), std::greater<>{});
    }

    SECTION("with range") {
        rng::sort(vec, std::greater<>{});
    }

    REQUIRE(std::is_sorted(vec.begin(), vec.end(), std::greater<>{}));
}

TEST_CASE("alg.basic.partial_sort")
{
    std::vector<int> vec{5, 4, 3, 2, 1};

    SECTION("with iterators") {
        rng::partial_sort(vec.begin(), vec.begin() + 3, vec.end());
    }

    SECTION("with range") {
        rng::partial_sort(vec, vec.begin() + 3);
    }

    REQUIRE(vec[0] == 1);
    REQUIRE(vec[1] == 2);
    REQUIRE(vec[2] == 3);
}

TEST_CASE("alg.basic.partial_sort (with comparator)")
{
    std::vector<int> vec{1, 2, 3, 4, 5};

    SECTION("with iterators") {
        rng::partial_sort(vec.begin(), vec.begin() + 3, vec.end(), rng::greater{});
    }

    SECTION("with range") {
        rng::partial_sort(vec, vec.begin() + 3, rng::greater{});
    }

    REQUIRE(vec[0] == 5);
    REQUIRE(vec[1] == 4);
    REQUIRE(vec[2] == 3);
}

TEST_CASE("alg.basic.partial_sort_copy")
{
    std::istringstream iss{"5 4 3 2 1"};
    std::vector<int> vec(3);

    SECTION("with iterators") {
        rng::partial_sort_copy(std::istream_iterator<int>{iss},
                               std::istream_iterator<int>{},
                               vec.begin(), vec.end());
    }

    SECTION("with ranges") {
        auto in_rng = nano::subrange(
            std::istream_iterator<int>{iss},
            std::istream_iterator<int>{});
        rng::partial_sort_copy(in_rng, vec);
    }

    REQUIRE(vec[0] == 1);
    REQUIRE(vec[1] == 2);
    REQUIRE(vec[2] == 3);
}

TEST_CASE("alg.basic.partial_sort_copy (with comparator)")
{
    std::istringstream iss{"1 2 3 4 5"};
    std::vector<int> vec(3);

    SECTION("with iterators") {
        rng::partial_sort_copy(std::istream_iterator<int>{iss},
                               std::istream_iterator<int>{},
                               vec.begin(), vec.end(),
                               rng::greater{});
    }

    SECTION("with ranges") {
        auto in_rng = nano::subrange(
            std::istream_iterator<int>{iss},
            std::istream_iterator<int>{});
        rng::partial_sort_copy(in_rng, vec, rng::greater{});
    }

    REQUIRE(vec[0] == 5);
    REQUIRE(vec[1] == 4);
    REQUIRE(vec[2] == 3);
}

namespace {

struct int_pair {
    int i;
    int j;
};

bool operator==(const int_pair& lhs, const int_pair& rhs)
{
    return lhs.i == rhs.i  &&
           lhs.j == rhs.j;
}

bool operator!=(const int_pair& lhs, const int_pair& rhs)
{
    return !(lhs == rhs);
}

bool operator<(const int_pair& lhs, const int_pair& rhs)
{
    return lhs.i < rhs.i;
}

bool operator>(const int_pair& lhs, const int_pair& rhs)
{
    return rhs < lhs;
}

bool operator<=(const int_pair& lhs, const int_pair& rhs)
{
    return !(rhs < lhs);
}

bool operator>=(const int_pair& lhs, const int_pair& rhs)
{
    return !(lhs < rhs);
}

}

TEST_CASE("alg.basic.stable_sort")
{
    std::vector<int_pair> vec{{5, 5}, {5, 4}, {5, 3}, {5, 2}, {5, 1}};
    const std::vector<int_pair> check = vec;

    SECTION("with iterators") {
        rng::stable_sort(vec.begin(), vec.end());
    }

    SECTION("with range") {
        rng::stable_sort(vec);
    }

    REQUIRE(vec == check);
}

TEST_CASE("alg.basic.stable_sort (with comparator)")
{
    std::vector<int_pair> vec{{5, 5}, {5, 4}, {5, 3}, {5, 2}, {5, 1}};
    const std::vector<int_pair> check = vec;

    SECTION("with iterators") {
        rng::stable_sort(vec.begin(), vec.end(), rng::greater{});
    }

    SECTION("with range") {
        rng::stable_sort(vec, rng::greater{});
    }

    REQUIRE(vec == check);
}

TEST_CASE("alg.basic.nth_element")
{
    std::vector<int> vec{5, 4, 3, 2, 1};

    SECTION("with iterators") {
        rng::nth_element(vec.begin(), vec.begin() + 1, vec.end());
    }

    SECTION("with range") {
        rng::nth_element(vec, vec.begin() + 1);
    }

    REQUIRE(vec[1] == 2);
}

TEST_CASE("alg.basic.nth_element (with comparator)")
{
    std::vector<int> vec{1, 2, 3, 4, 5};

    SECTION("with iterators") {
        rng::nth_element(vec.begin(), vec.begin() + 1, vec.end(), rng::greater{});
    }

    SECTION("with range") {
        rng::nth_element(vec, vec.begin() + 1, rng::greater{});
    }

    REQUIRE(vec[1] == 4);
}