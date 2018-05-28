
#include "../catch.hpp"

#include <nanorange/algorithm.hpp>

#include <array>
#include <vector>

namespace rng = nano::ranges;

TEST_CASE("alg.basic.all_of")
{
    const std::array<int, 3> arr{{1, 1, 1}};
    const auto pred = [](int i) { return i == 1; };

    REQUIRE(rng::all_of(arr.begin(), arr.end(), pred));
    REQUIRE(rng::all_of(arr, pred));
}

TEST_CASE("alg.basic.all_of (with projection)")
{
    const std::array<int, 3> arr{{1, 1, 1}};
    const auto pred = [] (int i) { return i == 7; };
    const auto proj = [] (int i) { return i + 6; };

    REQUIRE(rng::all_of(arr.begin(), arr.end(),pred, proj));
    REQUIRE(rng::all_of(arr, pred, proj));
}


TEST_CASE("alg.basic.any_of")
{
    constexpr std::array<int, 3> arr{{1, 2, 3}};
    const auto pred = [](int i) { return i == 2; };

    REQUIRE(rng::any_of(arr.begin(), arr.end(), pred));
    REQUIRE(rng::any_of(arr, pred));
}


TEST_CASE("alg.basic.none_of")
{
    constexpr std::array<int, 3> arr{{1, 2, 3}};
    const auto pred = [](int i) { return i < 0; };

    REQUIRE(rng::none_of(arr, pred));
}


TEST_CASE("alg.basic.for_each")
{
    constexpr std::array<int, 3> arr{{1, 2, 3}};
    int sum = 0;
    const auto func = [&sum] (int i) { sum += i; };

    const auto func2 = rng::for_each(arr, func);
    REQUIRE(func2.first == arr.end());

    REQUIRE(sum == 6);
}

TEST_CASE("alg.basic.count")
{
    constexpr std::array<int, 3> arr = {{2, 2, 2}};
    const auto n = rng::count(arr, 2);
    REQUIRE(n == 3);
}

TEST_CASE("alg.basic.count_if")
{
    constexpr std::array<int, 3> arr = {{2, 2, 2}};
    const auto n = rng::count_if(arr, [](int i) { return i == 2; });
    REQUIRE(n == 3);
}

TEST_CASE("alg.basic.mismatch (three-legged)")
{
    constexpr std::array<int, 3> arr1 = {{1, 2, 3}};
    constexpr std::array<int, 3> arr2 = {{1, 2, 4}};


    const auto p = rng::mismatch(arr1.begin(), arr1.end(), arr2.begin());
    REQUIRE(p.first == std::prev(arr1.end()));
    REQUIRE(p.second == std::prev(arr2.end()));
}

TEST_CASE("alg.basic.mismatch (three-legged with predicate)")
{
    constexpr std::array<int, 3> arr1 = {{1, 2, 3}};
    constexpr std::array<int, 3> arr2 = {{1, 2, 4}};

    const auto p = rng::mismatch(arr1.begin(), arr1.end(), arr2.begin(), std::equal_to<>{});
    REQUIRE(p.first == std::prev(arr1.end()));
    REQUIRE(p.second == std::prev(arr2.end()));
}


TEST_CASE("alg.basic.mismatch (four-legged)")
{
    constexpr std::array<int, 3> arr1 = {{1, 2, 3}};
    constexpr std::array<int, 3> arr2 = {{1, 2, 4}};

    SECTION("Iterators") {
        const auto p = rng::mismatch(arr1.begin(), arr1.end(), arr2.begin(), arr2.end());
        REQUIRE(p.first == std::prev(arr1.end()));
        REQUIRE(p.second == std::prev(arr2.end()));
    }

    SECTION("Ranges") {
        const auto p = rng::mismatch(arr1, arr2);
        REQUIRE(p.first == std::prev(arr1.end()));
        REQUIRE(p.second == std::prev(arr2.end()));
    }
}

TEST_CASE("alg.basic.mismatch (four-legged with predicate)")
{
    constexpr std::array<int, 3> arr1 = {{1, 2, 3}};
    constexpr std::array<int, 3> arr2 = {{1, 2, 4}};

    SECTION("Iterators") {
        const auto p = rng::mismatch(arr1.begin(), arr1.end(), arr2.begin(), arr2.end(), std::equal_to<>{});
        REQUIRE(p.first == std::prev(arr1.end()));
        REQUIRE(p.second == std::prev(arr2.end()));
    }

    SECTION("Ranges") {
        const auto p = rng::mismatch(arr1, arr2, std::equal_to<>{});
        REQUIRE(p.first == std::prev(arr1.end()));
        REQUIRE(p.second == std::prev(arr2.end()));
    }
}

TEST_CASE("alg.basic.equal (three-legged)")
{
    constexpr std::array<int, 3> arr = {{1, 2, 3}};
    const std::vector<int> vec = {1, 2, 3};


    bool b = rng::equal(arr.begin(), arr.end(), vec.begin());
    REQUIRE(b);
}

TEST_CASE("alg.basic.equal (three-legged with predicate)")
{
    constexpr std::array<int, 3> arr{{1, 2, 3}};
    const std::vector<int> vec{2, 3, 4};

    bool b = rng::equal(arr.begin(), arr.end(), vec.begin(), std::less<>{});
    REQUIRE(b);
}

TEST_CASE("alg.basic.equal (four-legged)")
{
    constexpr std::array<int, 3> arr = {{1, 2, 3}};
    const std::vector<int> vec = {1, 2, 3};


    SECTION("with iterators") {
        bool b = rng::equal(arr.begin(), arr.end(), vec.begin(), vec.end());
        REQUIRE(b);
    }

    SECTION("with ranges") {
        bool b = rng::equal(arr, vec);
        REQUIRE(b);
    }
}

TEST_CASE("alg.basic.equal (four-legged with predicate)")
{
    constexpr std::array<int, 3> arr = {{1, 2, 3}};
    const std::vector<int> vec = {2, 3, 4};


    SECTION("with iterators") {
        bool b = rng::equal(arr.begin(), arr.end(), vec.begin(), vec.end(), std::less<>{});
        REQUIRE(b);
    }

    SECTION("with ranges") {
        bool b = rng::equal(arr, vec, std::less<>{});
        REQUIRE(b);
    }
}

TEST_CASE("alg.basic.find")
{
    constexpr std::array<int, 3> arr = {{1, 2, 3}};

    SECTION("with iterators") {
        auto it = rng::find(arr.begin(), arr.end(), 2);
        REQUIRE(it == arr.begin() + 1);

        it = rng::find(arr.begin(), arr.end(), 27);
        REQUIRE(it == arr.end());
    }

    SECTION("with range") {
        auto it = rng::find(arr, 2);
        REQUIRE(it == arr.begin() + 1);

        it = rng::find(arr, 27);
        REQUIRE(it == arr.end());
    }
}


TEST_CASE("alg.basic.find_if")
{
    constexpr std::array<int, 3> arr = {{1, 2, 3}};
    const auto pred = [] (int i) { return i == 2; };

    SECTION("with iterators") {
        auto it = rng::find_if(arr.begin(), arr.end(), pred);
        REQUIRE(it == arr.begin() + 1);
    }

    SECTION("with range") {
        auto it = rng::find_if(arr, pred);
        REQUIRE(it == arr.begin() + 1);
    }
}

TEST_CASE("alg.basic.find_if_not")
{
    constexpr std::array<int, 3> arr = {{1, 1, 2}};
    const auto pred = [] (int i) { return i == 1; };

    SECTION("with iterators") {
        auto it = rng::find_if_not(arr.begin(), arr.end(), pred);
        REQUIRE(it == std::prev(arr.end()));
    }

    SECTION("with range") {
        auto it = rng::find_if_not(arr, pred);
        REQUIRE(it == std::prev(arr.end()));
    }
}

TEST_CASE("alg.basic.find_end")
{
    constexpr std::array<int, 9> arr = {{1, 2, 3, 1, 2, 3, 1, 2, 3}};
    const std::vector<int> vec = {1, 2, 3};

    SECTION("with iterators") {
        const auto it = rng::find_end(arr.begin(), arr.end(), vec.begin(), vec.end());
        REQUIRE(it == arr.end() - 3);
    }

    SECTION("with ranges") {
        const auto it = rng::find_end(arr, vec);
        REQUIRE(it == arr.end() - 3);
    }
}

TEST_CASE("alg.basic.find_end (with predicate)")
{
    constexpr std::array<int, 9> arr = {{1, 2, 3, 1, 2, 3, 1, 2, 3}};
    const std::vector<int> vec = {1, 2, 3};

    SECTION("with iterators") {
        const auto it = rng::find_end(arr.begin(), arr.end(), vec.begin(), vec.end(), std::equal_to<>{});
        REQUIRE(it == arr.end() - 3);
    }

    SECTION("with ranges") {
        const auto it = rng::find_end(arr, vec, std::equal_to<>{});
        REQUIRE(it == arr.end() - 3);
    }
}

TEST_CASE("alg.basic.find_first_of")
{
    const std::vector<int> vec{1, 2, 3, 4, 5, 6};
    const std::array<int, 3> arr{{-1, 22, 3}};

    SECTION("with iterators") {
        const auto it = rng::find_first_of(vec.begin(), vec.end(), arr.begin(), arr.end());
        REQUIRE(it == vec.begin() + 2);
    }

    SECTION("with ranges") {
        const auto it = rng::find_first_of(vec, arr);
        REQUIRE(it == vec.begin() + 2);
    }
}

TEST_CASE("alg.basic.find_first_of (with predicate)")
{
    const std::vector<int> vec{1, 2, 3, 4, 5, 6};
    const std::array<int, 3> arr{{-1, 22, 3}};

    SECTION("with iterators") {
        const auto it = rng::find_first_of(vec.begin(), vec.end(), arr.begin(), arr.end(), std::equal_to<>{});
        REQUIRE(it == vec.begin() + 2);
    }

    SECTION("with ranges") {
        const auto it = rng::find_first_of(vec, arr, std::equal_to<>{});
        REQUIRE(it == vec.begin() + 2);
    }
}

TEST_CASE("alg.basic.adjacent_find")
{
    const std::vector<int> vec{1, 2, 3, 3, 3, 4};

    SECTION("with iterators") {
        const auto it = rng::adjacent_find(vec.begin(), vec.end());
        REQUIRE(it == vec.begin() + 2);
    }

    SECTION("with ranges") {
        const auto it = rng::adjacent_find(vec);
        REQUIRE(it == vec.begin() + 2);
    }
}

TEST_CASE("alg.basic.adjacent_find (with predicate)")
{
    const std::vector<int> vec{1, 2, 3, 2, 1};

    SECTION("with iterators") {
        const auto it = rng::adjacent_find(vec.begin(), vec.end(), std::greater<>{});
        REQUIRE(it == vec.begin() + 2);
    }

    SECTION("with ranges") {
        const auto it = rng::adjacent_find(vec, std::greater<>{});
        REQUIRE(it == vec.begin() + 2);
    }

}

TEST_CASE("alg.basic.search")
{
    const std::vector<int> vec = {1, 2, 3, 4, 5, 6, 7, 8, 9};
    constexpr std::array<int, 3> arr = {{3, 4, 5}};

    SECTION("with iterators") {
        const auto it = rng::search(vec.begin(), vec.end(), arr.begin(), arr.end());
        REQUIRE(it == vec.begin() + 2);
    }

    SECTION("with ranges") {
        const auto it = rng::search(vec, arr);
        REQUIRE(it == vec.begin() + 2);
    }
}

TEST_CASE("alg.basic.search (with predicate)")
{
    const std::vector<int> vec = {1, 2, 3, 4, 5, 6, 7, 8, 9};
    constexpr std::array<int, 3> arr = {{3, 4, 5}};

    struct {
        bool operator()(int i, int j) const { return i == j; }
    } pred;

    SECTION("with iterators") {
        const auto it = rng::search(vec.begin(), vec.end(), arr.begin(), arr.end(), pred);
        REQUIRE(it == vec.begin() + 2);
    }

    SECTION("with ranges") {
        const auto it = rng::search(vec, arr, pred);
        REQUIRE(it == vec.begin() + 2);
    }
}

TEST_CASE("alg.basic.search_n")
{
    const std::vector<int> vec{1, 2, 3, 4, 1, 1, 1};

    SECTION("with iterators") {
        const auto it = rng::search_n(vec.begin(), vec.end(), 3, 1);
        REQUIRE(it == vec.begin() + 4);
    }

    SECTION("with range") {
        const auto it = rng::search_n(vec, 3, 1);
        REQUIRE(it == vec.begin() + 4);
    }
}

TEST_CASE("alg.basic.search_n (with predicate)")
{
    const std::vector<int> vec{1, 2, 3, 4, 1, 1, 1};

    SECTION("with iterators") {
        const auto it = rng::search_n(vec.begin(), vec.end(), 3, 1, std::equal_to<>{});
        REQUIRE(it == vec.begin() + 4);
    }

    SECTION("with range") {
        const auto it = rng::search_n(vec, 3, 1, std::equal_to<>{});
        REQUIRE(it == vec.begin() + 4);
    }
}

TEST_CASE("alg.basic.is_permutation (with predicate)")
{
    struct foo { foo(int i):i(i){} int i; };

    const std::vector<foo> vec{1, 2, 3, 4, 5};
    const std::vector<foo> vec2{{5, 4, 3, 2, 1}};
    const auto pred = [](auto x, auto y) {
        return x.i == y.i;
    };

    SECTION("with iterators") {
        REQUIRE(rng::is_permutation(vec.begin(), vec.end(), vec2.begin(), vec2.end(), pred));
    }

    SECTION("with iterators (three-legged)") {
        REQUIRE(rng::is_permutation(vec.begin(), vec.end(), vec2.begin(), pred));
    }

    SECTION("with ranges") {
        REQUIRE(rng::is_permutation(vec, vec2, pred));
    }

    SECTION("with range and iterator") {
        REQUIRE(rng::is_permutation(vec, vec2.begin(), pred));
    }

}