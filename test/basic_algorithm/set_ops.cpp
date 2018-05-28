
#include <nanorange/algorithm.hpp>

#include "../catch.hpp"
#include "../test_utils.hpp"

#include <sstream>

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

        nano::merge(first1, last, first2, last, result, nano::greater<>{});

        CHECK(out == std::vector<int>{10, 9, 8, 7, 6, 5, 4, 3, 2, 1});
    }

    SECTION("with iterators") {
        auto rng1 = nano::ext::make_range(
            std::istream_iterator<int>{oss1},
            std::istream_iterator<int>{});
        auto rng2 = nano::ext::make_range(
            std::istream_iterator<int>{oss2},
            std::istream_iterator<int>{});
        auto result = nano::back_inserter(out);

        nano::merge(rng1, rng2, result, nano::greater<>{});

        CHECK(out == std::vector<int>{10, 9, 8, 7, 6, 5, 4, 3, 2, 1});
    }
}