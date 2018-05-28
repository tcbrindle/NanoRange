
#include <nanorange/algorithm.hpp>

#include "../catch.hpp"

#include <vector>

TEST_CASE("alg.stl.is_permutation")
{
    const std::vector<int> v1{1, 2, 3, 4, 5};
    const std::vector<int> v2{1, 2, 3, 5, 4};

    SECTION("with iters") {
        CHECK(nano::is_permutation(v1.begin(), v1.end(),
                                   v2.begin(), v2.end()));
    }

    SECTION("With ranges") {
        CHECK(nano::is_permutation(v1, v2));
    }
}