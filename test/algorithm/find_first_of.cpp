// Range v3 library
//
//  Copyright Eric Niebler 2014
//
//  Use, modification and distribution is subject to the
//  Boost Software License, Version 1.0. (See accompanying
//  file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt)
//
// Project home: https://github.com/ericniebler/range-v3

//===----------------------------------------------------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is dual licensed under the MIT and the University of Illinois Open
// Source Licenses. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include <nanorange/algorithm/find_first_of.hpp>
namespace rng = nano::ranges;

#include "../catch.hpp"
#include "../test_iterators.hpp"
#include "../test_utils.hpp"

namespace {

void test_iter()
{
    using rng::find_first_of;
    int ia[] = {0, 1, 2, 3, 0, 1, 2, 3};
    static constexpr unsigned sa = rng::size(ia);
    int ib[] = {1, 3, 5, 7};
    static constexpr unsigned sb = rng::size(ib);
    CHECK(rng::find_first_of(input_iterator<const int*>(ia),
                             sentinel<const int*>(ia + sa),
                             forward_iterator<const int*>(ib),
                             sentinel<const int*>(ib + sb)) ==
            input_iterator<const int*>(ia + 1));
    int ic[] = {7};
    CHECK(rng::find_first_of(input_iterator<const int*>(ia),
                             sentinel<const int*>(ia + sa),
                             forward_iterator<const int*>(ic),
                             sentinel<const int*>(ic + 1)) ==
            input_iterator<const int*>(ia + sa));
    CHECK(rng::find_first_of(input_iterator<const int*>(ia),
                             sentinel<const int*>(ia + sa),
                             forward_iterator<const int*>(ic),
                             sentinel<const int*>(ic)) ==
            input_iterator<const int*>(ia + sa));
    CHECK(rng::find_first_of(input_iterator<const int*>(ia),
                             sentinel<const int*>(ia),
                             forward_iterator<const int*>(ic),
                             sentinel<const int*>(ic + 1)) ==
            input_iterator<const int*>(ia));
}

void test_iter_pred()
{
    using nano::find_first_of;
    int ia[] = {0, 1, 2, 3, 0, 1, 2, 3};
    static constexpr unsigned sa = nano::size(ia);
    int ib[] = {1, 3, 5, 7};
    static constexpr unsigned sb = nano::size(ib);
    CHECK(rng::find_first_of(input_iterator<const int*>(ia),
                             sentinel<const int*>(ia + sa),
                             forward_iterator<const int*>(ib),
                             sentinel<const int*>(ib + sb),
                             std::equal_to<int>()) ==
            input_iterator<const int*>(ia + 1));
    int ic[] = {7};
    CHECK(rng::find_first_of(input_iterator<const int*>(ia),
                             sentinel<const int*>(ia + sa),
                             forward_iterator<const int*>(ic),
                             sentinel<const int*>(ic + 1),
                             std::equal_to<int>()) ==
            input_iterator<const int*>(ia + sa));
    CHECK(rng::find_first_of(input_iterator<const int*>(ia),
                             sentinel<const int*>(ia + sa),
                             forward_iterator<const int*>(ic),
                             sentinel<const int*>(ic),
                             std::equal_to<int>()) ==
            input_iterator<const int*>(ia + sa));
    CHECK(rng::find_first_of(input_iterator<const int*>(ia),
                             sentinel<const int*>(ia),
                             forward_iterator<const int*>(ic),
                             sentinel<const int*>(ic + 1),
                             std::equal_to<int>()) ==
            input_iterator<const int*>(ia));
}

void test_rng()
{
    using nano::find_first_of;
    using nano::subrange;
    int ia[] = {0, 1, 2, 3, 0, 1, 2, 3};
    static constexpr unsigned sa = nano::size(ia);
    int ib[] = {1, 3, 5, 7};
    static constexpr unsigned sb = nano::size(ib);
    CHECK(rng::find_first_of(
            as_lvalue(subrange(input_iterator<const int*>(ia),
                                 input_iterator<const int*>(ia + sa))),
            subrange(forward_iterator<const int*>(ib),
                       forward_iterator<const int*>(ib + sb))) ==
            input_iterator<const int*>(ia + 1));
    CHECK(rng::find_first_of(subrange(input_iterator<const int*>(ia),
                                        input_iterator<const int*>(ia + sa)),
                             subrange(forward_iterator<const int*>(ib),
                                        forward_iterator<const int*>(
                                                ib + sb))) ==
            input_iterator<const int*>(ia + 1));
    int ic[] = {7};
    CHECK(rng::find_first_of(
            as_lvalue(subrange(input_iterator<const int*>(ia),
                                 input_iterator<const int*>(ia + sa))),
            subrange(forward_iterator<const int*>(ic),
                       forward_iterator<const int*>(ic + 1))) ==
            input_iterator<const int*>(ia + sa));
    CHECK(rng::find_first_of(
            as_lvalue(subrange(input_iterator<const int*>(ia),
                                 input_iterator<const int*>(ia + sa))),
            subrange(forward_iterator<const int*>(ic),
                       forward_iterator<const int*>(ic))) ==
            input_iterator<const int*>(ia + sa));
    CHECK(rng::find_first_of(
            as_lvalue(subrange(input_iterator<const int*>(ia),
                                 input_iterator<const int*>(ia))),
            subrange(forward_iterator<const int*>(ic),
                       forward_iterator<const int*>(ic + 1))) ==
            input_iterator<const int*>(ia));
    CHECK(rng::find_first_of(subrange(input_iterator<const int*>(ia),
                                        input_iterator<const int*>(ia + sa)),
                             subrange(forward_iterator<const int*>(ic),
                                        forward_iterator<const int*>(
                                                ic + 1))) ==
            input_iterator<const int*>(ia + sa));
    CHECK(rng::find_first_of(subrange(input_iterator<const int*>(ia),
                                        input_iterator<const int*>(ia + sa)),
                             subrange(forward_iterator<const int*>(ic),
                                        forward_iterator<const int*>(
                                                ic))) ==
            input_iterator<const int*>(ia + sa));
    CHECK(rng::find_first_of(subrange(input_iterator<const int*>(ia),
                                        input_iterator<const int*>(ia)),
                             subrange(forward_iterator<const int*>(ic),
                                        forward_iterator<const int*>(
                                                ic + 1))) ==
            input_iterator<const int*>(ia));
}

void test_rng_pred()
{
    using nano::subrange;
    int ia[] = {0, 1, 2, 3, 0, 1, 2, 3};
    static constexpr unsigned sa = nano::size(ia);
    int ib[] = {1, 3, 5, 7};
    static constexpr unsigned sb = nano::size(ib);
    CHECK(rng::find_first_of(
            as_lvalue(subrange(input_iterator<const int*>(ia),
                                 input_iterator<const int*>(ia + sa))),
            subrange(forward_iterator<const int*>(ib),
                       forward_iterator<const int*>(ib + sb)),
            std::equal_to<int>()) ==
            input_iterator<const int*>(ia + 1));
    int ic[] = {7};
    CHECK(rng::find_first_of(
            as_lvalue(subrange(input_iterator<const int*>(ia),
                                 input_iterator<const int*>(ia + sa))),
            subrange(forward_iterator<const int*>(ic),
                       forward_iterator<const int*>(ic + 1)),
            std::equal_to<int>()) ==
            input_iterator<const int*>(ia + sa));
    CHECK(rng::find_first_of(
            as_lvalue(subrange(input_iterator<const int*>(ia),
                                 input_iterator<const int*>(ia + sa))),
            subrange(forward_iterator<const int*>(ic),
                       forward_iterator<const int*>(ic)),
            std::equal_to<int>()) ==
            input_iterator<const int*>(ia + sa));
    CHECK(rng::find_first_of(
            as_lvalue(subrange(input_iterator<const int*>(ia),
                                 input_iterator<const int*>(ia))),
            subrange(forward_iterator<const int*>(ic),
                       forward_iterator<const int*>(ic + 1)),
            std::equal_to<int>()) ==
            input_iterator<const int*>(ia));
}

struct S {
    int i;
};

void test_rng_pred_proj()
{
    using nano::subrange;
    S ia[] = {S{0}, S{1}, S{2}, S{3}, S{0}, S{1}, S{2}, S{3}};
    static constexpr unsigned sa = nano::size(ia);
    S ib[] = {S{1}, S{3}, S{5}, S{7}};
    static constexpr unsigned sb = nano::size(ib);
    CHECK(rng::find_first_of(as_lvalue(subrange(input_iterator<const S*>(ia),
                                                  input_iterator<const S*>(
                                                          ia + sa))),
                             subrange(forward_iterator<const S*>(ib),
                                        forward_iterator<const S*>(ib + sb)),
                             std::equal_to<int>(), &S::i, &S::i) ==
            input_iterator<const S*>(ia + 1));
    S ic[] = {S{7}};
    CHECK(rng::find_first_of(as_lvalue(subrange(input_iterator<const S*>(ia),
                                                  input_iterator<const S*>(
                                                          ia + sa))),
                             subrange(forward_iterator<const S*>(ic),
                                        forward_iterator<const S*>(ic + 1)),
                             std::equal_to<int>(), &S::i, &S::i) ==
            input_iterator<const S*>(ia + sa));
    CHECK(rng::find_first_of(as_lvalue(subrange(input_iterator<const S*>(ia),
                                                  input_iterator<const S*>(
                                                          ia + sa))),
                             subrange(forward_iterator<const S*>(ic),
                                        forward_iterator<const S*>(ic)),
                             std::equal_to<int>(), &S::i, &S::i) ==
            input_iterator<const S*>(ia + sa));
    CHECK(rng::find_first_of(as_lvalue(subrange(input_iterator<const S*>(ia),
                                                  input_iterator<const S*>(
                                                          ia))),
                             subrange(forward_iterator<const S*>(ic),
                                        forward_iterator<const S*>(ic + 1)),
                             std::equal_to<int>(), &S::i, &S::i) ==
            input_iterator<const S*>(ia));
}

}

TEST_CASE("alg.find_first_of")
{
	::test_iter();
	::test_iter_pred();
	::test_rng();
	::test_rng_pred();
	::test_rng_pred_proj();
}
