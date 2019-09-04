// test/views/elements_view.cpp
//
// Copyright (c) 2019 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#include <nanorange/views/elements.hpp>
#include <nanorange/algorithm/equal.hpp>
#include <nanorange/algorithm/find.hpp>

#include <nanorange/views/reverse.hpp>
#include <nanorange/views/take.hpp>
#include <nanorange/views/drop.hpp>
#include <nanorange/views/single.hpp>

#include <array>
#include <map>
#include <unordered_map>

#include "../catch.hpp"
#include "../test_utils.hpp"

namespace ranges = nano::ranges;
namespace views = ranges::views;
using namespace std::string_view_literals;

namespace {

constexpr bool test_constexpr()
{
    const std::array<std::array<int, 2>, 3> arr{
        std::array<int, 2>{1, 1}, {2, 2}, {3, 3}
    };

    const auto keys = arr | views::keys;
    static_assert(ranges::random_access_range<decltype(keys)>);

    const auto check = {1, 2, 3};
    if (!ranges::equal(keys, check)) {
        return false;
    }

    const auto vals = arr | views::values;
    static_assert(ranges::random_access_range<decltype(vals)>);

    return ranges::equal(vals, check);
}

}

TEST_CASE("views.elements")
{
    auto contains = [](const auto& rng, const auto& val) {
      return ranges::find(rng, val) != ranges::end(rng);
    };

    {
        auto historical_figures = std::map<std::string_view, int>{
            {"Lovelace"sv, 1815},
            {"Turing"sv, 1912},
            {"Babbage"sv, 1791},
            {"Hamilton"sv, 1936}
        };

        auto names = historical_figures | views::elements<0>;
        static_assert(ranges::view<decltype(names)>);
        static_assert(ranges::bidirectional_range<decltype(names)>);
        static_assert(ranges::sized_range<decltype(names)>);
        static_assert(ranges::same_as<ranges::range_value_t<decltype(names)>, std::string_view>);

        CHECK(ranges::size(names) == 4);
        CHECK(contains(names, "Lovelace"sv));
        CHECK(contains(names, "Turing"sv));
        CHECK(contains(names, "Babbage"sv));
        CHECK(contains(names, "Hamilton"sv));

        auto vals = historical_figures | views::values;
        static_assert(ranges::view<decltype(vals)>);
        static_assert(ranges::bidirectional_range<decltype(vals)>);
        static_assert(ranges::sized_range<decltype(vals)>);
        static_assert(ranges::same_as<ranges::range_value_t<decltype(vals)>, int>);

        CHECK(contains(vals, 1815));
        CHECK(contains(vals, 1912));
        CHECK(contains(vals, 1791));
        CHECK(contains(vals, 1936));
    }

    {
        auto historical_figures = std::unordered_map<std::string_view, int>{
            {"Lovelace"sv, 1815},
            {"Turing"sv, 1912},
            {"Babbage"sv, 1791},
            {"Hamilton"sv, 1936}
        };

        auto names = historical_figures | views::elements<0>;
        static_assert(ranges::view<decltype(names)>);
        static_assert(ranges::forward_range<decltype(names)>);
        static_assert(ranges::sized_range<decltype(names)>);
        static_assert(ranges::same_as<ranges::range_value_t<decltype(names)>, std::string_view>);

        CHECK(ranges::size(names) == 4);
        CHECK(contains(names, "Lovelace"sv));
        CHECK(contains(names, "Turing"sv));
        CHECK(contains(names, "Babbage"sv));
        CHECK(contains(names, "Hamilton"sv));

        auto vals = historical_figures | views::values;
        static_assert(ranges::view<decltype(vals)>);
        static_assert(ranges::forward_range<decltype(vals)>);
        static_assert(ranges::sized_range<decltype(vals)>);
        static_assert(ranges::same_as<ranges::range_value_t<decltype(vals)>, int>);

        CHECK(contains(vals, 1815));
        CHECK(contains(vals, 1912));
        CHECK(contains(vals, 1791));
        CHECK(contains(vals, 1936));
    }

    {
        using map_t = std::vector<std::pair<std::string_view, int>>;
        auto historical_figures = map_t{
            {"Lovelace"sv, 1815},
            {"Turing"sv, 1912},
            {"Babbage"sv, 1791},
            {"Hamilton"sv, 1936}
        };

        auto names = historical_figures | views::elements<0>;
        static_assert(ranges::view<decltype(names)>);
        static_assert(ranges::random_access_range<decltype(names)>);
        static_assert(ranges::sized_range<decltype(names)>);
        static_assert(ranges::same_as<ranges::range_value_t<decltype(names)>, std::string_view>);

        CHECK(ranges::size(names) == 4);
        ::check_equal(names, {"Lovelace"sv, "Turing"sv, "Babbage"sv, "Hamilton"sv});

        auto vals = historical_figures | views::values;
        static_assert(ranges::view<decltype(vals)>);
        static_assert(ranges::random_access_range<decltype(vals)>);
        static_assert(ranges::sized_range<decltype(vals)>);
        static_assert(ranges::same_as<ranges::range_value_t<decltype(vals)>, int>);

        ::check_equal(vals, {1815, 1912, 1791, 1936});
    }

    {
        using tuple_t = std::tuple<int, int, int>;
        auto vec = std::vector<tuple_t> {
            tuple_t{1, 1, 1},
            {2, 2, 2},
            {3, 3, 3},
            {4, 4 ,4}
        };

        auto elems = vec
            | views::reverse
            | views::drop(1)
            | views::take(1)
            | views::elements<2>;

        CHECK(ranges::equal(elems, views::single(3)));
    }

    static_assert(test_constexpr());
}