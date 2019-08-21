// cmcstl2 - A concept-enabled C++ standard library
//
//  Copyright Casey Carter 2018
//
//  Use, modification and distribution is subject to the
//  Boost Software License, Version 1.0. (See accompanying
//  file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt)
//
// Project home: https://github.com/caseycarter/cmcstl2
//
#include <nanorange/views/single.hpp>
#include <type_traits>
#include <utility>
#include "../catch.hpp"

namespace ranges = nano::ranges;

template<class T>
using remove_cvref_t = std::remove_cv_t<std::remove_reference_t<T>>;

namespace {

template <class V, class T>
void test_one(V& v, const T& t)
{
    static_assert(ranges::ContiguousRange<V>);
    static_assert(ranges::SizedRange<V>);
    static_assert(ranges::CommonRange<V>);
    using I = ranges::iterator_t<V>;
    static_assert(std::is_pointer_v<I>);
    static_assert(ranges::Same<T, ranges::iter_value_t<I>>);
    static_assert(V::size() == 1u);
    CHECK(v.size() == 1u);
    CHECK(v.data() != nullptr);
    CHECK(&*v.begin() == v.data());
    CHECK(v.end() == v.begin() + 1);
    CHECK(v.front() == t);
}

template <class T>
void test(T&& t)
{
    using D = remove_cvref_t<T>;
    auto v = ranges::views::single(std::forward<T>(t));
    static_assert(ranges::Same<ranges::single_view<D>, decltype(v)>);
    test_one(v, t);
    test_one(std::as_const(v), t);
}

}

TEST_CASE("views.single") {
    using namespace ranges;

    test(42);
    {
        int i = 42;
        test(i);
    }

    {
        const int i = 42;
        test(i);
    }

    constexpr single_view<int> s;
    static_assert(*s.begin() == 0);
}
