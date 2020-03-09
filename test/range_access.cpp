// cmcstl2 - A concept-enabled C++ standard library
//
//  Copyright Casey Carter 2015
//  Copyright Eric Niebler 2015
//
//  Use, modification and distribution is subject to the
//  Boost Software License, Version 1.0. (See accompanying
//  file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt)
//
// Project home: https://github.com/caseycarter/cmcstl2
//

#include <nanorange/algorithm/find.hpp>
#include <nanorange/views/subrange.hpp>
#include <nanorange/views/ref.hpp>
#include <nanorange/views/iota.hpp>
#include <iostream>
#include <vector>
#include <utility>
#include "catch.hpp"

namespace ranges = nano::ranges;

namespace {

void test_range_access_ambiguity()
{
    std::vector<ranges::reverse_iterator<int*>> vri{};
    using namespace ranges;
    (void) begin(vri);
    (void) end(vri);
    (void) cbegin(vri);
    (void) cend(vri);
    (void) rbegin(vri);
    (void) rend(vri);
    (void) crbegin(vri);
    (void) crend(vri);
}

void test_initializer_list()
{
    std::initializer_list<int> il = {0, 1, 2};
    {
        int count = 0;
        for (auto p = ranges::begin(il), e = ranges::end(il); p != e; ++p) {
            CHECK(*p == count++);
        }
    }
    {
        int count = 3;
        for (auto p = ranges::rbegin(il), e = ranges::rend(il); p != e; ++p) {
            CHECK(*p == --count);
        }
    }
    CHECK(ranges::size(il) == std::size_t{3});
    CHECK(ranges::data(il) == &*il.begin());
    CHECK(ranges::empty(il) == false);
}

template <class T, std::size_t... Is>
void test_array(std::index_sequence<Is...>)
{
    T a[sizeof...(Is)] = {Is...};
    {
        int count = 0;
        for (auto p = ranges::begin(a), e = ranges::end(a); p != e; ++p) {
            CHECK(*p == count++);
        }
    }
    {
        int count = sizeof...(Is);
        for (auto p = ranges::rbegin(a), e = ranges::rend(a); p != e; ++p) {
            CHECK(*p == --count);
        }
    }
    CHECK(ranges::size(a) == sizeof...(Is));
    CHECK(ranges::data(a) == a + 0);
    CHECK(ranges::empty(a) == false);
}

namespace begin_testing {

struct CanBegin_concept {
    template <class R>
    auto requires_(R&& r) -> decltype(
        ranges::begin((R &&) r)
    );
};

template <class R>
NANO_CONCEPT CanBegin = ranges::detail::requires_<CanBegin_concept, R>;

struct CanCBegin_concept {
    template <class R>
    auto requires_(R&& r) -> decltype(
        ranges::cbegin((R &&) r)
    );
};

template <class R>
NANO_CONCEPT CanCBegin = ranges::detail::requires_<CanCBegin_concept, R>;

struct A {
    int* begin();
    int* end();
    const int* begin() const;
    const int* end() const;
};

struct B : A {};
void* begin(B&);

struct C : A {};
void begin(C&);

struct D : A {};
char* begin(D&);

void test()
{
    // Valid
    static_assert(CanBegin<int(&)[2]>);
    static_assert(
        ranges::same_as<decltype(ranges::begin(std::declval<int(&)[2]>())),
                        int*>);
    static_assert(CanBegin<const int(&)[2]>);
    static_assert(ranges::same_as<decltype(ranges::begin(
                                      std::declval<const int(&)[2]>())),
                                  const int*>);

    static_assert(CanCBegin<int(&)[2]>);
    static_assert(
        ranges::same_as<decltype(ranges::cbegin(std::declval<int(&)[2]>())),
                        const int*>);
    static_assert(CanCBegin<const int(&)[2]>);
    static_assert(ranges::same_as<decltype(ranges::cbegin(
                                      std::declval<const int(&)[2]>())),
                                  const int*>);

    // Ill-formed: array rvalue
    static_assert(!CanBegin<int(&&)[2]>);
    static_assert(!CanBegin<const int(&&)[2]>);

    static_assert(!CanCBegin<int(&&)[2]>);
    static_assert(!CanCBegin<const int(&&)[2]>);

    // Valid: only member begin
    static_assert(CanBegin<A&>);
    static_assert(!CanBegin<A>);
    static_assert(
        ranges::same_as<decltype(ranges::begin(std::declval<A&>())), int*>);
    static_assert(CanBegin<const A&>);
    static_assert(!CanBegin<const A>);
    static_assert(
        ranges::same_as<decltype(ranges::begin(std::declval<const A&>())),
                        const int*>);

    // Valid: Both member and non-member begin, but non-member returns non-input_or_output_iterator.
    static_assert(CanBegin<B&>);
    static_assert(!CanBegin<B>);
    static_assert(
        ranges::same_as<decltype(ranges::begin(std::declval<B&>())), int*>);
    static_assert(CanBegin<const B&>);
    static_assert(!CanBegin<const B>);
    static_assert(
        ranges::same_as<decltype(ranges::begin(std::declval<const B&>())),
                        const int*>);

    // Valid: Both member and non-member begin, but non-member returns non-input_or_output_iterator.
    static_assert(CanBegin<C&>);
    static_assert(!CanBegin<C>);
    static_assert(CanBegin<const C&>);
    static_assert(!CanBegin<const C>);

    // Valid: Prefer member begin
    static_assert(CanBegin<D&>);
    static_assert(!CanBegin<D>);
    static_assert(
        ranges::same_as<int*, decltype(ranges::begin(std::declval<D&>()))>);
    static_assert(CanBegin<const D&>);
    static_assert(!CanBegin<const D>);
    static_assert(ranges::same_as<const int*, decltype(ranges::begin(
                                                  std::declval<const D&>()))>);

    {
        using T = std::initializer_list<int>;
        // Valid: begin accepts lvalue initializer_list
        static_assert(ranges::same_as<const int*, decltype(ranges::begin(
                                                      std::declval<T&>()))>);
        static_assert(
            ranges::same_as<const int*,
                            decltype(ranges::begin(std::declval<const T&>()))>);
        static_assert(!CanBegin<std::initializer_list<int>>);
        static_assert(!CanBegin<const std::initializer_list<int>>);
    }

    static_assert(CanBegin<ranges::subrange<int*, int*>&>);
    static_assert(CanBegin<const ranges::subrange<int*, int*>&>);
    static_assert(CanBegin<ranges::subrange<int*, int*>>);
    static_assert(CanBegin<const ranges::subrange<int*, int*>>);

    static_assert(CanCBegin<ranges::subrange<int*, int*>&>);
    static_assert(CanCBegin<const ranges::subrange<int*, int*>&>);
    static_assert(CanCBegin<ranges::subrange<int*, int*>>);
    static_assert(CanCBegin<const ranges::subrange<int*, int*>>);

    static_assert(CanBegin<ranges::ref_view<int[5]>&>);
    static_assert(CanBegin<const ranges::ref_view<int[5]>&>);
    static_assert(CanBegin<ranges::ref_view<int[5]>>);
    static_assert(CanBegin<const ranges::ref_view<int[5]>>);

    static_assert(CanCBegin<ranges::ref_view<int[5]>&>);
    static_assert(CanCBegin<const ranges::ref_view<int[5]>&>);
    static_assert(CanCBegin<ranges::ref_view<int[5]>>);
    static_assert(CanCBegin<const ranges::ref_view<int[5]>>);

    static_assert(CanBegin<ranges::iota_view<int, int>&>);
    static_assert(CanBegin<const ranges::iota_view<int, int>&>);
    static_assert(CanBegin<ranges::iota_view<int, int>>);
    static_assert(CanBegin<const ranges::iota_view<int, int>>);

    static_assert(CanCBegin<ranges::iota_view<int, int>&>);
    static_assert(CanCBegin<const ranges::iota_view<int, int>&>);
    static_assert(CanCBegin<ranges::iota_view<int, int>>);
    static_assert(CanCBegin<const ranges::iota_view<int, int>>);
}
} // namespace begin_testing

namespace X {
template <class T, std::size_t N>
/*requires(N != 0)*/ struct array {
    T elements_[N];

    constexpr bool empty() const noexcept { return N == 0; }
    constexpr T* data() noexcept { return elements_; }
    constexpr const T* data() const noexcept { return elements_; }
};

template <class T, std::size_t N>
constexpr T* begin(array<T, N>& a) noexcept
{
    return a.elements_;
}
template <class T, std::size_t N>
constexpr T* end(array<T, N>& a) noexcept
{
    return a.elements_ + N;
}
template <class T, std::size_t N>
constexpr const T* begin(const array<T, N>& a) noexcept
{
    return a.elements_;
}
template <class T, std::size_t N>
constexpr const T* end(const array<T, N>& a) noexcept
{
    return a.elements_ + N;
}

template <class T, std::size_t N>
/*requires(N != 0)*/ struct non_constexpr_array {
    T elements_[N];

    bool empty() const noexcept { return N == 0; }
    T* data() noexcept { return elements_; }
    const T* data() const noexcept { return elements_; }
};

template <class T, std::size_t N>
T* begin(non_constexpr_array<T, N>& a) noexcept
{
    return a.elements_;
}
template <class T, std::size_t N>
T* end(non_constexpr_array<T, N>& a) noexcept
{
    return a.elements_ + N;
}
template <class T, std::size_t N>
const T* begin(const non_constexpr_array<T, N>& a) noexcept
{
    return a.elements_;
}
template <class T, std::size_t N>
const T* end(const non_constexpr_array<T, N>& a) noexcept
{
    return a.elements_ + N;
}
} // namespace X

using I = int*;
using CI = const int*;
static_assert(ranges::input_or_output_iterator<I>);
static_assert(ranges::input_or_output_iterator<CI>);

void test_string_view_p0970()
{
    // basic_string_views are non-dangling
    using I = ranges::iterator_t<std::string_view>;
    static_assert(ranges::same_as<I, decltype(ranges::begin(
                                         std::declval<std::string_view>()))>);
    static_assert(ranges::same_as<I, decltype(ranges::end(
                                         std::declval<std::string_view>()))>);
    static_assert(
        ranges::same_as<I, decltype(ranges::begin(
                               std::declval<const std::string_view>()))>);
    static_assert(
        ranges::same_as<I, decltype(ranges::end(
                               std::declval<const std::string_view>()))>);

    {
        const char hw[] = "Hello, World!";
        auto result = ranges::find(std::string_view{hw}, 'W');
        static_assert(ranges::same_as<I, decltype(result)>);
        CHECK(result == std::string_view{hw}.begin() + 7);
    }
}

}

TEST_CASE("range_access") {
	using namespace ranges;

	static constexpr X::array<int, 4> some_ints = {{0,1,2,3}};

	constexpr auto first = begin(some_ints);
	constexpr auto last = end(some_ints);
	static_assert(ranges::same_as<const CI, decltype(first)>);
	static_assert(ranges::same_as<const CI, decltype(last)>);
	static_assert(first == cbegin(some_ints));
	static_assert(last == cend(some_ints));

	{
		X::non_constexpr_array<int, 4> not_a_constant_expression{{0,1,2,3}};
		static_assert(noexcept(begin(not_a_constant_expression)));
		static_assert(noexcept(end(not_a_constant_expression)));
		static_assert(noexcept(cbegin(not_a_constant_expression)));
		static_assert(noexcept(cend(not_a_constant_expression)));
		static_assert(noexcept(empty(not_a_constant_expression)));
		static_assert(noexcept(data(not_a_constant_expression)));
	}

	constexpr bool output = false;
	static_assert(!empty(some_ints));
	if (output) std::cout << '{';
	auto is_first = true;
	auto count = 0;
	for (auto&& i : some_ints) {
		CHECK(i == count++);
		if (is_first) {
			is_first = false;
		} else {
			if (output) std::cout << ", ";
		}
		if (output) std::cout << i;
	}
	if (output) std::cout << "}\n";

	test_initializer_list();
	test_array<int>(std::make_index_sequence<3>{});
	test_array<const int>(std::make_index_sequence<3>{});
	begin_testing::test();

	test_string_view_p0970();
}
