// cmcstl2 - A concept-enabled C++ standard library
//
//  Copyright Casey Carter 2015
//
//  Use, modification and distribution is subject to the
//  Boost Software License, Version 1.0. (See accompanying
//  file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt)
//
// Project home: https://github.com/caseycarter/cmcstl2
//
#include "validate.hpp"

#include <nanorange/concepts.hpp>

namespace models = nano::ranges;
namespace ns = nano::ranges;

#include <iostream>
#include <type_traits>

//CONCEPT_ASSERT(models::Same<>);
CONCEPT_ASSERT(models::same_as<int, int>);
CONCEPT_ASSERT(models::same_as<double, double>);
//CONCEPT_ASSERT(models::Same<double>);
CONCEPT_ASSERT(!models::same_as<double, int>);
CONCEPT_ASSERT(!models::same_as<int, double>);

//CONCEPT_ASSERT(models::Same<int, int, int, int>);
//CONCEPT_ASSERT(!models::Same<int, int, double, int>);

namespace convertible_to_test {
struct A {};
struct B : A {};

CONCEPT_ASSERT(models::convertible_to<A, A>);
CONCEPT_ASSERT(models::convertible_to<B, B>);
CONCEPT_ASSERT(!models::convertible_to<A, B>);
CONCEPT_ASSERT(models::convertible_to<B, A>);
CONCEPT_ASSERT(models::convertible_to<int, double>);
CONCEPT_ASSERT(models::convertible_to<double, int>);
CONCEPT_ASSERT(models::convertible_to<void, void>);
}

namespace common_test {
CONCEPT_ASSERT(models::same_as<ns::common_type_t<int, int>, int>);
CONCEPT_ASSERT(models::same_as<ns::common_type_t<int, float, double>, double>);

CONCEPT_ASSERT(models::common_with<int, int>);
CONCEPT_ASSERT(models::common_with<int, double>);
CONCEPT_ASSERT(models::common_with<double, int>);
CONCEPT_ASSERT(models::common_with<double, double>);
CONCEPT_ASSERT(!models::common_with<void, int>);
CONCEPT_ASSERT(!models::common_with<int*, int>);
// FIXME: MSVC
//CONCEPT_ASSERT(models::common_with<void*, int*>);
CONCEPT_ASSERT(models::common_with<double,long long>);
CONCEPT_ASSERT(models::common_with<void, void>);
//CONCEPT_ASSERT(models::common_with<int, float, double>);
//CONCEPT_ASSERT(!models::common_with<int, float, double, void>);

struct B {};
struct C { C() = default; C(B) {} C(int) {} };
CONCEPT_ASSERT(models::common_with<B,C>);
//CONCEPT_ASSERT(models::common_with<int, C, B>);

struct incomplete;
// FIXME: MSVC
//CONCEPT_ASSERT(models::common_with<void*, incomplete*>);
}

namespace {
struct A { A() = default; A(int) {} };

enum class result {
	exact, convertible, unrelated
};

// C++11 version
result f(A) {
	std::cout << "exactly A\n";
	return result::exact;
}

template <class T, class U>
constexpr bool convertible_to = nano::convertible_to<T, U>;

template <class T>
std::enable_if_t<
	convertible_to<T,A> &&
		!std::is_same<A,T>::value,
	result>
f(T) {
	std::cout << "Convertible to A\n";
	return result::convertible;
}

template <class T>
std::enable_if_t<
	!(convertible_to<T, A> ||
		std::is_same<A,T>::value),
	result>
f(T) {
	std::cout << "Nothing to do with A\n";
	return result::unrelated;
}

} // unnamed namespace

TEST_CASE("concepts.core")
{
	CHECK(f(A{}) == result::exact);
	{ const A a{}; CHECK(f(a) == result::exact); }
	CHECK(f(42) == result::convertible);
	CHECK(f("foo") == result::unrelated);
}
