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
CONCEPT_ASSERT(models::Same<int, int>);
CONCEPT_ASSERT(models::Same<double, double>);
//CONCEPT_ASSERT(models::Same<double>);
CONCEPT_ASSERT(!models::Same<double, int>);
CONCEPT_ASSERT(!models::Same<int, double>);

//CONCEPT_ASSERT(models::Same<int, int, int, int>);
//CONCEPT_ASSERT(!models::Same<int, int, double, int>);

namespace convertible_to_test {
struct A {};
struct B : A {};

CONCEPT_ASSERT(models::ConvertibleTo<A, A>);
CONCEPT_ASSERT(models::ConvertibleTo<B, B>);
CONCEPT_ASSERT(!models::ConvertibleTo<A, B>);
CONCEPT_ASSERT(models::ConvertibleTo<B, A>);
CONCEPT_ASSERT(models::ConvertibleTo<int, double>);
CONCEPT_ASSERT(models::ConvertibleTo<double, int>);
CONCEPT_ASSERT(models::ConvertibleTo<void, void>);
}

namespace common_test {
CONCEPT_ASSERT(models::Same<ns::common_type_t<int, int>, int>);
CONCEPT_ASSERT(models::Same<ns::common_type_t<int, float, double>, double>);

CONCEPT_ASSERT(models::Common<int, int>);
CONCEPT_ASSERT(models::Common<int, double>);
CONCEPT_ASSERT(models::Common<double, int>);
CONCEPT_ASSERT(models::Common<double, double>);
CONCEPT_ASSERT(!models::Common<void, int>);
CONCEPT_ASSERT(!models::Common<int*, int>);
// FIXME: MSVC
//CONCEPT_ASSERT(models::Common<void*, int*>);
CONCEPT_ASSERT(models::Common<double,long long>);
CONCEPT_ASSERT(models::Common<void, void>);
//CONCEPT_ASSERT(models::Common<int, float, double>);
//CONCEPT_ASSERT(!models::Common<int, float, double, void>);

struct B {};
struct C { C() = default; C(B) {} C(int) {} };
CONCEPT_ASSERT(models::Common<B,C>);
//CONCEPT_ASSERT(models::Common<int, C, B>);

struct incomplete;
// FIXME: MSVC
//CONCEPT_ASSERT(models::Common<void*, incomplete*>);
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
constexpr bool ConvertibleTo = nano::ConvertibleTo<T, U>;

template <class T>
std::enable_if_t<
	ConvertibleTo<T,A> &&
		!std::is_same<A,T>::value,
	result>
f(T) {
	std::cout << "Convertible to A\n";
	return result::convertible;
}

template <class T>
std::enable_if_t<
	!(ConvertibleTo<T, A> ||
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
