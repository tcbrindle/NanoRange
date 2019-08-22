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

#include <cstddef>

CONCEPT_ASSERT(models::integral<int>);
CONCEPT_ASSERT(!models::integral<double>);
CONCEPT_ASSERT(models::integral<unsigned>);
CONCEPT_ASSERT(!models::integral<void>);
CONCEPT_ASSERT(models::integral<std::ptrdiff_t>);
CONCEPT_ASSERT(models::integral<std::size_t>);

CONCEPT_ASSERT(models::signed_integral<int>);
CONCEPT_ASSERT(!models::signed_integral<double>);
CONCEPT_ASSERT(!models::signed_integral<unsigned>);
CONCEPT_ASSERT(!models::signed_integral<void>);
CONCEPT_ASSERT(models::signed_integral<std::ptrdiff_t>);
CONCEPT_ASSERT(!models::signed_integral<std::size_t>);

CONCEPT_ASSERT(!models::unsigned_integral<int>);
CONCEPT_ASSERT(!models::unsigned_integral<double>);
CONCEPT_ASSERT(models::unsigned_integral<unsigned>);
CONCEPT_ASSERT(!models::unsigned_integral<void>);
CONCEPT_ASSERT(!models::unsigned_integral<std::ptrdiff_t>);
CONCEPT_ASSERT(models::unsigned_integral<std::size_t>);

#if VALIDATE_STL2
namespace scalar_types {
enum class t {
	regular, scalar, arithmetic, floating_point,
	integral, signed_integral, unsigned_integral, ull
};

constexpr t f(__stl2::regular) { return t::regular; }
constexpr t f(__stl2::ext::Scalar) { return t::scalar; }
constexpr t f(__stl2::ext::Arithmetic) { return t::arithmetic; }
constexpr t f(__stl2::ext::FloatingPoint) { return t::floating_point; }
constexpr t f(__stl2::integral) { return t::integral; }
constexpr t f(__stl2::signed_integral) { return t::signed_integral; }
constexpr t f(__stl2::unsigned_integral) { return t::unsigned_integral; }
constexpr t f(unsigned long long) { return t::ull; }

void test() {
	CHECK(f(0.0f) == t::floating_point);
	CHECK(f(0.0) == t::floating_point);
	CHECK(f(0) == t::signed_integral);
	CHECK(f(0u) == t::unsigned_integral);
	CHECK(f(nullptr) == t::scalar);
	CHECK(f(0ull) == t::ull);
	CHECK((f('a') == t::signed_integral || f('a') == t::unsigned_integral));
	{
		int i;
		CHECK(f(&i) == t::scalar);
	}
	{
		struct A { void foo() {} };
		CHECK(f(&A::foo) == t::scalar);
	}
}
}
#endif

