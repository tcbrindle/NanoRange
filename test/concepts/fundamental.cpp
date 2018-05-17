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

#include <nanorange.hpp>

namespace models = nanorange;

#include <cstddef>

CONCEPT_ASSERT(models::Integral<int>);
CONCEPT_ASSERT(!models::Integral<double>);
CONCEPT_ASSERT(models::Integral<unsigned>);
CONCEPT_ASSERT(!models::Integral<void>);
CONCEPT_ASSERT(models::Integral<std::ptrdiff_t>);
CONCEPT_ASSERT(models::Integral<std::size_t>);

CONCEPT_ASSERT(models::SignedIntegral<int>);
CONCEPT_ASSERT(!models::SignedIntegral<double>);
CONCEPT_ASSERT(!models::SignedIntegral<unsigned>);
CONCEPT_ASSERT(!models::SignedIntegral<void>);
CONCEPT_ASSERT(models::SignedIntegral<std::ptrdiff_t>);
CONCEPT_ASSERT(!models::SignedIntegral<std::size_t>);

CONCEPT_ASSERT(!models::UnsignedIntegral<int>);
CONCEPT_ASSERT(!models::UnsignedIntegral<double>);
CONCEPT_ASSERT(models::UnsignedIntegral<unsigned>);
CONCEPT_ASSERT(!models::UnsignedIntegral<void>);
CONCEPT_ASSERT(!models::UnsignedIntegral<std::ptrdiff_t>);
CONCEPT_ASSERT(models::UnsignedIntegral<std::size_t>);

#if VALIDATE_STL2
namespace scalar_types {
enum class t {
	regular, scalar, arithmetic, floating_point,
	integral, signed_integral, unsigned_integral, ull
};

constexpr t f(__stl2::Regular) { return t::regular; }
constexpr t f(__stl2::ext::Scalar) { return t::scalar; }
constexpr t f(__stl2::ext::Arithmetic) { return t::arithmetic; }
constexpr t f(__stl2::ext::FloatingPoint) { return t::floating_point; }
constexpr t f(__stl2::Integral) { return t::integral; }
constexpr t f(__stl2::SignedIntegral) { return t::signed_integral; }
constexpr t f(__stl2::UnsignedIntegral) { return t::unsigned_integral; }
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

