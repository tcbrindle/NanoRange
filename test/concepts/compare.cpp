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

#include <type_traits>
#include <bitset>

namespace models = nano::ranges;

namespace boolean_test {
// Better have at least these three, since we use them as
// examples in the TS draft.
CONCEPT_ASSERT(models::detail::boolean_testable<bool>);
CONCEPT_ASSERT(models::detail::boolean_testable<std::true_type>);
CONCEPT_ASSERT(models::detail::boolean_testable<std::bitset<42>::reference>);

CONCEPT_ASSERT(models::detail::boolean_testable<int>);
//CONCEPT_ASSERT(!models::detail::boolean_testable<void*>);

struct A {};
struct B { operator bool() const; };

CONCEPT_ASSERT(!models::detail::boolean_testable<A>);
CONCEPT_ASSERT(models::detail::boolean_testable<B>);
}

namespace equality_comparable_test {
struct A {
	friend bool operator==(const A&, const A&);
	friend bool operator!=(const A&, const A&);
};

CONCEPT_ASSERT(models::equality_comparable<int>);
CONCEPT_ASSERT(models::equality_comparable<A>);
CONCEPT_ASSERT(!models::equality_comparable<void>);
CONCEPT_ASSERT(models::equality_comparable<int&>);
CONCEPT_ASSERT(models::equality_comparable<std::nullptr_t>);

CONCEPT_ASSERT(models::equality_comparable_with<int, int>);
CONCEPT_ASSERT(models::equality_comparable_with<A, A>);
CONCEPT_ASSERT(!models::equality_comparable_with<void, void>);
CONCEPT_ASSERT(models::equality_comparable_with<int&, int>);
} // namespace equality_comparable_test

CONCEPT_ASSERT(models::totally_ordered<int>);
CONCEPT_ASSERT(models::totally_ordered<float>);
CONCEPT_ASSERT(!models::totally_ordered<void>);
CONCEPT_ASSERT(models::totally_ordered<int&>);

CONCEPT_ASSERT(models::totally_ordered_with<int, int>);
CONCEPT_ASSERT(models::totally_ordered_with<int, double>);
CONCEPT_ASSERT(!models::totally_ordered_with<int, void>);
CONCEPT_ASSERT(models::totally_ordered_with<int&, int>);

