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

#include <type_traits>
#include <bitset>

namespace models = nanorange;

namespace boolean_test {
// Better have at least these three, since we use them as
// examples in the TS draft.
CONCEPT_ASSERT(models::Boolean<bool>);
CONCEPT_ASSERT(models::Boolean<std::true_type>);
CONCEPT_ASSERT(models::Boolean<std::bitset<42>::reference>);

CONCEPT_ASSERT(models::Boolean<int>);
CONCEPT_ASSERT(!models::Boolean<void*>);

struct A {};
struct B { operator bool() const; };

CONCEPT_ASSERT(!models::Boolean<A>);
CONCEPT_ASSERT(models::Boolean<B>);
}

namespace equality_comparable_test {
struct A {
	friend bool operator==(const A&, const A&);
	friend bool operator!=(const A&, const A&);
};

CONCEPT_ASSERT(models::EqualityComparable<int>);
CONCEPT_ASSERT(models::EqualityComparable<A>);
CONCEPT_ASSERT(!models::EqualityComparable<void>);
CONCEPT_ASSERT(models::EqualityComparable<int&>);
CONCEPT_ASSERT(models::EqualityComparable<std::nullptr_t>);

CONCEPT_ASSERT(models::EqualityComparableWith<int, int>);
CONCEPT_ASSERT(models::EqualityComparableWith<A, A>);
CONCEPT_ASSERT(!models::EqualityComparableWith<void, void>);
CONCEPT_ASSERT(models::EqualityComparableWith<int&, int>);
} // namespace equality_comparable_test

CONCEPT_ASSERT(models::StrictTotallyOrdered<int>);
CONCEPT_ASSERT(models::StrictTotallyOrdered<float>);
CONCEPT_ASSERT(!models::StrictTotallyOrdered<void>);
CONCEPT_ASSERT(models::StrictTotallyOrdered<int&>);

CONCEPT_ASSERT(models::StrictTotallyOrderedWith<int, int>);
CONCEPT_ASSERT(models::StrictTotallyOrderedWith<int, double>);
CONCEPT_ASSERT(!models::StrictTotallyOrderedWith<int, void>);
CONCEPT_ASSERT(models::StrictTotallyOrderedWith<int&, int>);

