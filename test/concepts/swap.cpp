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

namespace models = nano;
namespace ns = nano;

namespace nano {
using std::declval;
}

#include <cstddef>

namespace swappable_test {
	CONCEPT_ASSERT(models::swappable<int>);
	CONCEPT_ASSERT(models::swappable_with<int&, int&>);
	CONCEPT_ASSERT(models::swappable<int[4]>);
// FIXME MSVC
#ifndef _MSC_VER
	CONCEPT_ASSERT(models::swappable_with<int(&)[4], int(&)[4]>);
#endif
	CONCEPT_ASSERT(!models::swappable_with<int, int>);
	CONCEPT_ASSERT(!models::swappable_with<int&, double&>);
	CONCEPT_ASSERT(!models::swappable_with<int(&)[4], bool(&)[4]>);
	CONCEPT_ASSERT(!models::swappable<int[]>);
	CONCEPT_ASSERT(!models::swappable<int[][4]>);

	CONCEPT_ASSERT(noexcept(ns::swap(ns::declval<int&>(), ns::declval<int&>())));
	//CONCEPT_ASSERT(ns::is_nothrow_swappable<int&, int&>());
	//CONCEPT_ASSERT(ns::is_nothrow_swappable<int(&)[42], int(&)[42]>());

//#if VALIDATE_STL2
	// range-v3 doesn't support swapping multidimensional arrays
	CONCEPT_ASSERT(models::swappable<int[3][4]>);
// FIXME MSVC
#ifndef _MSC_VER
	CONCEPT_ASSERT(models::swappable_with<int(&)[3][4], int(&)[3][4]>);
	CONCEPT_ASSERT(models::swappable<int[3][4][1][2]>);
	CONCEPT_ASSERT(models::swappable_with<int(&)[3][4][1][2], int(&)[3][4][1][2]>);
	CONCEPT_ASSERT(!models::swappable_with<int(&)[3][4][1][2], int(&)[4][4][1][2]>);
#endif
	//CONCEPT_ASSERT(ns::is_nothrow_swappable<int(&)[6][7], int(&)[6][7]>());

	struct unswappable : std::string { // Has std:: as an associated namespace
		unswappable() = default;
		unswappable(const unswappable&) = delete;
		unswappable(unswappable&&) = delete;
	};
	CONCEPT_ASSERT(!models::swappable_with<unswappable&, unswappable&>);
#if 0
	namespace __constrained_swappable {
		// Has a constrained swap findable via ADL:
		struct constrained_swappable {
			constrained_swappable() = default;
			constrained_swappable(const constrained_swappable&) = default;
			constrained_swappable(constrained_swappable&&) = default;
		};
		template <class T>
		constexpr bool ConstrainedSwappable =
			ns::Same<T, constrained_swappable>;
		template <ConstrainedSwappable T, ConstrainedSwappable U>
		void swap(T&, U&) {}
		template <ConstrainedSwappable T>
		void swap(T &, T &) {}
	}
	using __constrained_swappable::constrained_swappable;
	CONCEPT_ASSERT(models::SwappableWith<constrained_swappable&, constrained_swappable&>);
	CONCEPT_ASSERT(!models::SwappableWith<const volatile constrained_swappable&, const volatile constrained_swappable&>);
#endif

	namespace {
		struct A {
			A() = default;
			A(A&&) = delete;
			A& operator=(A&&) = delete;
			friend void swap(A&, A&) noexcept {}
		};

		CONCEPT_ASSERT(models::swappable<A>);
		CONCEPT_ASSERT(noexcept(ns::swap(ns::declval<A&>(), ns::declval<A&>())));
//		CONCEPT_ASSERT(ns::is_nothrow_swappable<A&, A&>());
	}

	namespace {
		struct B {
			friend void swap(B&, B&) {}
		};

		CONCEPT_ASSERT(models::swappable<B>);
		CONCEPT_ASSERT(!noexcept(ns::swap(ns::declval<B&>(), ns::declval<B&>())));
//		CONCEPT_ASSERT(!ns::is_nothrow_swappable<B&, B&>());
	}
} // namespace swappable_test

#if 0 // No longer functional
template <class T, std::size_t N>
struct array {
	T elements_[N];

	constexpr T& operator[](std::size_t i) noexcept {
		STL2_EXPECT(i < N);
		return elements_[i];
	}

	constexpr const T& operator[](std::size_t i) const noexcept {
		STL2_EXPECT(i < N);
		return elements_[i];
	}
};

template <class T, class U, std::size_t N>
	requires stl2::SwappableWith<T&, U&>
void swap(array<T, N>& a, array<U, N>& b)
	noexcept(noexcept(ns::swap(a.elements_, b.elements_))) {
	ns::swap(a.elements_, b.elements_);
}

template <class T, class U, std::size_t N>
	requires stl2::SwappableWithT&, U&>()
void swap(array<T, N>& a, U (&b)[N])
	noexcept(noexcept(ns::swap(a.elements_, b))) {
	ns::swap(a.elements_, b);
}

template <class T, class U, std::size_t N>
	requires stl2::SwappableWith<T&, U&>
void swap(T (&b)[N], array<U, N>& a)
	noexcept(noexcept(ns::swap(a.elements_, b))) {
	ns::swap(a.elements_, b);
}
#endif

TEST_CASE("concepts.swap")
{
	{
		int a[2][2] = {{0, 1}, {2, 3}};
		int b[2][2] = {{4, 5}, {6, 7}};

// FIXME MSVC
#ifndef _MSC_VER
		CONCEPT_ASSERT(models::swappable_with<decltype((a)),decltype((b))>);
#endif
		ns::swap(a, b);
		CONCEPT_ASSERT(noexcept(ns::swap(a, b)));

		CHECK(a[0][0] == 4);
		CHECK(a[0][1] == 5);
		CHECK(a[1][0] == 6);
		CHECK(a[1][1] == 7);

		CHECK(b[0][0] == 0);
		CHECK(b[0][1] == 1);
		CHECK(b[1][0] == 2);
		CHECK(b[1][1] == 3);
	}

#if 0
	{
		array<int, 4> a = {0,1,2,3};
		int b[4] = {4,5,6,7};

		CONCEPT_ASSERT(models::SwappableWith<decltype(a[0]),decltype(b[0])>);
		ns::swap(a[0], b[0]);
		CONCEPT_ASSERT(noexcept(ns::swap(a[0], b[0])));

		CONCEPT_ASSERT(models::SwappableWith<decltype((a)),decltype((b))>);
		ns::swap(a, b);
		CONCEPT_ASSERT(noexcept(ns::swap(a, b)));

		CHECK(a[0] == 0);
		CHECK(a[1] == 5);
		CHECK(a[2] == 6);
		CHECK(a[3] == 7);

		CHECK(b[0] == 4);
		CHECK(b[1] == 1);
		CHECK(b[2] == 2);
		CHECK(b[3] == 3);
	}

	{
		array<array<int, 2>, 3> a = {{{{0, 1}}, {{2, 3}}, {{4, 5}}}};
		int b[3][2] = {{6, 7}, {8, 9}, {10, 11}};

		CONCEPT_ASSERT(models::SwappableWith<decltype(a[0]),decltype(b[0])>);
		ns::swap(a[0], b[0]);
		CONCEPT_ASSERT(noexcept(ns::swap(a[0], b[0])));

		CONCEPT_ASSERT(models::SwappableWith<decltype((a)),decltype((b))>);
		ns::swap(a, b);
		CONCEPT_ASSERT(noexcept(ns::swap(a, b)));

		CHECK(a[0][0] == 0);
		CHECK(a[0][1] == 1);
		CHECK(a[1][0] == 8);
		CHECK(a[1][1] == 9);
		CHECK(a[2][0] == 10);
		CHECK(a[2][1] == 11);

		CHECK(b[0][0] == 6);
		CHECK(b[0][1] == 7);
		CHECK(b[1][0] == 2);
		CHECK(b[1][1] == 3);
		CHECK(b[2][0] == 4);
		CHECK(b[2][1] == 5);
	}
#endif
}
