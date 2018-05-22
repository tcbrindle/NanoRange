// cmcstl2 - A concept-enabled C++ standard library
//
//  Copyright Eric Niebler 2014
//  Copyright Casey Carter 2015
//
//  Use, modification and distribution is subject to the
//  Boost Software License, Version 1.0. (See accompanying
//  file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt)
//
// Project home: https://github.com/caseycarter/cmcstl2
//
//// Adapted (stolen) from: Range v3 library
////
////// These tests of reverse_iterator have been adapted from libc++
////// (http://libcxx.llvm.org).
//////
//////===----------------------------------------------------------------------===//
//////
//////                     The LLVM Compiler Infrastructure
//////
////// This file is dual licensed under the MIT and the University of Illinois Open
////// Source Licenses. See LICENSE.TXT for details.
//////
//////===----------------------------------------------------------------------===//
//
#include <stl2/iterator.hpp>
#include "../simple_test.hpp"
#include "../test_iterators.hpp"

template <class It> void test() { __stl2::reverse_iterator<It>{}; }

template <class It> void test2(It i) {
	__stl2::reverse_iterator<It> r(i);
	CHECK(r.base() == i);
}

template <class It, class U> void test3(U u) {
	const __stl2::reverse_iterator<U> r2(u);
	__stl2::reverse_iterator<It> r1 = r2;
	CHECK(r1.base() == u);
}

struct Base {};
struct Derived : Base {};

template <class It> void test4(It i) {
	const auto r = __stl2::make_reverse_iterator(i);
	static_assert(std::is_same<decltype(r), const __stl2::reverse_iterator<It>>());
	CHECK(r.base() == i);
}

template <class It> void test5(It l, It r, bool x) {
	const __stl2::reverse_iterator<It> r1(l);
	const __stl2::reverse_iterator<It> r2(r);
	CHECK((r1 != r2) == x);
}

template <class It> void test6(It i, It x) {
	__stl2::reverse_iterator<It> r(i);
	__stl2::reverse_iterator<It> rr = r++;
	CHECK(r.base() == x);
	CHECK(rr.base() == i);
}

template <class It> void test7(It i, It x) {
	__stl2::reverse_iterator<It> r(i);
	__stl2::reverse_iterator<It> &rr = ++r;
	CHECK(r.base() == x);
	CHECK(&rr == &r);
}

template <class It>
void test8(It i, __stl2::difference_type_t<It> n, It x) {
	const __stl2::reverse_iterator<It> r(i);
	__stl2::reverse_iterator<It> rr = r + n;
	CHECK(rr.base() == x);
}

template <class It>
void test9(It i, __stl2::difference_type_t<It> n, It x) {
	__stl2::reverse_iterator<It> r(i);
	__stl2::reverse_iterator<It> &rr = r += n;
	CHECK(r.base() == x);
	CHECK(&rr == &r);
}

template <class It> void test10(It i, It x) {
	__stl2::reverse_iterator<It> r(i);
	__stl2::reverse_iterator<It> rr = r--;
	CHECK(r.base() == x);
	CHECK(rr.base() == i);
}
template <class It> void test11(It i, It x) {
	__stl2::reverse_iterator<It> r(i);
	__stl2::reverse_iterator<It> &rr = --r;
	CHECK(r.base() == x);
	CHECK(&rr == &r);
}
template <class It>
void test12(It i, __stl2::difference_type_t<It> n, It x) {
	const __stl2::reverse_iterator<It> r(i);
	__stl2::reverse_iterator<It> rr = r - n;
	CHECK(rr.base() == x);
}

template <class It>
void test13(It i, __stl2::difference_type_t<It> n, It x) {
	__stl2::reverse_iterator<It> r(i);
	__stl2::reverse_iterator<It> &rr = r -= n;
	CHECK(r.base() == x);
	CHECK(&rr == &r);
}

class A {
	int data_ = 1;

public:
	A() = default;

	friend bool operator==(const A &x, const A &y) { return x.data_ == y.data_; }
};

template <class It> void test14(It i, __stl2::value_type_t<It> x) {
	__stl2::reverse_iterator<It> r(i);
	CHECK(*r == x);
}

template <class It, class U> void test15(U u) {
	const __stl2::reverse_iterator<U> r2(u);
	__stl2::reverse_iterator<It> r1;
	__stl2::reverse_iterator<It> &rr = r1 = r2;
	CHECK(r1.base() == u);
	CHECK(&rr == &r1);
}
template <class It> void test16(It l, It r, bool x) {
	const __stl2::reverse_iterator<It> r1(l);
	const __stl2::reverse_iterator<It> r2(r);
	CHECK((r1 == r2) == x);
}

template <class It1, class It2> void test17(It1 l, It2 r, std::ptrdiff_t x) {
	 const __stl2::reverse_iterator<It1> r1(l);
	 const __stl2::reverse_iterator<It2> r2(r);
	 CHECK((r1 - r2) == x);
}

template <class It> void test18(It l, It r, bool x) {
	const __stl2::reverse_iterator<It> r1(l);
	const __stl2::reverse_iterator<It> r2(r);
	CHECK((r1 > r2) == x);
}

template <class It> void test19(It l, It r, bool x) {
	const __stl2::reverse_iterator<It> r1(l);
	const __stl2::reverse_iterator<It> r2(r);
	CHECK((r1 >= r2) == x);
}

template <class It>
void test20(It i, __stl2::difference_type_t<It> n, __stl2::value_type_t<It> x) {
	const __stl2::reverse_iterator<It> r(i);
	__stl2::value_type_t<It> rr = r[n];
	CHECK(rr == x);
}

template <class It> void test21(It l, It r, bool x) {
	const __stl2::reverse_iterator<It> r1(l);
	const __stl2::reverse_iterator<It> r2(r);
	CHECK((r1 < r2) == x);
}

template <class It>
void
test22(It l, It r, bool x) {
	const __stl2::reverse_iterator<It> r1(l);
	const __stl2::reverse_iterator<It> r2(r);
	CHECK((r1 < r2) == x);
}

template <class It>
void
test23(It l, It r, bool x) {
	const __stl2::reverse_iterator<It> r1(l);
	const __stl2::reverse_iterator<It> r2(r);
	CHECK((r1 <= r2) == x);
}

class B
{
	int data_ = 1;
public:
	B() = default;

	int get() const { return data_; }

	friend bool operator==(const B& x, const B& y) {
		return x.data_ == y.data_;
	}
};

template <class It>
void
test24(It i, __stl2::value_type_t<It> x)
{
	__stl2::reverse_iterator<It> r(i);
	CHECK((*r).get() == x.get());
}

class C
{
	int data_ = 1;
public:
	C(int d) : data_(d) {}
	C() = default;

	int get() const { return data_; }

	friend bool operator==(const C& x, const C& y) {
		return x.data_ == y.data_;
	}
	const C *operator&() const { return nullptr; }
	C       *operator&()       { return nullptr; }
};

template <class It>
void
test25(It i, __stl2::difference_type_t<It> n, It x)
{
	const __stl2::reverse_iterator<It> r(i);
	__stl2::reverse_iterator<It> rr = n + r;
	CHECK(rr.base() == x);
}


constexpr bool test_constexpr() {
	int rng[] = {0,1,2,3};
	auto const first = __stl2::make_reverse_iterator(__stl2::end(rng));
	auto const last = __stl2::make_reverse_iterator(__stl2::begin(rng));
	if (!(first == first)) return false;
	if (first != first) return false;
	if (last < first) return false;
	if (!(last > first)) return false;
	if (last <= first) return false;
	if (!(last >= first)) return false;
	if (*(first + 2) != 1) return false;
	if (*(last - 2) != 1) return false;
	if (first[3] != 0) return false;
	if (first - last != -4) return false;
	{
		__stl2::reverse_iterator<int const*> foo{first};
		foo = first;
		if (!(foo == first)) return false;
		if (foo != first) return false;
		if (last < foo) return false;
		if (!(last > foo)) return false;
		if (last <= foo) return false;
		if (!(last >= foo)) return false;
		if (*(foo + 2) != 1) return false;
		if (*(last - 2) != 1) return false;
		if (foo[3] != 0) return false;
		if (foo - last != -4) return false;
	}
	auto pos = first;
	++pos;
	if (pos.base() != rng + 3) return false;
	if (pos + 3 != last) return false;
	if (*pos != 2) return false;
	if (pos.operator->() != rng + 2) return false;
	--pos;
	if (pos != first) return false;
	pos += 2;
	if (*pos != 1) return false;
	pos -= -1;
	if (*pos != 0) return false;

	return true;
}
static_assert(test_constexpr());

int main() {
	{
		namespace models = __stl2::models;
		static_assert(
				models::BidirectionalIterator<
					__stl2::reverse_iterator<bidirectional_iterator<const char *>>>);
		static_assert(
				models::RandomAccessIterator<
					__stl2::reverse_iterator<random_access_iterator<const char *>>>);
	}
	{ // test
		test<bidirectional_iterator<const char *>>();
		test<random_access_iterator<char *>>();
		test<char *>();
		test<const char *>();
	}
	{ // test 2
		const char s[] = "123";
		test2(bidirectional_iterator<const char *>(s));
		test2(random_access_iterator<const char *>(s));
	}
	{ // test3
		Derived d;
		test3<bidirectional_iterator<Base *>>(
				bidirectional_iterator<Derived *>(&d));
		test3<random_access_iterator<const Base *>>(
				random_access_iterator<Derived *>(&d));
	}
	{ // test4
		const char *s = "1234567890";
		random_access_iterator<const char *> b(s);
		random_access_iterator<const char *> e(s + 10);
		while (b != e)
			test4(b++);
	}
	{ // test5
		const char *s = "1234567890";
		test5(bidirectional_iterator<const char *>(s),
					bidirectional_iterator<const char *>(s), false);
		test5(bidirectional_iterator<const char *>(s),
					bidirectional_iterator<const char *>(s + 1), true);
		test5(random_access_iterator<const char *>(s),
					random_access_iterator<const char *>(s), false);
		test5(random_access_iterator<const char *>(s),
					random_access_iterator<const char *>(s + 1), true);
		test5(s, s, false);
		test5(s, s + 1, true);
	}
	{
		const char *s = "123";
		test6(bidirectional_iterator<const char *>(s + 1),
					bidirectional_iterator<const char *>(s));
		test6(random_access_iterator<const char *>(s + 1),
					random_access_iterator<const char *>(s));
		test6(s + 1, s);
	}
	{
		const char *s = "123";
		test7(bidirectional_iterator<const char *>(s + 1),
					bidirectional_iterator<const char *>(s));
		test7(random_access_iterator<const char *>(s + 1),
					random_access_iterator<const char *>(s));
		test7(s + 1, s);
	}
	{
		const char *s = "1234567890";
		test8(random_access_iterator<const char *>(s + 5), 5,
					random_access_iterator<const char *>(s));
		test8(s + 5, 5, s);
	}
	{
		const char *s = "1234567890";
		test9(random_access_iterator<const char *>(s + 5), 5,
					random_access_iterator<const char *>(s));
		test9(s + 5, 5, s);
	}
	{
		const char *s = "123";
		test10(bidirectional_iterator<const char *>(s + 1),
					 bidirectional_iterator<const char *>(s + 2));
		test10(random_access_iterator<const char *>(s + 1),
					 random_access_iterator<const char *>(s + 2));
		test10(s + 1, s + 2);
	}
	{
		const char *s = "123";
		test11(bidirectional_iterator<const char *>(s + 1),
					 bidirectional_iterator<const char *>(s + 2));
		test11(random_access_iterator<const char *>(s + 1),
					 random_access_iterator<const char *>(s + 2));
		test11(s + 1, s + 2);
	}
	{
		const char *s = "1234567890";
		test12(random_access_iterator<const char *>(s + 5), 5,
					 random_access_iterator<const char *>(s + 10));
		test12(s + 5, 5, s + 10);
	}
	{
		const char *s = "1234567890";
		test13(random_access_iterator<const char *>(s + 5), 5,
					 random_access_iterator<const char *>(s + 10));
		test13(s + 5, 5, s + 10);
	}
	{
		A a;
		test14(&a + 1, A());
	}
	{
		Derived d;

		test15<bidirectional_iterator<Base *>>(
				bidirectional_iterator<Derived *>(&d));
		test15<random_access_iterator<const Base *>>(
				random_access_iterator<Derived *>(&d));
		test15<Base *>(&d);
	}
	{
		const char *s = "1234567890";
		test16(bidirectional_iterator<const char *>(s),
					 bidirectional_iterator<const char *>(s), true);
		test16(bidirectional_iterator<const char *>(s),
					 bidirectional_iterator<const char *>(s + 1), false);
		test16(random_access_iterator<const char *>(s),
					 random_access_iterator<const char *>(s), true);
		test16(random_access_iterator<const char *>(s),
					 random_access_iterator<const char *>(s + 1), false);
		test16(s, s, true);
		test16(s, s + 1, false);
	}
	{
		char s[3] = {0};
		test17(random_access_iterator<const char *>(s),
					 random_access_iterator<char *>(s), 0);
		random_access_iterator<char *> inp1(s);
		test17(random_access_iterator<char *>(s),
					 random_access_iterator<const char *>(s + 1), 1);
		test17(random_access_iterator<const char *>(s + 1),
					 random_access_iterator<char *>(s), -1);
		test17(s, s, 0);
		test17(s, s + 1, 1);
		test17(s + 1, s, -1);
	}
	{
		const char *s = "1234567890";
		test18(random_access_iterator<const char *>(s),
					 random_access_iterator<const char *>(s), false);
		test18(random_access_iterator<const char *>(s),
					 random_access_iterator<const char *>(s + 1), true);
		test18(random_access_iterator<const char *>(s + 1),
					 random_access_iterator<const char *>(s), false);
		test18(s, s, false);
		test18(s, s + 1, true);
		test18(s + 1, s, false);
	}
	{
		const char *s = "1234567890";
		test19(random_access_iterator<const char *>(s),
					 random_access_iterator<const char *>(s), true);
		test19(random_access_iterator<const char *>(s),
					 random_access_iterator<const char *>(s + 1), true);
		test19(random_access_iterator<const char *>(s + 1),
					 random_access_iterator<const char *>(s), false);
		test19(s, s, true);
		test19(s, s + 1, true);
		test19(s + 1, s, false);
	}
	{
		const char *s = "1234567890";
		test20(random_access_iterator<const char *>(s + 5), 4, '1');
		test20(s + 5, 4, '1');
	}
	{
		const char *s = "1234567890";
		test21(random_access_iterator<const char *>(s),
				 random_access_iterator<const char *>(s), false);
		test21(random_access_iterator<const char *>(s),
				 random_access_iterator<const char *>(s + 1), false);
		test21(random_access_iterator<const char *>(s + 1),
				 random_access_iterator<const char *>(s), true);
		test21(s, s, false);
		test21(s, s + 1, false);
		test21(s + 1, s, true);
	}
	{
		const char* s = "1234567890";
		test22(random_access_iterator<const char*>(s), random_access_iterator<const char*>(s), false);
		test22(random_access_iterator<const char*>(s), random_access_iterator<const char*>(s+1), false);
		test22(random_access_iterator<const char*>(s+1), random_access_iterator<const char*>(s), true);
		test22(s, s, false);
		test22(s, s+1, false);
		test22(s+1, s, true);
	}
	{
		const char* s = "1234567890";
		test23(random_access_iterator<const char*>(s), random_access_iterator<const char*>(s), true);
		test23(random_access_iterator<const char*>(s), random_access_iterator<const char*>(s+1), false);
		test23(random_access_iterator<const char*>(s+1), random_access_iterator<const char*>(s), true);
		test23(s, s, true);
		test23(s, s+1, false);
		test23(s+1, s, true);
	}
	{
		B a;
		test24(&a+1, B());
	}
	{
		C l[3] = {C(0), C(1), C(2)};

		auto ri = __stl2::rbegin(l);
		CHECK ( (*ri).get() == 2 );  ++ri;
		CHECK ( (*ri).get() == 1 );  ++ri;
		CHECK ( (*ri).get() == 0 );  ++ri;
		CHECK ( ri == __stl2::rend(l));
	}
	{
		const char* s = "1234567890";
		test25(random_access_iterator<const char*>(s+5), 5, random_access_iterator<const char*>(s));
		test25(s+5, 5, s);
	}

	{
		// Verify that reverse_iterator's constructor that accepts a base iterator
		// is explicit.
		using RI = __stl2::reverse_iterator<char*>;
		static_assert(__stl2::models::Constructible<RI, char*>);
		static_assert(!__stl2::models::ConvertibleTo<char*, RI>);
	}

	return test_result();
}
