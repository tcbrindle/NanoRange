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
#include <cstring>
#include <iostream>

#include <stl2/iterator.hpp>
#include <stl2/memory.hpp>
#include <stl2/detail/raw_ptr.hpp>

#include "../simple_test.hpp"

namespace models = ::__stl2::models;

template <class T>
struct reference_wrapper {
	__stl2::detail::raw_ptr<T> ptr_;

	reference_wrapper() = default;
	reference_wrapper(T& t) noexcept : ptr_{std::addressof(t)} {}
	reference_wrapper(T&&) = delete;

	T& get() const noexcept {
		return *ptr_;
	}

	reference_wrapper & operator=(const T& t)
		noexcept(std::is_nothrow_copy_assignable<T>::value) {
		get() = t;
		return *this;
	}
	reference_wrapper & operator=(T&& t)
		noexcept(std::is_nothrow_move_assignable<T>::value) {
		get() = __stl2::move(t);
		return *this;
	}

	reference_wrapper const& operator=(const T& t) const
		noexcept(std::is_nothrow_copy_assignable<T>::value) {
		get() = t;
		return *this;
	}
	reference_wrapper const& operator=(T&& t) const
		noexcept(std::is_nothrow_move_assignable<T>::value) {
		get() = __stl2::move(t);
		return *this;
	}

	operator T&() const noexcept { return get(); }
};

template <class T, std::size_t N>
struct array {
	T e_[N];

	using reference = reference_wrapper<T>;
	using const_reference = reference_wrapper<const T>;

	reference operator[](const std::size_t n) noexcept {
		return {e_[n]};
	}
	const_reference operator[](const std::size_t n) const noexcept {
		return {e_[n]};
	}

	struct iterator {
		using iterator_category = __stl2::random_access_iterator_tag;
		using difference_type = std::ptrdiff_t;
		using value_type = T;

		T* ptr_;

		iterator() = default;
		iterator(T* p) noexcept : ptr_{p} {}

		reference operator*() const noexcept {
			STL2_EXPECT(ptr_);
			return {*ptr_};
		}

		T* operator->() const noexcept
		requires std::is_class<T>::value || std::is_union<T>::value
		{
			STL2_EXPECT(ptr_);
			return ptr_;
		}

		iterator& operator++() noexcept {
			++ptr_;
			return *this;
		}

		iterator operator++(int) noexcept {
			auto tmp = *this;
			++*this;
			return tmp;
		}

		bool operator==(const iterator& other) const noexcept {
			return ptr_ == other.ptr_;
		}

		bool operator!=(const iterator& other) const noexcept {
			return !(*this == other);
		}

		iterator operator+(std::ptrdiff_t n) const noexcept {
			return {ptr_ + n};
		}

		friend T&& iter_move(iterator i) noexcept {
			//std::cout << "iter_move(" << static_cast<void*>(i.ptr_) << ")\n";
			STL2_EXPECT(i.ptr_);
			return static_cast<T&&>(*i.ptr_);
		}
	};

	iterator begin() { return {&e_[0]}; }
	iterator end() { return {&e_[0] + N}; }
};

enum class category {
	none, output, input, forward, bidirectional, random_access, contiguous
};

std::ostream& operator<<(std::ostream& sout, category c) {
	switch(c) {
	case category::output:
		return sout << "category::output";
	case category::input:
		return sout << "category::input";
	case category::forward:
		return sout << "category::forward";
	case category::bidirectional:
		return sout << "category::bidirectional";
	case category::random_access:
		return sout << "category::random_access";
	case category::contiguous:
		return sout << "category::contiguous";
	default:
		return sout << "category::none";
	}
}

template <class>
constexpr category iterator_dispatch() { return category::none; }
template <__stl2::OutputIterator<const int&> I>
requires !__stl2::InputIterator<I>
constexpr category iterator_dispatch() { return category::output; }
template <__stl2::InputIterator>
constexpr category iterator_dispatch() { return category::input; }
template <__stl2::ForwardIterator>
constexpr category iterator_dispatch() { return category::forward; }
template <__stl2::BidirectionalIterator>
constexpr category iterator_dispatch() { return category::bidirectional; }
template <__stl2::RandomAccessIterator>
constexpr category iterator_dispatch() { return category::random_access; }
template <__stl2::ext::ContiguousIterator>
constexpr category iterator_dispatch() { return category::contiguous; }

template <class C, bool EC, class R = int&>
struct arbitrary_iterator {
	using difference_type = std::ptrdiff_t;

	arbitrary_iterator& operator*();
	arbitrary_iterator& operator=(std::remove_reference_t<R>);

	arbitrary_iterator& operator++();
	arbitrary_iterator& operator++(int);

	bool operator==(arbitrary_iterator) const
	requires EC;
	bool operator!=(arbitrary_iterator) const
	requires EC;
};

template <__stl2::DerivedFrom<__stl2::input_iterator_tag> C, bool EC, class R>
struct arbitrary_iterator<C, EC, R> {
	using iterator_category = C;
	using value_type = std::remove_reference_t<R>;
	using difference_type = std::ptrdiff_t;

	R operator*() const;

	arbitrary_iterator& operator++();
	arbitrary_iterator operator++(int);

	bool operator==(arbitrary_iterator) const
	requires EC || __stl2::DerivedFrom<C, __stl2::forward_iterator_tag>;
	bool operator!=(arbitrary_iterator) const
	requires EC || __stl2::DerivedFrom<C, __stl2::forward_iterator_tag>;

	arbitrary_iterator& operator--()
	requires __stl2::DerivedFrom<C, __stl2::bidirectional_iterator_tag>;
	arbitrary_iterator operator--(int)
	requires __stl2::DerivedFrom<C, __stl2::bidirectional_iterator_tag>;

	bool operator<(arbitrary_iterator) const
	requires __stl2::DerivedFrom<C, __stl2::random_access_iterator_tag>;
	bool operator>(arbitrary_iterator) const
	requires __stl2::DerivedFrom<C, __stl2::random_access_iterator_tag>;
	bool operator<=(arbitrary_iterator) const
	requires __stl2::DerivedFrom<C, __stl2::random_access_iterator_tag>;
	bool operator>=(arbitrary_iterator) const
	requires __stl2::DerivedFrom<C, __stl2::random_access_iterator_tag>;

	arbitrary_iterator& operator+=(difference_type)
	requires __stl2::DerivedFrom<C, __stl2::random_access_iterator_tag>;
	arbitrary_iterator& operator-=(difference_type)
	requires __stl2::DerivedFrom<C, __stl2::random_access_iterator_tag>;

	arbitrary_iterator operator-(difference_type) const
	requires __stl2::DerivedFrom<C, __stl2::random_access_iterator_tag>;
	difference_type operator-(arbitrary_iterator) const
	requires __stl2::DerivedFrom<C, __stl2::random_access_iterator_tag>;

	value_type& operator[](difference_type) const
	requires __stl2::DerivedFrom<C, __stl2::random_access_iterator_tag>;
};

template <__stl2::DerivedFrom<__stl2::random_access_iterator_tag> C, bool B, class R>
arbitrary_iterator<C, B, R> operator+(
	arbitrary_iterator<C, B, R>, typename arbitrary_iterator<C, B, R>::difference_type);

template <__stl2::DerivedFrom<__stl2::random_access_iterator_tag> C, bool B, class R>
arbitrary_iterator<C, B, R> operator+(
	typename arbitrary_iterator<C, B, R>::difference_type, arbitrary_iterator<C, B, R>);

void test_iterator_dispatch() {
	CHECK(iterator_dispatch<void>() == category::none);
	static_assert(models::ContiguousIterator<int*>);
	CHECK(iterator_dispatch<int*>() == category::contiguous);

	{
		static_assert(models::Readable<int* const>);
	}

	{
		using I = arbitrary_iterator<__stl2::input_iterator_tag, false>;
		static_assert(models::InputIterator<I>);
		CHECK(iterator_dispatch<I>() == category::input);
	}
	{
		using I = arbitrary_iterator<__stl2::input_iterator_tag, true>;
		static_assert(models::InputIterator<I>);
		static_assert(models::EqualityComparable<I>);
		CHECK(iterator_dispatch<I>() == category::input);
	}
	{
		using I = arbitrary_iterator<__stl2::forward_iterator_tag, true>;
		static_assert(models::ForwardIterator<I>);
		CHECK(iterator_dispatch<I>() == category::forward);
	}
	{
		using I = arbitrary_iterator<__stl2::bidirectional_iterator_tag, true>;
		static_assert(models::BidirectionalIterator<I>);
		CHECK(iterator_dispatch<I>() == category::bidirectional);
	}
	{
		using I = arbitrary_iterator<__stl2::random_access_iterator_tag, true>;
		static_assert(models::RandomAccessIterator<I>);
		CHECK(iterator_dispatch<I>() == category::random_access);
	}
	{
		using I = arbitrary_iterator<__stl2::ext::contiguous_iterator_tag, true>;
		static_assert(models::ContiguousIterator<I>);
		CHECK(iterator_dispatch<I>() == category::contiguous);
	}

	{
		using I = arbitrary_iterator<void, false>;
		static_assert(models::OutputIterator<I, const int&>);
		static_assert(!models::InputIterator<I>);
		CHECK(iterator_dispatch<I>() == category::output);
	}
	{
		using I = arbitrary_iterator<void, true>;
		static_assert(models::OutputIterator<I, const int&>);
		static_assert(models::EqualityComparable<I>);
		static_assert(!models::InputIterator<I>);
		CHECK(iterator_dispatch<I>() == category::output);
	}
}

template <__stl2::InputIterator I, __stl2::Sentinel<I> S, class O>
requires __stl2::IndirectlyCopyable<I, O>
bool copy(I first, S last, O o) {
	for (; first != last; ++first, ++o) {
		*o = *first;
	}
	return false;
}

template <__stl2::ext::ContiguousIterator I, __stl2::SizedSentinel<I> S,
	__stl2::ext::ContiguousIterator O>
requires
	__stl2::IndirectlyCopyable<I, O> &&
	__stl2::Same<__stl2::value_type_t<I>, __stl2::value_type_t<O>> &&
	std::is_trivially_copyable<__stl2::value_type_t<I>>::value
bool copy(I first, S last, O o) {
	auto n = last - first;
	STL2_EXPECT(n >= 0);
	if (n) {
		std::memmove(std::addressof(*o), std::addressof(*first),
			n * sizeof(__stl2::value_type_t<I>));
	}
	return true;
}

void test_copy() {
	{
		struct A {
			int value;
			A(int i) : value{i} {}
			A(const A& that) :
				value{that.value} {}
		};
		A a[] = {0,1,2,3}, b[] = {4,5,6,7};
		CHECK(!copy(__stl2::begin(a) + 1, __stl2::end(a) - 1, __stl2::begin(b) + 1));
		CHECK(b[0].value == 4);
		CHECK(b[1].value == 1);
		CHECK(b[2].value == 2);
		CHECK(b[3].value == 7);
	}

	{
		int a[] = {0,1,2,3,4,5,6,7}, b[] = {7,6,5,4,3,2,1,0};
		CHECK(copy(__stl2::begin(a) + 2, __stl2::end(a) - 2, __stl2::begin(b) + 2));
		CHECK(b[0] == 7);
		CHECK(b[1] == 6);
		CHECK(b[2] == 2);
		CHECK(b[3] == 3);
		CHECK(b[4] == 4);
		CHECK(b[5] == 5);
		CHECK(b[6] == 1);
		CHECK(b[7] == 0);
	}
}

void test_iter_swap2() {
	{
		int a[] = { 42, 13 };
		__stl2::iter_swap(a + 0, a + 1);
		CHECK(a[0] == 13);
		CHECK(a[1] == 42);
		__stl2::iter_swap(a + 0, a + 1);
		CHECK(a[0] == 42);
		CHECK(a[1] == 13);
	}

	{
		auto a = std::make_unique<int>(42);
		auto b = std::make_unique<int>(13);
		using I = decltype(a);
		static_assert(models::Same<I, decltype(b)>);
		static_assert(models::Readable<I>);
		using R = __stl2::reference_t<I>;
		static_assert(models::Same<int&, R>);
		using RR = __stl2::rvalue_reference_t<I>;
		static_assert(models::Same<int&&, RR>);
		static_assert(models::SwappableWith<R, R>);

		// Swappable<R, R> is true, calls the first overload of
		// iter_swap (which delegates to swap(*a, *b)):
		__stl2::iter_swap(a, b);
		CHECK(*a == 13);
		CHECK(*b == 42);
		__stl2::iter_swap(a, b);
		CHECK(*a == 42);
		CHECK(*b == 13);
	}

	{
		auto a = array<int, 4>{0,1,2,3};
		using I = decltype(a.begin());
		static_assert(models::Readable<I>);
		using V = __stl2::value_type_t<I>;
		static_assert(models::Same<int, V>);
		using R = __stl2::reference_t<I>;
		static_assert(models::Same<reference_wrapper<int>, R>);
		using RR = __stl2::rvalue_reference_t<I>;
		static_assert(models::Same<int&&, RR>);

		static_assert(models::Same<I, decltype(a.begin() + 2)>);
		static_assert(models::CommonReference<const R&, const R&>);
		static_assert(!models::SwappableWith<R, R>);
		static_assert(models::IndirectlyMovableStorable<I, I>);

		// Swappable<R, R> is not satisfied, and
		// IndirectlyMovableStorable<I, I> is satisfied,
		// so this should resolve to the second overload of iter_swap.
		__stl2::iter_swap(a.begin() + 1, a.begin() + 3);
		CHECK(a[0] == 0);
		CHECK(a[1] == 3);
		CHECK(a[2] == 2);
		CHECK(a[3] == 1);
	}
}

template <class T>
constexpr bool has_category = false;
template <class T>
requires
	requires { typename T::iterator_category; }
constexpr bool has_category<T> = true;

void test_std_traits() {
	using WO = arbitrary_iterator<void, false>;
	static_assert(models::Same<std::iterator_traits<WO>::iterator_category,
		std::output_iterator_tag>);

	using O = arbitrary_iterator<void, true>;
	static_assert(models::Same<std::iterator_traits<O>::iterator_category,
		std::output_iterator_tag>);

	using WI = arbitrary_iterator<__stl2::input_iterator_tag, false>;
	static_assert(!has_category<std::iterator_traits<WI>>);

	using I = arbitrary_iterator<__stl2::input_iterator_tag, true>;
	static_assert(models::Same<std::iterator_traits<I>::iterator_category,
		std::input_iterator_tag>);

	using F = arbitrary_iterator<__stl2::forward_iterator_tag, true>;
	static_assert(models::Same<std::iterator_traits<F>::iterator_category,
		std::forward_iterator_tag>);

	using B = arbitrary_iterator<__stl2::bidirectional_iterator_tag, true>;
	static_assert(models::Same<std::iterator_traits<B>::iterator_category,
		std::bidirectional_iterator_tag>);

	using R = arbitrary_iterator<__stl2::random_access_iterator_tag, true>;
	static_assert(models::Same<std::iterator_traits<R>::iterator_category,
		std::random_access_iterator_tag>);

	using C = arbitrary_iterator<__stl2::ext::contiguous_iterator_tag, true>;
	static_assert(models::Same<std::iterator_traits<C>::iterator_category,
		std::random_access_iterator_tag>);

	using IV = arbitrary_iterator<__stl2::input_iterator_tag, true, int>;
	static_assert(models::Same<std::iterator_traits<IV>::iterator_category,
		std::input_iterator_tag>);

	using FV = arbitrary_iterator<__stl2::forward_iterator_tag, true, int>;
	static_assert(models::Same<std::iterator_traits<FV>::iterator_category,
		std::input_iterator_tag>);

	using BV = arbitrary_iterator<__stl2::bidirectional_iterator_tag, true, int>;
	static_assert(models::Same<std::iterator_traits<BV>::iterator_category,
		std::input_iterator_tag>);

	using RV = arbitrary_iterator<__stl2::random_access_iterator_tag, true, int>;
	static_assert(models::Same<std::iterator_traits<RV>::iterator_category,
		std::input_iterator_tag>);
}

struct MakeString {
	using value_type = std::string;
	std::string operator*() const;
};

static_assert(models::Readable<MakeString>);
static_assert(!models::Writable<MakeString, std::string>);
static_assert(!models::Writable<MakeString, const std::string &>);
static_assert(!models::IndirectlyMovable<MakeString, MakeString>);
static_assert(!models::IndirectlyCopyable<MakeString, MakeString>);
static_assert(!models::IndirectlySwappable<MakeString, MakeString>);

int main() {
	test_iter_swap2();
	test_iterator_dispatch();
	test_copy();
	test_std_traits();

	return ::test_result();
}
