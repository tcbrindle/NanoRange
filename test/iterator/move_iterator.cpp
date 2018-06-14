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
#include <numeric>
#include <memory>
#include <utility>
#include <vector>
#include <nanorange/iterator.hpp>
#include <nanorange/algorithm/copy.hpp>
#include <nanorange/algorithm/equal.hpp>
//#include <stl2/view/iota.hpp>
//#include <stl2/view/take.hpp>
//#include <stl2/view/repeat_n.hpp>
#include "../catch.hpp"

namespace ranges = nano::ranges;

namespace {

struct A {
	static std::size_t copy_count;
	static std::size_t move_count;
	int i{0};

	static void clear() { copy_count = move_count = 0; }

	A() = default;
	A(int i) : i(i) {}
	A(const A& that) : i(that.i) { ++copy_count; }
	A(A&& that) noexcept : i(that.i) { ++move_count; that.i = -1; }
	A& operator=(const A& that) & { ++copy_count; i = that.i; return *this; }
	A& operator=(A&& that) & noexcept {
		++move_count;
		i = that.i;
		that.i = -1;
		return *this;
	}
	friend bool operator==(A a, A b) {
		return a.i == b.i;
	}
	friend bool operator!=(A a, A b) {
		return !(a == b);
	}
};

std::size_t A::copy_count;
std::size_t A::move_count;

void test_move_iterator() {
	static constexpr std::size_t N = 42;
	std::vector<A> vec(N);
	std::vector<A> vec2;
	vec2.reserve(ranges::size(vec));

	A::clear();

	{
		auto first = ranges::make_move_iterator(ranges::begin(vec)),
			last = ranges::make_move_iterator(ranges::end(vec));
		static_assert(ranges::RandomAccessIterator<decltype(ranges::begin(vec))>, "");
		static_assert(ranges::InputIterator<decltype(first)>, "");
		static_assert(!ranges::ForwardIterator<decltype(first)>, "");
		auto out = ranges::back_inserter(vec2);

		for (; first != last; ++first, ++out) {
			*out = *first;
		}
	}

	CHECK(ranges::size(vec2) == N);
	CHECK(A::copy_count == std::size_t{0});
	CHECK(A::move_count == N);
}

void test_iter_move() {
	static constexpr std::size_t N = 42;
	std::vector<A> vec(N);
	std::vector<A> vec2;
	vec2.reserve(ranges::size(vec));

	A::clear();

	{
		auto first = ranges::begin(vec),
			last = ranges::end(vec);
		auto out = ranges::back_inserter(vec2);

		for (; first != last; ++first, ++out) {
			*out = ranges::iter_move(first);
		}
	}

	CHECK(ranges::size(vec2) == N);
	CHECK(A::copy_count == std::size_t{0});
	CHECK(A::move_count == N);
}

void test_both() {
	static constexpr std::size_t N = 42;
	std::vector<A> vec(N);
	std::vector<A> vec2;
	vec2.reserve(ranges::size(vec));

	A::clear();

	{
		auto first = ranges::make_move_iterator(ranges::begin(vec)),
			last = ranges::make_move_iterator(ranges::end(vec));
		auto out = ranges::back_inserter(vec2);

		for (; first != last; ++first, ++out) {
			*out = ranges::iter_move(first);
		}
	}

	CHECK(ranges::size(vec2) == N);
	CHECK(A::copy_count == std::size_t{0});
	CHECK(A::move_count == N);
}

template <class T>
class proxy_iterator {
public:
	using value_type = T;
	using difference_type = std::ptrdiff_t;
	using iterator_category = ranges::input_iterator_tag;

	proxy_iterator() = default;
	explicit proxy_iterator(T* p) :
		ptr_{std::make_shared<T*>(p)} {}

	struct readable_proxy {
		using value_type = T;
		mutable T cache_;
		T& operator*() const noexcept {
			return cache_;
		}
		friend T&& iter_move(const readable_proxy& p) noexcept {
			return std::move(p.cache_);
		}
	};

	std::reference_wrapper<T> operator*() const {
		return std::reference_wrapper<T>{**ptr_};
	}

	bool operator==(const proxy_iterator& that) const {
		return *ptr_ == *that.ptr_;
	}
	bool operator!=(const proxy_iterator& that) const {
		return !(*this == that);
	}

	proxy_iterator& operator++() & {
		// Input iterator is destructive!
		(*ptr_)->i = -1;
		++*ptr_;
		return *this;
	}
	readable_proxy operator++(int) & {
		readable_proxy tmp{ranges::iter_move(*ptr_)};
		++*this;
		return tmp;
	}

	friend T&& iter_move(const proxy_iterator& p) {
		return ranges::iter_move(*p.ptr_);
	}

private:
	std::shared_ptr<T*> ptr_;
};

template <typename T>
auto print_type() = delete;

void test_proxy_iterator() {
	static constexpr std::size_t N = 42;
	std::vector<A> vec(N);
	std::vector<A> vec2;
	vec2.reserve(ranges::size(vec));

	using P = proxy_iterator<A>;
	print_type<ranges::rvalue_reference_t<P>&&>();
	print_type<ranges::reference_t<P>&&>();

	//using C = ranges::common_reference_t<ranges::reference_t<P>&&, ranges::rvalue_reference_t<P>&&>>;
	static_assert(ranges::CommonReference<ranges::reference_t<P>&&, ranges::rvalue_reference_t<P>&&>, "");
	static_assert(ranges::CommonReference<ranges::rvalue_reference_t<P>&&, const ranges::value_type_t<P>&>, "");
	static_assert(ranges::Readable<proxy_iterator<A>>, "");

	static_assert(
		ranges::Same<
			ranges::reference_t<proxy_iterator<A>>,
			std::reference_wrapper<A>>, "");
	static_assert(
		ranges::Same<
			ranges::reference_t<const proxy_iterator<A>>,
			std::reference_wrapper<A>>, "");
	static_assert(
		ranges::Same<
			ranges::rvalue_reference_t<proxy_iterator<A>>,
			A&&>, "");

	{
		static_assert(
			ranges::Same<
				ranges::rvalue_reference_t<
					ranges::move_iterator<proxy_iterator<A>>>,
				A&&>, "");
		auto first = ranges::make_move_iterator(proxy_iterator<A>{ranges::data(vec)}),
			last = ranges::make_move_iterator(proxy_iterator<A>{ranges::data(vec) + ranges::size(vec)});
		auto out = ranges::back_inserter(vec2);

		std::iota(vec.begin(), vec.end(), 0);
		vec2.clear();
		A::clear();
		std::copy(first, last, out);

		CHECK(ranges::size(vec2) == N);
		CHECK(A::copy_count == std::size_t{0});
		CHECK(A::move_count == N);
#ifdef HAVE_VIEWS
		CHECK(ranges::equal(vec2, ranges::view::iota(0) | ranges::view::take(N), std::equal_to<>{}));
		CHECK(ranges::equal(vec, ranges::ext::repeat_n_view<int>{-1, N}, std::equal_to<>{}));
#endif

		first = ranges::make_move_iterator(proxy_iterator<A>{ranges::data(vec)});
		std::iota(vec.begin(), vec.end(), 0);
		vec2.clear();
		A::clear();
		while (first != last) // Test post-increment
			*out++ = *first++;

		CHECK(ranges::size(vec2) == N);
		CHECK(A::copy_count == std::size_t{0});
		CHECK(A::move_count == 2*N);
#ifdef HAVE_VIEWS
		CHECK(ranges::equal(vec2, ranges::view::iota(0) | ranges::view::take(N), std::equal_to<>{}));
		CHECK(ranges::equal(vec, ranges::ext::repeat_n_view<int>{-1, N}, std::equal_to<>{}));
#endif
	}

	{
		static_assert(
			ranges::Same<
				ranges::rvalue_reference_t<
					ranges::counted_iterator<proxy_iterator<A>>>,
				A&&>, "");
		static_assert(
			ranges::Same<
				ranges::rvalue_reference_t<
					ranges::move_iterator<
						ranges::counted_iterator<proxy_iterator<A>>>>,
				A&&>, "");
		auto first = ranges::make_move_iterator(
			ranges::make_counted_iterator(
				proxy_iterator<A>{ranges::data(vec)}, ranges::size(vec)));
		auto out = ranges::back_inserter(vec2);

		std::iota(vec.begin(), vec.end(), 0);
		vec2.clear();
		A::clear();
		ranges::copy(first, ranges::move_sentinel<ranges::default_sentinel>{}, out);

		CHECK(ranges::size(vec2) == N);
		CHECK(A::copy_count == std::size_t{0});
		CHECK(A::move_count == N);
	}

	{
		auto first = ranges::make_counted_iterator(vec.begin(), vec.size());
		auto last = ranges::default_sentinel{};
		static_assert(ranges::SizedSentinel<decltype(last), decltype(first)>, "");
		CHECK((static_cast<std::size_t>(last - first) == vec.size()));
		auto mfirst = ranges::make_move_iterator(first);
		auto mlast = ranges::make_move_sentinel(last);
		static_assert(ranges::SizedSentinel<decltype(mlast), decltype(mfirst)>, "");
		CHECK((static_cast<std::size_t>(mlast - mfirst) == vec.size()));
	}
}

constexpr bool test_constexpr() {
	struct Int {
		int i_;
		constexpr Int(int i) noexcept
		: i_{i}
		{}
		constexpr Int(const Int&) noexcept = default;
		constexpr Int& operator=(const Int&) & noexcept = default;
		constexpr Int(Int&& that) noexcept
		: i_{that.i_}
		{ that.i_ = -1; }
		constexpr Int& operator=(Int&& that) & noexcept {
			i_ = that.i_;
			that.i_ = -1;
			return *this;
		}
		constexpr operator int() const { return i_; }
	};
	Int some_ints[] = {0,1,2,3};

	{ constexpr ranges::move_iterator<int*> unused{}; (void)unused; }

	auto const first = ranges::make_move_iterator(ranges::begin(some_ints));
	if (first.base() != ranges::begin(some_ints)) return false;
	auto const last = ranges::make_move_iterator(ranges::end(some_ints));
	if (last.base() != ranges::end(some_ints)) return false;

	if (first == last) return false;
	if (!(first != last)) return false;
	if (!(first < last)) return false;
	if (first > last) return false;
	if (!(first <= last)) return false;
	if (first >= last) return false;
	if (last - first != 4) return false;
	if (first - last != -4) return false;
	if (Int{*(first + 2)} != 2) return false;
	if (*(first + 2) != -1) return false;

	{
		ranges::move_iterator<Int const*> tmp{first};
		tmp = first;
		if (tmp == last) return false;
		if (!(tmp != last)) return false;
		if (!(tmp < last)) return false;
		if (tmp > last) return false;
		if (!(tmp <= last)) return false;
		if (tmp >= last) return false;
		if (last - tmp != 4) return false;
		if (tmp - last != -4) return false;
		if (Int{*(tmp + 3)} != 3) return false;
		if (*(tmp + 3) != 3) return false;
	}

	auto end = ranges::make_move_sentinel(ranges::end(some_ints));

	if (first == end) return false;
	if (end == first) return false;
	if (last != end) return false;
	if (end != last) return false;
	if (end - first != 4) return false;
	if (first - end != -4) return false;
	{
		ranges::move_sentinel<Int const*> tmp{end};
		tmp = end;
		if (first == tmp) return false;
		if (tmp == first) return false;
		if (last != tmp) return false;
		if (tmp != last) return false;
		if (tmp - first != 4) return false;
		if (first - tmp != -4) return false;
	}

	auto pos = first;
	++pos;
	if (pos != first + 1) return false;
	--pos;
	if (pos != last - 4) return false;
	pos += 4;
	if (pos != last) return false;
	pos -= 4;
	if (pos != first) return false;
	if (pos + 2 != 2 + pos) return false;

	if (Int{first[3]} != 3) return false;
	if (first[3] != -1) return false;

	return true;
}
static_assert(test_constexpr(), "");

}

TEST_CASE("iter.move_iterator") {
	test_move_iterator();
	test_iter_move();
	test_both();
	test_proxy_iterator();
}
