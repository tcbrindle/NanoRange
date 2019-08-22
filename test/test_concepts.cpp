
#include <nanorange/iterator.hpp>
#include <nanorange/ranges.hpp>
//#include <nanorange-old.hpp>

#include <bitset>
#include <functional>
#include <memory>
#include <string>
#include <thread>
#include <vector>
#include <array>

namespace rng = nano::ranges;

struct incomplete;

struct base {};
struct derived : base {};
class private_derived : base {};

struct from_int_only {
    from_int_only(int);

    template <typename T>
    from_int_only(T) = delete;
};

struct to_int {
    operator int() const;
};

// same_as concept tests

static_assert(rng::same_as<int, int>, "");
static_assert(!rng::same_as<float, double>, "");
static_assert(rng::same_as<void, void>, "");
static_assert(rng::same_as<incomplete, incomplete>, "");
static_assert(!rng::same_as<int, void>, "");


// derived_from tests
static_assert(!rng::derived_from<int, int>, "");
static_assert(!rng::derived_from<int&, int&>, "");
static_assert(!rng::derived_from<void, incomplete>, "");
static_assert(!rng::derived_from<int, float>, "");
static_assert(rng::derived_from<derived, base>, "");
static_assert(!rng::derived_from<base, derived>, "");
static_assert(!rng::derived_from<private_derived, base>, "");

// ConvertibleTo tests
static_assert(rng::convertible_to<void, void>, "");
static_assert(!rng::convertible_to<int, void>, "");
static_assert(!rng::convertible_to<void, int>, "");
static_assert(rng::convertible_to<int, int>, "");
static_assert(rng::convertible_to<int, const int>, "");
static_assert(rng::convertible_to<const int, int>, "");
static_assert(rng::convertible_to<int&, const volatile int>, "");
static_assert(rng::convertible_to<int&, int const&>, "");
static_assert(!rng::convertible_to<const int&, int&>, "");
static_assert(rng::convertible_to<int&&, int const&>, "");
static_assert(!rng::convertible_to<int&, int&&>, "");
// Hmmm, is this correct?
static_assert(!rng::convertible_to<int[], int[]>, "");
static_assert(rng::convertible_to<int, bool>, "");
static_assert(rng::convertible_to<float, int>, "");
static_assert(rng::convertible_to<derived&, base&>, "");
static_assert(!rng::convertible_to<base&, derived&>, "");
static_assert(!rng::convertible_to<private_derived&, base&>, "");

static_assert(rng::convertible_to<int&, from_int_only>, "");
static_assert(!rng::convertible_to<long, from_int_only>, "");

static_assert(rng::convertible_to<to_int, int>, "");
static_assert(rng::convertible_to<to_int, long>, "");

static_assert(rng::convertible_to<int*, void*>, "");
static_assert(!rng::convertible_to<void*, int*>, "");

static_assert(rng::convertible_to<const char*, std::string>, "");
static_assert(!rng::convertible_to<std::string, const char*>, "");

// CommonReference tests
static_assert(rng::common_reference_with<int&, int&>, "");
static_assert(!rng::common_reference_with<void, int>, "");
using void_cr = rng::common_reference_t<void, void>;
static_assert(rng::same_as<void_cr, void>, "");
static_assert(rng::convertible_to<void, void>, "");
static_assert(rng::common_reference_with<void, void>, "");


// Common tests
static_assert(rng::common_with<int, int>, "");

// Integal tests
static_assert(rng::integral<char>, "");
static_assert(rng::integral<signed char>, "");
static_assert(rng::integral<unsigned char>, "");
static_assert(rng::integral<bool>, "");
static_assert(!rng::integral<float>, "");
static_assert(!rng::integral<int&>, "");
static_assert(rng::integral<const int>, "");
static_assert(!rng::integral<float>, "");
static_assert(!rng::integral<std::string>, "");
static_assert(!rng::integral<void>, "");

// SignedIntegral tests
static_assert(rng::signed_integral<signed char>, "");
static_assert(!rng::signed_integral<unsigned char>, "");
static_assert(!rng::signed_integral<bool>, "");
static_assert(!rng::signed_integral<std::string>, "");

// UnsignedIntegral tests
static_assert(rng::unsigned_integral<unsigned char>, "");
static_assert(!rng::unsigned_integral<signed char>, "");
static_assert(!rng::unsigned_integral<signed>, "");
static_assert(!rng::unsigned_integral<void>, "");
static_assert(!rng::unsigned_integral<std::string>, "");

// Assignable tests
struct weird_assign {
    int operator=(const weird_assign&);
};

static_assert(rng::assignable_from<int&, int&>, "");
static_assert(rng::assignable_from<int&, int>, "");
static_assert(rng::assignable_from<int&, int&&>, "");
static_assert(!rng::assignable_from<int, int&>, "");
static_assert(!rng::assignable_from<int const&, int&>, "");
static_assert(rng::assignable_from<std::string&, const char*>, "");
static_assert(!rng::assignable_from<weird_assign&, weird_assign&>, "");
static_assert(!rng::assignable_from<void, int>, "");

// Swappable tests
static_assert(rng::swappable<int>, "");
static_assert(!rng::swappable<void>, "");
static_assert(rng::swappable<std::string>, "");
static_assert(rng::swappable<base>, "");

static_assert(!rng::swappable_with<int, long>, "");
static_assert(!rng::swappable_with<int, const int>, "");
static_assert(!rng::swappable_with<int[], int[]>, "");
static_assert(!rng::swappable_with<int*, void*>, "");
static_assert(!rng::swappable_with<base, derived>, "");

// Destructible tests
struct throwing_dtor {
    ~throwing_dtor() noexcept(false) {}
};

class private_dtor {
    ~private_dtor() = default;
};

static_assert(rng::destructible<int>, "");
static_assert(rng::destructible<std::string>, "");
static_assert(!rng::destructible<void>, "");
static_assert(!rng::destructible<throwing_dtor>, "");
static_assert(!rng::destructible<private_dtor>, "");

// Constructible tests
static_assert(rng::constructible_from<int, long&>, "");
static_assert(rng::constructible_from<base&, derived&>, "");
static_assert(rng::constructible_from<std::string, const char(&)[6]>, "");
static_assert(rng::constructible_from<std::string, char, int, std::allocator<char>>, "");
static_assert(!rng::constructible_from<throwing_dtor>, "");

// DefaultConstructible tests
struct agg {
    int i; float f;
};

static_assert(!rng::default_constructible<void>, "");
static_assert(rng::default_constructible<int>, "");
static_assert(rng::default_constructible<agg>, "");
static_assert(rng::default_constructible<std::string>, "");
static_assert(!rng::default_constructible<from_int_only>, "");

// MoveConstructible tests
struct no_copy_or_move {
    no_copy_or_move(const no_copy_or_move&) = delete;
    no_copy_or_move& operator=(const no_copy_or_move&) = delete;
};

static_assert(!rng::move_constructible<void>, "");
static_assert(rng::move_constructible<int>, "");
static_assert(rng::move_constructible<std::string>, "");
static_assert(rng::move_constructible<std::unique_ptr<int>>, "");
static_assert(!rng::move_constructible<no_copy_or_move>, "");

// CopyConstructible tests
struct odd_copy_ctor {
    odd_copy_ctor(odd_copy_ctor&);
};

static_assert(!rng::copy_constructible<void>, "");
static_assert(rng::copy_constructible<int>, "");
static_assert(rng::copy_constructible<std::string>, "");
static_assert(!rng::copy_constructible<std::unique_ptr<int>>, "");
static_assert(!rng::copy_constructible<no_copy_or_move>, "");
static_assert(!rng::copy_constructible<odd_copy_ctor>, "");


// Boolean tests
struct explicitly_convertible_to_bool {
    explicit operator bool();
};

static_assert(!rng::boolean<void>, "");
static_assert(rng::boolean<bool>, "");
static_assert(rng::boolean<int>, "");
static_assert(rng::boolean<std::true_type>, "");
static_assert(rng::boolean<std::bitset<1>::reference>, "");
static_assert(rng::boolean<std::vector<bool>::reference>, "");
static_assert(!rng::boolean<int*>, "");
static_assert(!rng::boolean<std::unique_ptr<int>>, "");
static_assert(!rng::boolean<explicitly_convertible_to_bool>, "");

// EqualityComparable tests
static_assert(rng::equality_comparable<int>, "");
static_assert(rng::equality_comparable<int&>, "");
static_assert(!rng::equality_comparable<void>, "");
static_assert(!rng::equality_comparable<std::thread>, "");

// equality_comparable_with tests
static_assert(rng::equality_comparable_with<double, double>, "");
static_assert(rng::equality_comparable_with<std::string, const char*>, "");
static_assert(!rng::equality_comparable_with<int, void>, "");
static_assert(!rng::equality_comparable_with<int, std::string>, "");

// totally_ordered tests
static_assert(!rng::totally_ordered<void>, "");
static_assert(rng::totally_ordered<int>, "");
static_assert(rng::totally_ordered<float>, "");
static_assert(rng::totally_ordered<std::string>, "");
static_assert(!rng::totally_ordered<std::thread>, "");

// StrictTotallyOrderedWith tests
static_assert(!rng::totally_ordered_with<void, void>, "");
static_assert(rng::totally_ordered_with<int, int>, "");
//static_assert(rng::StrictTotallyOrderedWith<int, float>, "");
static_assert(rng::totally_ordered_with<std::string, const char*>, "");
static_assert(rng::totally_ordered_with<int, double>, "");

// Copyable tests
struct odd_assign {
    odd_assign(const odd_assign&) = default;
    odd_assign& operator=(odd_assign&);
};

static_assert(!rng::copyable<void>, "");
static_assert(rng::copyable<int>, "");
static_assert(!rng::copyable<int&>, "");
static_assert(rng::copyable<std::string>, "");
static_assert(!rng::copyable<std::unique_ptr<int>>, "");
static_assert(!rng::copyable<odd_assign>, "");

// Semiregular tests
static_assert(!rng::semiregular<void>, "");
static_assert(rng::semiregular<int>, "");
static_assert(rng::semiregular<std::string>, "");
static_assert(rng::semiregular<int*>, "");
static_assert(!rng::semiregular<int&>, "");

// regular tests
static_assert(!rng::regular<void>, "");
static_assert(rng::regular<int>, "");
static_assert(rng::regular<std::string>, "");
static_assert(rng::regular<int*>, "");
static_assert(!rng::regular<int&>, "");

// [regular]Invocable tests
// FIXME: Add these
static_assert(!rng::invocable<void>, "");

// Predicate tests
int int_cmp(int, int);

static_assert(!rng::predicate<void>, "");
static_assert(rng::predicate<decltype(int_cmp), int, int>, "");
static_assert(rng::predicate<std::equal_to<>, int, int>, "");
const auto cmp = [] (auto const& lhs, auto const& rhs) { return lhs < rhs; };
static_assert(rng::predicate<decltype(cmp), int, float>, "");

// Relation tests
static_assert(!rng::relation<void, void, void>, "");
static_assert(rng::relation<std::equal_to<>, int, int>, "");

// Readable tests
static_assert(!rng::readable<void>, "");
static_assert(!rng::readable<int>, "");
static_assert(rng::readable<int*>, "");
static_assert(rng::readable<std::unique_ptr<int>>, "");
static_assert(rng::readable<std::vector<int>::const_iterator>, "");

struct MoveOnlyReadable {
    using value_type = std::unique_ptr<int>;
    value_type operator*() const;
};

static_assert(rng::readable<MoveOnlyReadable>, "");

// Writable tests
static_assert(!rng::writable<void, void>, "");
static_assert(rng::writable<int*, int>, "");
static_assert(!rng::writable<int const*, int>, "");
static_assert(rng::writable<std::unique_ptr<int>, int>, "");
static_assert(rng::writable<std::vector<int>::iterator, int>, "");
static_assert(!rng::writable<std::vector<int>::const_iterator, int>, "");

// weakly_incrementable tests
static_assert(!rng::weakly_incrementable<void>, "");
static_assert(rng::weakly_incrementable<int>, "");
static_assert(rng::weakly_incrementable<int*>, "");
static_assert(rng::weakly_incrementable<std::vector<int>::iterator>, "");

// Incrementable tests
static_assert(!rng::incrementable<void>, "");
static_assert(rng::incrementable<int>, "");
static_assert(rng::incrementable<int*>, "");

// Iterator tests
static_assert(!rng::input_or_output_iterator<void>, "");
static_assert(!rng::input_or_output_iterator<int>, "");
static_assert(rng::input_or_output_iterator<int*>, "");
static_assert(rng::input_or_output_iterator<int const*>, "");
static_assert(!rng::input_or_output_iterator<std::unique_ptr<int>>, "");
static_assert(rng::input_or_output_iterator<std::vector<int>::iterator>, "");
static_assert(rng::input_or_output_iterator<std::vector<bool>::const_iterator>, "");

// Sentinel tests
static_assert(!rng::sentinel_for<void, void>, "");
static_assert(!rng::sentinel_for<void, int*>, "");
static_assert(rng::sentinel_for<int*, int*>, "");

// InputIterator tests
static_assert(!rng::input_iterator<void>, "");
static_assert(!rng::input_iterator<float>, "");
static_assert(rng::input_iterator<int*>, "");
static_assert(rng::input_iterator<int const*>, "");
static_assert(!rng::input_iterator<std::unique_ptr<int>>, "");
static_assert(rng::input_iterator<std::vector<int>::iterator>, "");
static_assert(rng::input_iterator<std::vector<bool>::const_iterator>, "");


// OutputIterator tests
static_assert(!rng::output_iterator<void, void>, "");
static_assert(!rng::output_iterator<int&, int>, "");
static_assert(rng::output_iterator<int*, int>, "");
static_assert(!rng::output_iterator<int const*, int>, "");
static_assert(rng::output_iterator<std::vector<int>::iterator, int>, "");
static_assert(!rng::output_iterator<std::vector<int>::const_iterator, int>, "");
// Hmmm....
//static_assert(rng::OutputIterator<std::vector<bool>::iterator, bool>, "");
static_assert(!rng::output_iterator<std::vector<bool>::const_iterator, bool>, "");

// ForwardIterator tests
static_assert(!rng::forward_iterator<void>, "");
static_assert(rng::forward_iterator<int*>, "");
static_assert(rng::forward_iterator<std::vector<int>::iterator>, "");

// BidirectionalIterator tests
static_assert(!rng::bidirectional_iterator<void>, "");
static_assert(rng::bidirectional_iterator<int*>, "");
static_assert(rng::bidirectional_iterator<std::vector<int>::iterator>, "");

// RandomAccessIterator tests
static_assert(!rng::random_access_iterator<void>, "");
static_assert(rng::random_access_iterator<int*>, "");
static_assert(rng::random_access_iterator<std::vector<int>::iterator>, "");

// ContiguousIterator tests
static_assert(!rng::contiguous_iterator<void>, "");
static_assert(!rng::contiguous_iterator<void*>, "");
static_assert(rng::contiguous_iterator<int*>, "");
static_assert(rng::contiguous_iterator<const int*>, "");

// IndirectUnaryInvocable tests
static_assert(!rng::indirect_unary_invocable<void, void>, "");


// Range tests
static_assert(!rng::range<void>, "");
static_assert(rng::range<std::vector<int>>, "");

// SizedRange tests
static_assert(!rng::sized_range<void>, "");
static_assert(rng::sized_range<std::vector<int>>, "");

// ContiguousRange tests
static_assert(!rng::contiguous_range<void>, "");
static_assert(!rng::contiguous_range<void*>, "");
static_assert(rng::contiguous_range<std::vector<int>>, "");

// View tests
static_assert(!rng::view<void>, "");
static_assert(!rng::view<std::vector<int>&>, "");

// common_iterator
using I = rng::common_iterator<int*, rng::unreachable_sentinel_t>;
static_assert(rng::input_or_output_iterator<rng::common_iterator<int*, rng::unreachable_sentinel_t>>, "");
static_assert(rng::input_iterator<rng::common_iterator<int*, rng::unreachable_sentinel_t>>, "");
static_assert(rng::forward_iterator<rng::common_iterator<int*, rng::unreachable_sentinel_t>>, "");
static_assert(rng::equality_comparable<I>, "");
using eq = decltype(std::declval<I const&>() == std::declval<I const&>());

// Regression test for #24
struct value_type_and_element_type {
    using value_type = int;
    using element_type = int;
};
static_assert(!rng::readable<value_type_and_element_type>, "");