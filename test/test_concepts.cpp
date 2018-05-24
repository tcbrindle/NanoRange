
#include <nanorange/range.hpp>
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

// Same concept tests

static_assert(rng::Same<int, int>, "");
static_assert(!rng::Same<float, double>, "");
static_assert(rng::Same<void, void>, "");
static_assert(rng::Same<incomplete, incomplete>, "");
static_assert(!rng::Same<int, void>, "");


// DerivedFrom tests
static_assert(!rng::DerivedFrom<int, int>, "");
static_assert(!rng::DerivedFrom<void, incomplete>, "");
static_assert(!rng::DerivedFrom<int, float>, "");
static_assert(rng::DerivedFrom<derived, base>, "");
static_assert(!rng::DerivedFrom<base, derived>, "");
static_assert(!rng::DerivedFrom<private_derived, base>, "");

// ConvertibleTo tests
static_assert(rng::ConvertibleTo<void, void>, "");
static_assert(!rng::ConvertibleTo<int, void>, "");
static_assert(!rng::ConvertibleTo<void, int>, "");
static_assert(rng::ConvertibleTo<int, int>, "");
static_assert(rng::ConvertibleTo<int, const int>, "");
static_assert(rng::ConvertibleTo<const int, int>, "");
static_assert(rng::ConvertibleTo<int&, const volatile int>, "");
static_assert(rng::ConvertibleTo<int&, int const&>, "");
static_assert(!rng::ConvertibleTo<const int&, int&>, "");
static_assert(rng::ConvertibleTo<int&&, int const&>, "");
static_assert(!rng::ConvertibleTo<int&, int&&>, "");
// Hmmm, is this correct?
static_assert(!rng::ConvertibleTo<int[], int[]>, "");
static_assert(rng::ConvertibleTo<int, bool>, "");
static_assert(rng::ConvertibleTo<float, int>, "");
static_assert(rng::ConvertibleTo<derived&, base&>, "");
static_assert(!rng::ConvertibleTo<base&, derived&>, "");
static_assert(!rng::ConvertibleTo<private_derived&, base&>, "");

static_assert(rng::ConvertibleTo<int&, from_int_only>, "");
static_assert(!rng::ConvertibleTo<long, from_int_only>, "");

static_assert(rng::ConvertibleTo<to_int, int>, "");
static_assert(rng::ConvertibleTo<to_int, long>, "");

static_assert(rng::ConvertibleTo<int*, void*>, "");
static_assert(!rng::ConvertibleTo<void*, int*>, "");

static_assert(rng::ConvertibleTo<const char*, std::string>, "");
static_assert(!rng::ConvertibleTo<std::string, const char*>, "");

// CommonReference tests
static_assert(rng::CommonReference<int&, int&>, "");
static_assert(!rng::CommonReference<void, int>, "");
using void_cr = rng::common_reference_t<void, void>;
static_assert(rng::Same<void_cr, void>, "");
static_assert(rng::ConvertibleTo<void, void>, "");
static_assert(rng::CommonReference<void, void>, "");


// Common tests
static_assert(rng::Common<int, int>, "");

// Integal tests
static_assert(rng::Integral<char>, "");
static_assert(rng::Integral<signed char>, "");
static_assert(rng::Integral<unsigned char>, "");
static_assert(rng::Integral<bool>, "");
static_assert(!rng::Integral<float>, "");
static_assert(!rng::Integral<int&>, "");
static_assert(rng::Integral<const int>, "");
static_assert(!rng::Integral<float>, "");
static_assert(!rng::Integral<std::string>, "");
static_assert(!rng::Integral<void>, "");

// SignedIntegral tests
static_assert(rng::SignedIntegral<signed char>, "");
static_assert(!rng::SignedIntegral<unsigned char>, "");
static_assert(!rng::SignedIntegral<bool>, "");
static_assert(!rng::SignedIntegral<std::string>, "");

// UnsignedIntegral tests
static_assert(rng::UnsignedIntegral<unsigned char>, "");
static_assert(!rng::UnsignedIntegral<signed char>, "");
static_assert(!rng::UnsignedIntegral<signed>, "");
static_assert(!rng::UnsignedIntegral<void>, "");
static_assert(!rng::UnsignedIntegral<std::string>, "");

// Assignable tests
struct weird_assign {
    int operator=(const weird_assign&);
};

static_assert(rng::Assignable<int&, int&>, "");
static_assert(rng::Assignable<int&, int>, "");
static_assert(rng::Assignable<int&, int&&>, "");
static_assert(!rng::Assignable<int, int&>, "");
static_assert(!rng::Assignable<int const&, int&>, "");
static_assert(rng::Assignable<std::string&, const char*>, "");
static_assert(!rng::Assignable<weird_assign&, weird_assign&>, "");
static_assert(!rng::Assignable<void, int>, "");

// Swappable tests
static_assert(rng::Swappable<int>, "");
static_assert(!rng::Swappable<void>, "");
static_assert(rng::Swappable<std::string>, "");
static_assert(rng::Swappable<base>, "");

static_assert(!rng::SwappableWith<int, long>, "");
static_assert(!rng::SwappableWith<int, const int>, "");
static_assert(!rng::SwappableWith<int[], int[]>, "");
static_assert(!rng::SwappableWith<int*, void*>, "");
static_assert(!rng::SwappableWith<base, derived>, "");

// Destructible tests
struct throwing_dtor {
    ~throwing_dtor() noexcept(false) {}
};

class private_dtor {
    ~private_dtor() = default;
};

static_assert(rng::Destructible<int>, "");
static_assert(rng::Destructible<std::string>, "");
static_assert(!rng::Destructible<void>, "");
static_assert(!rng::Destructible<throwing_dtor>, "");
static_assert(!rng::Destructible<private_dtor>, "");

// Constructible tests
static_assert(rng::Constructible<int, long&>, "");
static_assert(rng::Constructible<base&, derived&>, "");
static_assert(rng::Constructible<std::string, const char(&)[6]>, "");
static_assert(rng::Constructible<std::string, char, int, std::allocator<char>>, "");
static_assert(!rng::Constructible<throwing_dtor>, "");

// DefaultConstructible tests
struct agg {
    int i; float f;
};

static_assert(!rng::DefaultConstructible<void>, "");
static_assert(rng::DefaultConstructible<int>, "");
static_assert(rng::DefaultConstructible<agg>, "");
static_assert(rng::DefaultConstructible<std::string>, "");
static_assert(!rng::DefaultConstructible<from_int_only>, "");

// MoveConstructible tests
struct no_copy_or_move {
    no_copy_or_move(const no_copy_or_move&) = delete;
    no_copy_or_move& operator=(const no_copy_or_move&) = delete;
};

static_assert(!rng::MoveConstructible<void>, "");
static_assert(rng::MoveConstructible<int>, "");
static_assert(rng::MoveConstructible<std::string>, "");
static_assert(rng::MoveConstructible<std::unique_ptr<int>>, "");
static_assert(!rng::MoveConstructible<no_copy_or_move>, "");

// CopyConstructible tests
struct odd_copy_ctor {
    odd_copy_ctor(odd_copy_ctor&);
};

static_assert(!rng::CopyConstructible<void>, "");
static_assert(rng::CopyConstructible<int>, "");
static_assert(rng::CopyConstructible<std::string>, "");
static_assert(!rng::CopyConstructible<std::unique_ptr<int>>, "");
static_assert(!rng::CopyConstructible<no_copy_or_move>, "");
static_assert(!rng::CopyConstructible<odd_copy_ctor>, "");


// Boolean tests
struct explicitly_convertible_to_bool {
    explicit operator bool();
};

static_assert(!rng::Boolean<void>, "");
static_assert(rng::Boolean<bool>, "");
static_assert(rng::Boolean<int>, "");
static_assert(rng::Boolean<std::true_type>, "");
static_assert(rng::Boolean<std::bitset<1>::reference>, "");
static_assert(rng::Boolean<std::vector<bool>::reference>, "");
static_assert(!rng::Boolean<int*>, "");
static_assert(!rng::Boolean<std::unique_ptr<int>>, "");
static_assert(!rng::Boolean<explicitly_convertible_to_bool>, "");

// EqualityComparable tests
static_assert(rng::EqualityComparable<int>, "");
static_assert(rng::EqualityComparable<int&>, "");
static_assert(!rng::EqualityComparable<void>, "");
static_assert(!rng::EqualityComparable<std::thread>, "");

// EqualityComparableWith tests
static_assert(rng::EqualityComparableWith<double, double>, "");
static_assert(rng::EqualityComparableWith<std::string, const char*>, "");
static_assert(!rng::EqualityComparableWith<int, void>, "");
static_assert(!rng::EqualityComparableWith<int, std::string>, "");

// StrictTotallyOrdered tests
static_assert(!rng::StrictTotallyOrdered<void>, "");
static_assert(rng::StrictTotallyOrdered<int>, "");
static_assert(rng::StrictTotallyOrdered<float>, "");
static_assert(rng::StrictTotallyOrdered<std::string>, "");
static_assert(!rng::StrictTotallyOrdered<std::thread>, "");

// StrictTotallyOrderedWith tests
static_assert(!rng::StrictTotallyOrderedWith<void, void>, "");
static_assert(rng::StrictTotallyOrderedWith<int, int>, "");
//static_assert(rng::StrictTotallyOrderedWith<int, float>, "");
static_assert(rng::StrictTotallyOrderedWith<std::string, const char*>, "");
static_assert(rng::StrictTotallyOrderedWith<int, double>, "");

// Copyable tests
struct odd_assign {
    odd_assign(const odd_assign&) = default;
    odd_assign& operator=(odd_assign&);
};

static_assert(!rng::Copyable<void>, "");
static_assert(rng::Copyable<int>, "");
static_assert(!rng::Copyable<int&>, "");
static_assert(rng::Copyable<std::string>, "");
static_assert(!rng::Copyable<std::unique_ptr<int>>, "");
static_assert(!rng::Copyable<odd_assign>, "");

// Semiregular tests
static_assert(!rng::Semiregular<void>, "");
static_assert(rng::Semiregular<int>, "");
static_assert(rng::Semiregular<std::string>, "");
static_assert(rng::Semiregular<int*>, "");
static_assert(!rng::Semiregular<int&>, "");

// Regular tests
static_assert(!rng::Regular<void>, "");
static_assert(rng::Regular<int>, "");
static_assert(rng::Regular<std::string>, "");
static_assert(rng::Regular<int*>, "");
static_assert(!rng::Regular<int&>, "");

// [Regular]Invocable tests
// FIXME: Add these
static_assert(!rng::Invocable<void>, "");

// Predicate tests
int int_cmp(int, int);

static_assert(!rng::Predicate<void>, "");
static_assert(rng::Predicate<decltype(int_cmp), int, int>, "");
static_assert(rng::Predicate<std::equal_to<>, int, int>, "");
const auto cmp = [] (auto const& lhs, auto const& rhs) { return lhs < rhs; };
static_assert(rng::Predicate<decltype(cmp), int, float>, "");

// Relation tests
static_assert(!rng::Relation<void, void, void>, "");
static_assert(rng::Relation<std::equal_to<>, int, int>, "");

// Readable tests
static_assert(!rng::Readable<void>, "");
static_assert(!rng::Readable<int>, "");
static_assert(rng::Readable<int*>, "");
static_assert(rng::Readable<std::unique_ptr<int>>, "");
static_assert(rng::Readable<std::vector<int>::const_iterator>, "");

struct MoveOnlyReadable {
    using value_type = std::unique_ptr<int>;
    value_type operator*() const;
};

static_assert(rng::Readable<MoveOnlyReadable>, "");

// Writable tests
static_assert(!rng::Writable<void, void>, "");
static_assert(rng::Writable<int*, int>, "");
static_assert(!rng::Writable<int const*, int>, "");
static_assert(rng::Writable<std::unique_ptr<int>, int>, "");
static_assert(rng::Writable<std::vector<int>::iterator, int>, "");
static_assert(!rng::Writable<std::vector<int>::const_iterator, int>, "");

// WeaklyIncrementable tests
static_assert(!rng::WeaklyIncrementable<void>, "");
static_assert(rng::WeaklyIncrementable<int>, "");
static_assert(rng::WeaklyIncrementable<int*>, "");
static_assert(rng::WeaklyIncrementable<std::vector<int>::iterator>, "");

// Incrementable tests
static_assert(!rng::Incrementable<void>, "");
static_assert(rng::Incrementable<int>, "");
static_assert(rng::Incrementable<int*>, "");

// Iterator tests
static_assert(!rng::Iterator<void>, "");
static_assert(!rng::Iterator<int>, "");
static_assert(rng::Iterator<int*>, "");
static_assert(rng::Iterator<int const*>, "");
static_assert(!rng::Iterator<std::unique_ptr<int>>, "");
static_assert(rng::Iterator<std::vector<int>::iterator>, "");
static_assert(rng::Iterator<std::vector<bool>::const_iterator>, "");

// Sentinel tests
static_assert(!rng::Sentinel<void, void>, "");
static_assert(!rng::Sentinel<void, int*>, "");
static_assert(rng::Sentinel<int*, int*>, "");

// InputIterator tests
static_assert(!rng::InputIterator<void>, "");
static_assert(!rng::InputIterator<float>, "");
static_assert(rng::InputIterator<int*>, "");
static_assert(rng::InputIterator<int const*>, "");
static_assert(!rng::InputIterator<std::unique_ptr<int>>, "");
static_assert(rng::InputIterator<std::vector<int>::iterator>, "");
static_assert(rng::InputIterator<std::vector<bool>::const_iterator>, "");


// OutputIterator tests
static_assert(!rng::OutputIterator<void, void>, "");
static_assert(!rng::OutputIterator<int&, int>, "");
static_assert(rng::OutputIterator<int*, int>, "");
static_assert(!rng::OutputIterator<int const*, int>, "");
static_assert(rng::OutputIterator<std::vector<int>::iterator, int>, "");
static_assert(!rng::OutputIterator<std::vector<int>::const_iterator, int>, "");
// Hmmm....
//static_assert(rng::OutputIterator<std::vector<bool>::iterator, bool>, "");
static_assert(!rng::OutputIterator<std::vector<bool>::const_iterator, bool>, "");

// ForwardIterator tests
static_assert(!rng::ForwardIterator<void>, "");
static_assert(rng::ForwardIterator<int*>, "");
static_assert(rng::ForwardIterator<std::vector<int>::iterator>, "");

// BidirectionalIterator tests
static_assert(!rng::BidirectionalIterator<void>, "");
static_assert(rng::BidirectionalIterator<int*>, "");
static_assert(rng::BidirectionalIterator<std::vector<int>::iterator>, "");

// RandomAccessIterator tests
static_assert(!rng::RandomAccessIterator<void>, "");
static_assert(rng::RandomAccessIterator<int*>, "");
static_assert(rng::RandomAccessIterator<std::vector<int>::iterator>, "");

// IndirectUnaryInvocable tests
static_assert(!rng::IndirectUnaryInvocable<void, void>, "");


// Range tests
static_assert(!rng::Range<void>, "");
static_assert(rng::Range<std::vector<int>>, "");

// SizedRange tests
static_assert(!rng::SizedRange<void>, "");
static_assert(rng::SizedRange<std::vector<int>>, "");

// View tests
static_assert(!rng::View<void>, "");
static_assert(!rng::View<std::vector<int>&>, "");

// common_iterator
using I = rng::common_iterator<int*, rng::unreachable>;
static_assert(rng::Iterator<rng::common_iterator<int*, rng::unreachable>>, "");
static_assert(rng::InputIterator<rng::common_iterator<int*, rng::unreachable>>, "");
static_assert(rng::ForwardIterator<rng::common_iterator<int*, rng::unreachable>>, "");
static_assert(rng::EqualityComparable<I>, "");
using eq = decltype(std::declval<I const&>() == std::declval<I const&>());