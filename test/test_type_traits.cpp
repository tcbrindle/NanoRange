
#include <concepts.hpp>

#include <array>
#include <bitset>
#include <iterator>
#include <forward_list>
#include <functional>
#include <list>
#include <memory>
#include <vector>

#include "catch.hpp"

namespace rng = nanorange;

struct move_only {
    move_only(move_only&&) = default;
    move_only& operator=(move_only&&) = default;
};

struct not_movable {
    not_movable(not_movable&&) = delete;
    not_movable& operator=(not_movable&&) = delete;
};


struct explicitly_convertible_to_bool {
    explicit operator bool();
};


static_assert(rng::Boolean<bool>, "");
static_assert(rng::Boolean<std::true_type>, "");
static_assert(rng::Boolean<typename std::bitset<1>::reference>, "");
static_assert(!rng::Boolean<int*>, "");
static_assert(!rng::Boolean<std::unique_ptr<int>>, "");
static_assert(!rng::Boolean<explicitly_convertible_to_bool>, "");

static_assert(rng::Same<rng::difference_type_t<int*>, ptrdiff_t>, "");

static_assert(rng::Same<rng::value_type_t<std::vector<int>>, int>, "");

static_assert(std::is_rvalue_reference<rng::rvalue_reference_t<int*>>::value, "");
static_assert(std::is_rvalue_reference<rng::rvalue_reference_t<typename std::vector<int>::iterator>>::value, "");

static_assert(rng::Writable<int*, int>, "");
static_assert(!rng::Writable<const int*, int>, "");

constexpr int arr[] = {1, 2, 3, 4};
static_assert(nanorange::begin(arr) == arr, "");
static_assert(nanorange::end(arr) == arr + 4, "");

constexpr bool test_basic_swap()
{
    int a = 1;
    int b = 2;
    nanorange::swap(a, b);
    return a == 2 && b == 1;
}

constexpr bool test_array_swap()
{
    int a[] = {1, 2};
    int b[] = {3, 4};

    nanorange::swap(a, b);

    return a[0] == 3 && a[1] == 4 &&
            b[0] == 1 && b[1] == 2;
}

namespace test {

// Pfft, std::array should be constexpr in C++14. Boo.
template <typename T, std::size_t N>
struct carray {
    constexpr const T& operator[](std::size_t i) const { return data[i]; }
    T data[N];

    friend constexpr bool swap(carray&, carray&) {
        return false;
    }
};

}

constexpr bool test_custom_swap()
{
    test::carray<int, 2> a{{1, 2}};
    test::carray<int, 2> b{{3, 4}};

    nanorange::swap(a, b);

    return a[0] == 1 && a[1] == 2 &&
            b[0] == 3 && b[1] == 4;
}

static_assert(test_basic_swap(), "");
static_assert(test_array_swap(), "");
static_assert(test_custom_swap(), "");

static_assert(rng::Swappable<int>, "");
static_assert(rng::Swappable<std::vector<int>>, "");
static_assert(rng::Swappable<test::carray<int, 3>>, "");
//static_assert(!rng::Swappable<rng::detail::nonesuch>, "");

namespace test {

struct foo{
    void operator()(int);
};

}

static_assert(rng::Invocable<test::foo, int>, "");

/*
 * Iterator type traits static tests
 */

// Non-iterator should not match anything
static_assert(!rng::InputIterator<float>, "");
static_assert(!rng::ForwardIterator<float>, "");
static_assert(!rng::BidirectionalIterator<float>, "");
static_assert(!rng::RandomAccessIterator<float>, "");
static_assert(!rng::OutputIterator<float, float>, "");

// Input iterator tests
using input_iter_t = std::istream_iterator<char>;
static_assert(rng::InputIterator<input_iter_t>, "");
static_assert(!rng::ForwardIterator<input_iter_t>, "");
static_assert(!rng::BidirectionalIterator<input_iter_t>, "");
static_assert(!rng::RandomAccessIterator<input_iter_t>, "");
static_assert(!rng::OutputIterator<input_iter_t, char>, "");

// Forward iterator tests
using fwd_iter_t = typename std::forward_list<int>::iterator;
static_assert(rng::InputIterator<fwd_iter_t>, "");
static_assert(rng::ForwardIterator<fwd_iter_t>, "");
static_assert(!rng::BidirectionalIterator<fwd_iter_t>, "");
static_assert(!rng::RandomAccessIterator<fwd_iter_t>, "");
static_assert(rng::OutputIterator<fwd_iter_t, int>, "");

// Bidirectional iterator tests
using bidir_iter_t = typename std::list<int>::iterator;
static_assert(rng::InputIterator<bidir_iter_t>, "");
static_assert(rng::ForwardIterator<bidir_iter_t>, "");
static_assert(rng::BidirectionalIterator<bidir_iter_t>, "");
static_assert(!rng::RandomAccessIterator<bidir_iter_t>, "");
static_assert(rng::OutputIterator<bidir_iter_t, int>, "");

// Random access iterator tests
using ra_iter_t = typename std::vector<int>::iterator;
static_assert(rng::InputIterator<ra_iter_t>, "");
static_assert(rng::ForwardIterator<ra_iter_t>, "");
static_assert(rng::BidirectionalIterator<ra_iter_t>, "");
static_assert(rng::RandomAccessIterator<ra_iter_t>, "");
static_assert(rng::OutputIterator<ra_iter_t, int>, "");

// Pointer tests
using ptr_t = int*;
static_assert(rng::WeaklyIncrementable<ptr_t>, "");
static_assert(rng::Semiregular<ptr_t>, "");
static_assert(rng::detail::is_detected_v<rng::difference_type_t, ptr_t>, "");
static_assert(rng::Same<rng::difference_type_t<ptr_t>, std::ptrdiff_t>, "");
static_assert(rng::SignedIntegral<rng::detail::detected_t<rng::difference_type_t, ptr_t>>, "");
static_assert(rng::Regular<ptr_t>, "");

static_assert(rng::InputIterator<ptr_t>, "");
static_assert(rng::ForwardIterator<ptr_t>, "");
static_assert(rng::BidirectionalIterator<ptr_t>, "");
static_assert(rng::RandomAccessIterator<ptr_t>, "");
static_assert(rng::OutputIterator<ptr_t, int>, "");

// Output iterator tests
using output_iter_t = rng::ostream_iterator<char>;
static_assert(!rng::InputIterator<output_iter_t>, "");
static_assert(!rng::ForwardIterator<output_iter_t>, "");
static_assert(!rng::BidirectionalIterator<output_iter_t>, "");
static_assert(!rng::RandomAccessIterator<output_iter_t>, "");
static_assert(rng::OutputIterator<output_iter_t, char>, "");
static_assert(rng::Iterator<output_iter_t>, "");

/*
 * Range type traits static tests
 */

// Non-ranges should not match anything
static_assert(!rng::InputRange<float>, "");
static_assert(!rng::ForwardRange<float>, "");
static_assert(!rng::BidirectionalRange<float>, "");
static_assert(!rng::RandomAccessRange<float>, "");
static_assert(!rng::OutputRange<float, float>, "");

// Input range tests
struct input_rng_t {
    std::istream_iterator<char> begin();
    std::istream_iterator<char> end();
};

static_assert(rng::InputRange<input_rng_t>, "");
static_assert(!rng::ForwardRange<input_rng_t>, "");
static_assert(!rng::BidirectionalRange<input_rng_t>, "");
static_assert(!rng::RandomAccessRange<input_rng_t>, "");
static_assert(!rng::OutputRange<input_rng_t, char>, "");

// Forward range tests
using fwd_rng_t = std::forward_list<int>;
static_assert(rng::InputRange<fwd_rng_t>, "");
static_assert(rng::ForwardRange<fwd_rng_t>, "");
static_assert(!rng::BidirectionalRange<fwd_rng_t>, "");
static_assert(!rng::RandomAccessRange<fwd_rng_t>, "");
static_assert(rng::OutputRange<fwd_rng_t, int>, "");
static_assert(!rng::OutputRange<const fwd_rng_t, int>, "");

// Bidirectional range tests
using bidir_rng_t = std::list<int>;
static_assert(rng::InputRange<bidir_rng_t>, "");
static_assert(rng::ForwardRange<bidir_rng_t>, "");
static_assert(rng::BidirectionalRange<bidir_rng_t>, "");
static_assert(!rng::RandomAccessRange<bidir_rng_t>, "");
static_assert(rng::OutputRange<bidir_rng_t, int>, "");
static_assert(!rng::OutputRange<const bidir_rng_t, int>, "");

// Random access range tests
using ra_rng_t = std::vector<int>;
static_assert(rng::InputRange<ra_rng_t>, "");
static_assert(rng::ForwardRange<ra_rng_t>, "");
static_assert(rng::BidirectionalRange<ra_rng_t>, "");
static_assert(rng::RandomAccessRange<ra_rng_t>, "");
//static_assert(rng::SizedRange<ra_rng_t>, "");
static_assert(rng::Same<decltype(rng::size(std::declval<ra_rng_t>())), std::size_t>, "");
static_assert(rng::Same<rng::range_difference_type_t<ra_rng_t>, std::ptrdiff_t>, "");
static_assert(rng::ConvertibleTo<std::size_t, std::ptrdiff_t>, "");
static_assert(rng::OutputRange<ra_rng_t, int>, "");
static_assert(!rng::OutputRange<const ra_rng_t, int>, "");

// C array tests
using array_t = int[4];
static_assert(rng::InputRange<array_t>, "");
static_assert(rng::ForwardRange<array_t>, "");
static_assert(rng::BidirectionalRange<array_t>, "");
static_assert(rng::RandomAccessRange<array_t>, "");
static_assert(rng::OutputRange<array_t, int>, "");
static_assert(!rng::OutputRange<const array_t, int>, "");


static_assert(rng::Same<decltype(rng::begin(std::declval<ra_rng_t&>())),
                        typename ra_rng_t::iterator>, "");
static_assert(rng::Same<decltype(rng::begin(std::declval<const ra_rng_t&>())),
                        typename ra_rng_t::const_iterator>, "");
static_assert(rng::Same<decltype(rng::begin(std::declval<ra_rng_t&&>())),
                        typename ra_rng_t::const_iterator>, "");
static_assert(rng::Same<decltype(rng::begin(std::declval<const ra_rng_t&&>())),
                        typename ra_rng_t::const_iterator>, "");

static_assert(rng::Same<decltype(rng::end(std::declval<ra_rng_t&>())),
        typename ra_rng_t::iterator>, "");
static_assert(rng::Same<decltype(rng::end(std::declval<const ra_rng_t&>())),
        typename ra_rng_t::const_iterator>, "");
static_assert(rng::Same<decltype(rng::end(std::declval<ra_rng_t&&>())),
        typename ra_rng_t::const_iterator>, "");
static_assert(rng::Same<decltype(rng::end(std::declval<const ra_rng_t&&>())),
        typename ra_rng_t::const_iterator>, "");

static_assert(rng::Same<decltype(rng::cbegin(std::declval<ra_rng_t&>())),
        typename ra_rng_t::const_iterator>, "");
static_assert(rng::Same<decltype(rng::cbegin(std::declval<const ra_rng_t&>())),
        typename ra_rng_t::const_iterator>, "");
static_assert(rng::Same<decltype(rng::cbegin(std::declval<ra_rng_t&&>())),
        typename ra_rng_t::const_iterator>, "");
static_assert(rng::Same<decltype(rng::cbegin(std::declval<const ra_rng_t&&>())),
        typename ra_rng_t::const_iterator>, "");

static_assert(rng::Same<decltype(rng::cend(std::declval<ra_rng_t&>())),
        typename ra_rng_t::const_iterator>, "");
static_assert(rng::Same<decltype(rng::cend(std::declval<const ra_rng_t&>())),
        typename ra_rng_t::const_iterator>, "");
static_assert(rng::Same<decltype(rng::cend(std::declval<ra_rng_t&&>())),
        typename ra_rng_t::const_iterator>, "");
static_assert(rng::Same<decltype(rng::cend(std::declval<const ra_rng_t&&>())),
        typename ra_rng_t::const_iterator>, "");

static_assert(rng::Same<decltype(rng::rbegin(std::declval<ra_rng_t&>())),
        typename ra_rng_t::reverse_iterator>, "");
static_assert(rng::Same<decltype(rng::rbegin(std::declval<const ra_rng_t&>())),
        typename ra_rng_t::const_reverse_iterator>, "");
static_assert(rng::Same<decltype(rng::rbegin(std::declval<ra_rng_t&&>())),
        typename ra_rng_t::const_reverse_iterator>, "");
static_assert(rng::Same<decltype(rng::rbegin(std::declval<const ra_rng_t&&>())),
        typename ra_rng_t::const_reverse_iterator>, "");

static_assert(rng::Same<decltype(rng::rend(std::declval<ra_rng_t&>())),
        typename ra_rng_t::reverse_iterator>, "");
static_assert(rng::Same<decltype(rng::rend(std::declval<const ra_rng_t&>())),
        typename ra_rng_t::const_reverse_iterator>, "");
static_assert(rng::Same<decltype(rng::rend(std::declval<ra_rng_t&&>())),
        typename ra_rng_t::const_reverse_iterator>, "");
static_assert(rng::Same<decltype(rng::rend(std::declval<const ra_rng_t&&>())),
        typename ra_rng_t::const_reverse_iterator>, "");

static_assert(rng::Same<decltype(rng::crbegin(std::declval<ra_rng_t&>())),
        typename ra_rng_t::const_reverse_iterator>, "");
static_assert(rng::Same<decltype(rng::crbegin(std::declval<const ra_rng_t&>())),
        typename ra_rng_t::const_reverse_iterator>, "");
static_assert(rng::Same<decltype(rng::crbegin(std::declval<ra_rng_t&&>())),
        typename ra_rng_t::const_reverse_iterator>, "");
static_assert(rng::Same<decltype(rng::crbegin(std::declval<const ra_rng_t&&>())),
        typename ra_rng_t::const_reverse_iterator>, "");

static_assert(rng::Same<decltype(rng::crend(std::declval<ra_rng_t&>())),
        typename ra_rng_t::const_reverse_iterator>, "");
static_assert(rng::Same<decltype(rng::crend(std::declval<const ra_rng_t&>())),
        typename ra_rng_t::const_reverse_iterator>, "");
static_assert(rng::Same<decltype(rng::crend(std::declval<ra_rng_t&&>())),
        typename ra_rng_t::const_reverse_iterator>, "");
static_assert(rng::Same<decltype(rng::crend(std::declval<const ra_rng_t&&>())),
        typename ra_rng_t::const_reverse_iterator>, "");


// Output range tests
namespace test {
struct output_rng_t {

    struct iterator {
        using value_type = char;
        using reference = void;
        using iterator_category = std::output_iterator_tag;
        using pointer = void;
        using difference_type = std::ptrdiff_t;

        iterator& operator=(char);
        iterator& operator*();
        iterator& operator++();
        iterator& operator++(int);
        bool operator==(iterator) const;
        bool operator!=(iterator) const;
    };

};

output_rng_t::iterator begin(output_rng_t&);
output_rng_t::iterator end(output_rng_t&);
}
using test::output_rng_t;

static_assert(!rng::InputRange<output_rng_t>, "");
static_assert(!rng::ForwardRange<output_rng_t>, "");
static_assert(!rng::BidirectionalRange<output_rng_t>, "");
static_assert(!rng::RandomAccessRange<output_rng_t>, "");
static_assert(rng::OutputRange<output_rng_t, char>, "");
static_assert(rng::Range<output_rng_t>, "");
static_assert(rng::Sentinel<output_rng_t::iterator, output_rng_t::iterator>, "");
static_assert(rng::Iterator<output_rng_t::iterator>, "");
static_assert(rng::Semiregular<output_rng_t::iterator>, "");
static_assert(rng::WeaklyEqualityComparableWith<output_rng_t::iterator, output_rng_t::iterator>, "");
static_assert(rng::OutputIterator<rng::iterator_t<output_rng_t>, char>, "");

// View tests

struct my_type{};

template <>
struct rng::enable_view<std::vector<my_type>> : std::true_type {};

struct my_view1  {
    int* begin();
    int* begin() const;
    int* end();
    int* end() const;
};

struct my_view2 : rng::view_base {
    int* begin();
    const int* begin() const;
    int* end();
    const int* end() const;
};

static_assert(!rng::View<float>, "");
static_assert(!rng::View<ra_rng_t>, "");
static_assert(!rng::View<std::initializer_list<int>>, "");
static_assert(rng::View<std::vector<my_type>>, "");
static_assert(rng::View<my_view1>, "");
static_assert(rng::View<my_view2>, "");

/*
 * Predicate type trait tests
 */

bool unary_pred(int);

static_assert(rng::Predicate<decltype(unary_pred)&, int>, "");
static_assert(!rng::IndirectUnaryPredicate<int, ra_iter_t>, "");
static_assert(rng::IndirectUnaryPredicate<decltype(unary_pred)&, fwd_iter_t>, "");


/*
 * Common reference trait tests
 * This is a little hampered by the fact that I don't know exactly what
 * common_reference_t is supposed to return in complex cases...
 */

// Zero parameter case
static_assert(!rng::detail::is_detected_v<rng::common_reference_t>, "");

// One parameter case
static_assert(rng::Same<rng::common_reference_t<int>, int>, "");

// Two parameter cases
static_assert(rng::Same<rng::common_reference_t<int&, int&>, int&>, "");
static_assert(rng::Same<rng::common_reference_t<const int&, int&>, const int&>, "");
static_assert(rng::Same<rng::common_reference_t<int&&, int&>, const int&>, "");
static_assert(rng::Same<rng::common_reference_t<int&, int&&>, const int&>, "");
static_assert(rng::Same<rng::common_reference_t<int&&, int&&>, int&&>, "");
static_assert(rng::Same<rng::common_reference_t<const int&&, int&&>, const int&&>, "");

namespace  test {

struct base {};
struct derived : base {};
struct derived2 : base{};

}
static_assert(rng::Same<rng::common_reference_t<test::base&, test::derived&>, test::base&>, "");
static_assert(rng::Same<rng::common_reference_t<test::base&&, test::derived&&>, test::base&&>, "");


// Three parameter cases
static_assert(rng::Same<rng::common_reference_t<int&, const int&, volatile int&>, const volatile int&>, "");
static_assert(rng::Same<rng::common_reference_t<test::base&, test::derived&, test::derived2&>, test::base&>, "");
// but...
static_assert(!rng::detail::is_detected_v<
        rng::common_reference_t, test::derived&, test::derived2&, test::base&>, "");

static_assert(rng::CommonReference<int&, const volatile int&>, "");
static_assert(rng::Common<const int&&, volatile int>, "");



