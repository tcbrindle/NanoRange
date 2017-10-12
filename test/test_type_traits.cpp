
#include <nanorange.hpp>

#include <bitset>
#include <iterator>
#include <forward_list>
#include <functional>
#include <list>
#include <memory>
#include <vector>

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
static_assert(rng::ConvertibleTo<rng::detail::detected_t<rng::iterator_category_t, fwd_iter_t>, std::forward_iterator_tag>, "");
static_assert(rng::Incrementable<fwd_iter_t>, "");
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

/*
 * Predicate type trait tests
 */

bool unary_pred(int);

static_assert(rng::Predicate<decltype(unary_pred)&, int>, "");
static_assert(!rng::IndirectUnaryPredicate<int, ra_iter_t>, "");
static_assert(rng::IndirectUnaryPredicate<decltype(unary_pred)&, fwd_iter_t>, "");


