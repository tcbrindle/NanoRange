
#include <nanorange.hpp>

#include <iterator>
#include <forward_list>
#include <functional>
#include <list>
#include <vector>

namespace rng = tcb::ranges;

/*
 * Iterator type traits static tests
 */

// Non-iterator should not match anything
static_assert(!rng::detail::is_input_iterator_v<float>, "");
static_assert(!rng::detail::is_forward_iterator_v<float>, "");
static_assert(!rng::detail::is_bidirectional_iterator_v<float>, "");
static_assert(!rng::detail::is_random_access_iterator_v<float>, "");
static_assert(!rng::detail::is_output_iterator_v<float, float>, "");

// Input iterator tests
using input_iter_t = std::istream_iterator<char>;
static_assert(rng::detail::is_input_iterator_v<input_iter_t>, "");
static_assert(!rng::detail::is_forward_iterator_v<input_iter_t>, "");
static_assert(!rng::detail::is_bidirectional_iterator_v<input_iter_t>, "");
static_assert(!rng::detail::is_random_access_iterator_v<input_iter_t>, "");
static_assert(!rng::detail::is_output_iterator_v<input_iter_t, char>, "");

// Forward iterator tests
using fwd_iter_t = typename std::forward_list<int>::iterator;
static_assert(rng::detail::is_input_iterator_v<fwd_iter_t>, "");
static_assert(rng::detail::is_forward_iterator_v<fwd_iter_t>, "");
static_assert(!rng::detail::is_bidirectional_iterator_v<fwd_iter_t>, "");
static_assert(!rng::detail::is_random_access_iterator_v<fwd_iter_t>, "");
static_assert(rng::detail::is_output_iterator_v<fwd_iter_t, int>, "");

// Bidirectional iterator tests
using bidir_iter_t = typename std::list<int>::iterator;
static_assert(rng::detail::is_input_iterator_v<bidir_iter_t>, "");
static_assert(rng::detail::is_forward_iterator_v<bidir_iter_t>, "");
static_assert(rng::detail::is_bidirectional_iterator_v<bidir_iter_t>, "");
static_assert(!rng::detail::is_random_access_iterator_v<bidir_iter_t>, "");
static_assert(rng::detail::is_output_iterator_v<bidir_iter_t, int>, "");

// Random access iterator tests
using ra_iter_t = typename std::vector<int>::iterator;
static_assert(rng::detail::is_input_iterator_v<ra_iter_t>, "");
static_assert(rng::detail::is_forward_iterator_v<ra_iter_t>, "");
static_assert(rng::detail::is_bidirectional_iterator_v<ra_iter_t>, "");
static_assert(rng::detail::is_random_access_iterator_v<ra_iter_t>, "");
static_assert(rng::detail::is_output_iterator_v<ra_iter_t, int>, "");

// Pointer tests
using ptr_t = int*;
static_assert(rng::detail::is_input_iterator_v<ptr_t>, "");
static_assert(rng::detail::is_forward_iterator_v<ptr_t>, "");
static_assert(rng::detail::is_bidirectional_iterator_v<ptr_t>, "");
static_assert(rng::detail::is_random_access_iterator_v<ptr_t>, "");
static_assert(rng::detail::is_output_iterator_v<ptr_t, int>, "");

// Output iterator tests
using output_iter_t = std::ostream_iterator<char>;
static_assert(!rng::detail::is_input_iterator_v<output_iter_t>, "");
static_assert(!rng::detail::is_forward_iterator_v<output_iter_t>, "");
static_assert(!rng::detail::is_bidirectional_iterator_v<output_iter_t>, "");
static_assert(!rng::detail::is_random_access_iterator_v<output_iter_t>, "");
static_assert(rng::detail::is_output_iterator_v<output_iter_t, char>, "");

/*
 * Range type traits static tests
 */

// Non-ranges should not match anything
static_assert(!rng::detail::is_input_range_v<float>, "");
static_assert(!rng::detail::is_forward_range_v<float>, "");
static_assert(!rng::detail::is_bidirectional_range_v<float>, "");
static_assert(!rng::detail::is_random_access_range_v<float>, "");
static_assert(!rng::detail::is_output_range_v<float, float>, "");

// Input range tests
struct input_rng_t {
    std::istream_iterator<char> begin();
    std::istream_iterator<char> end();
};

static_assert(rng::detail::is_input_range_v<input_rng_t>, "");
static_assert(!rng::detail::is_forward_range_v<input_rng_t>, "");
static_assert(!rng::detail::is_bidirectional_range_v<input_rng_t>, "");
static_assert(!rng::detail::is_random_access_range_v<input_rng_t>, "");
static_assert(!rng::detail::is_output_range_v<input_rng_t, char>, "");

// Forward range tests
using fwd_rng_t = std::forward_list<int>;
static_assert(rng::detail::is_input_range_v<fwd_rng_t>, "");
static_assert(rng::detail::is_forward_range_v<fwd_rng_t>, "");
static_assert(!rng::detail::is_bidirectional_range_v<fwd_rng_t>, "");
static_assert(!rng::detail::is_random_access_range_v<fwd_rng_t>, "");
static_assert(rng::detail::is_output_range_v<fwd_rng_t, int>, "");
static_assert(!rng::detail::is_output_range_v<const fwd_rng_t, int>, "");

// Bidirectional range tests
using bidir_rng_t = std::list<int>;
static_assert(rng::detail::is_input_range_v<bidir_rng_t>, "");
static_assert(rng::detail::is_forward_range_v<bidir_rng_t>, "");
static_assert(rng::detail::is_bidirectional_range_v<bidir_rng_t>, "");
static_assert(!rng::detail::is_random_access_range_v<bidir_rng_t>, "");
static_assert(rng::detail::is_output_range_v<bidir_rng_t, int>, "");
static_assert(!rng::detail::is_output_range_v<const bidir_rng_t, int>, "");

// Random access range tests
using ra_rng_t = std::vector<int>;
static_assert(rng::detail::is_input_range_v<ra_rng_t>, "");
static_assert(rng::detail::is_forward_range_v<ra_rng_t>, "");
static_assert(rng::detail::is_bidirectional_range_v<ra_rng_t>, "");
static_assert(rng::detail::is_random_access_range_v<ra_rng_t>, "");
static_assert(rng::detail::is_output_range_v<ra_rng_t, int>, "");
static_assert(!rng::detail::is_output_range_v<const ra_rng_t, int>, "");

// C array tests
using array_t = int[4];
static_assert(rng::detail::is_input_range_v<array_t>, "");
static_assert(rng::detail::is_forward_range_v<array_t>, "");
static_assert(rng::detail::is_bidirectional_range_v<array_t>, "");
static_assert(rng::detail::is_random_access_range_v<array_t>, "");
static_assert(rng::detail::is_output_range_v<array_t, int>, "");
static_assert(!rng::detail::is_output_range_v<const array_t, int>, "");

// Output range tests
namespace test {
struct output_rng_t;

std::ostream_iterator<char> begin(const output_rng_t&);
std::ostream_iterator<char> end(const output_rng_t&);
}
using test::output_rng_t;

static_assert(!rng::detail::is_input_range_v<output_rng_t>, "");
static_assert(!rng::detail::is_forward_range_v<output_rng_t>, "");
static_assert(!rng::detail::is_bidirectional_range_v<output_rng_t>, "");
static_assert(!rng::detail::is_random_access_range_v<output_rng_t>, "");
static_assert(rng::detail::is_output_range_v<output_rng_t, char>, "");


/*
 * Predicate type trait tests
 */

void* unary_pred(int);

static_assert(!rng::detail::is_unary_predicate_v<int, int>, "");
static_assert(rng::detail::is_unary_predicate_v<decltype(unary_pred), int>, "");

static_assert(!rng::detail::is_binary_predicate_v<int, int, int>, "");
static_assert(rng::detail::is_binary_predicate_v<std::equal_to<>, int, float>, "");

