
#ifndef TCB_RANGES_HPP_INCLUDED
#define TCB_RANGES_HPP_INCLUDED

#include <algorithm>
#include <functional>
#include <iterator>
#include <numeric>
#include <type_traits>

#ifdef TCB_RANGES_NO_DEPRECATION_WARNINGS
#define TCB_RANGES_DEPRECATED
#else
#define TCB_RANGES_DEPRECATED [[deprecated]]
#endif

namespace tcb {
namespace ranges {

namespace detail {
inline namespace adl {

using std::begin;
using std::end;

template <typename T>
auto adl_begin(T&& t) -> decltype(begin(std::forward<T>(t)))
{
    return begin(std::forward<T>(t));
}

template <typename T>
auto adl_end(T&& t) -> decltype(end(std::forward<T>(t)))
{
    return end(std::forward<T>(t));
}

} // end inline namespace adl
} // end namespace detail

/*
 * Various aliases taken from the Ranges TS.
 * For similicity we take these from iterator_traits, rather than the other way
 * around as the TS does.
 */
template <typename Iter>
using difference_type_t = typename std::iterator_traits<Iter>::difference_type;

template <typename Iter>
using value_type_t = typename std::iterator_traits<Iter>::value_type;

template <typename Iter>
using reference_t = typename std::iterator_traits<Iter>::reference;

template <typename Iter>
using iterator_category_t = typename std::iterator_traits<Iter>::iterator_category;

template <typename Rng>
using iterator_t = decltype(detail::adl_begin(std::declval<Rng&>()));

template <typename Rng>
using sentinel_t = decltype(detail::adl_end(std::declval<Rng&>()));

template <typename Rng>
using range_value_type_t = value_type_t<iterator_t<Rng>>;

template <typename Rng>
using range_difference_type_t = difference_type_t<iterator_t<Rng>>;

namespace detail {

template <typename...>
using void_t = void;

template <typename, typename, typename = void>
constexpr bool category_at_least_v = false;

template <typename Iter, typename Tag>
constexpr bool category_at_least_v<Iter, Tag,
        std::enable_if_t<std::is_base_of<Tag, iterator_category_t<Iter>>::value>> = true;

/* Iterator "concept" checks */

// For our purposes, a type is an iterator if iterator_traits<T> contains
// all five required typedefs.
template <typename, typename = void>
constexpr bool is_iterator_v = false;

template <typename Iter>
constexpr bool is_iterator_v<Iter, void_t<
        value_type_t<Iter>,
        difference_type_t<Iter>,
        reference_t<Iter>,
        typename std::iterator_traits<Iter>::pointer,
        iterator_category_t<Iter>>> = true;

template <typename Iter>
constexpr bool is_input_iterator_v = is_iterator_v<Iter> &&
        category_at_least_v<Iter, std::input_iterator_tag>;

template <typename Iter>
constexpr bool is_forward_iterator_v = is_iterator_v<Iter> &&
        category_at_least_v<Iter, std::forward_iterator_tag>;

template <typename Iter>
constexpr bool is_bidirectional_iterator_v = is_iterator_v<Iter> &&
        category_at_least_v<Iter, std::bidirectional_iterator_tag>;

template <typename Iter>
constexpr bool is_random_access_iterator_v = is_iterator_v<Iter> &&
        category_at_least_v<Iter, std::random_access_iterator_tag>;

template <typename, typename, typename = void>
constexpr bool is_writable_v = false;

template <typename Iter, typename T>
constexpr bool is_writable_v<Iter, T, void_t<decltype(*std::declval<Iter&>() = std::declval<T>())>>
    = true;

template <typename Iter, typename T>
constexpr bool is_output_iterator_v = is_iterator_v<Iter> &&
        (category_at_least_v<Iter, std::output_iterator_tag> || is_forward_iterator_v<Iter>) &&
        is_writable_v<Iter, T>;


/* Range tests */

// For our purposes, a type is a range if the result of begin() is an
// iterator, and begin() and end() return the same type
template <typename, typename = void>
constexpr bool is_range_v = false;

template <typename Rng>
constexpr bool is_range_v<Rng, std::enable_if_t<
        is_iterator_v<iterator_t<Rng>> &&
        std::is_same<iterator_t<Rng>, sentinel_t<Rng>>::value>> = true;

template <typename, typename = void>
constexpr bool is_input_range_v = false;

template <typename Rng>
constexpr bool is_input_range_v<Rng, std::enable_if_t<
        is_range_v<Rng> && is_input_iterator_v<iterator_t<Rng>>>> = true;

template <typename, typename = void>
constexpr bool is_forward_range_v = false;

template <typename Rng>
constexpr bool is_forward_range_v<Rng, std::enable_if_t<
        is_range_v<Rng> && is_forward_iterator_v<iterator_t<Rng>>>> = true;

template <typename, typename = void>
constexpr bool is_bidirectional_range_v = false;

template <typename Rng>
constexpr bool is_bidirectional_range_v<Rng, std::enable_if_t<
        is_range_v<Rng> && is_bidirectional_iterator_v<iterator_t<Rng>>>> = true;

template <typename, typename = void>
constexpr bool is_random_access_range_v = false;

template <typename Rng>
constexpr bool is_random_access_range_v<Rng, std::enable_if_t<
        is_range_v<Rng> && is_random_access_iterator_v<iterator_t<Rng>>>> = true;

template <typename, typename, typename = void>
constexpr bool is_output_range_v = false;

template <typename Rng, typename T>
constexpr bool is_output_range_v<Rng, T, std::enable_if_t<
        is_range_v<Rng> && is_output_iterator_v<iterator_t<Rng>, T>>> = true;



template <typename, typename, typename = void>
struct is_unary_predicate : std::false_type {};

template <typename Func, typename T>
struct is_unary_predicate<Func, T,
        std::enable_if_t<
                std::is_convertible<
                        std::result_of_t<Func&(T)>, bool>::value>>
    : std::true_type {};

template <typename Func, typename T>
constexpr bool is_unary_predicate_v = is_unary_predicate<Func, T>::value;

template <typename, typename, typename, typename = void>
struct is_binary_predicate : std::false_type {};

template <typename Func, typename T, typename U>
struct is_binary_predicate<Func, T, U, std::enable_if_t<
        std::is_convertible<
                std::result_of_t<Func&(T, U)>, bool>::value>>
    : std::true_type {};

template <typename Func, typename T, typename U = T>
constexpr bool is_binary_predicate_v = is_binary_predicate<Func, T, U>::value;

template <typename Func, typename T, typename U = T>
constexpr bool is_comparator_v = is_binary_predicate_v<Func, T, U>;

}; // end namespace detail

#define REQUIRES(...) std::enable_if_t<__VA_ARGS__, int> = 0

// Non-modifying sequence operations
template <typename Iter, typename UnaryPredicate,
          REQUIRES(detail::is_input_iterator_v<Iter> &&
                   detail::is_unary_predicate_v<UnaryPredicate, value_type_t<Iter>>)>
bool all_of(Iter first, Iter last, UnaryPredicate pred)
{
    return std::all_of(std::move(first), std::move(last), std::move(pred));
}

template <typename Range, typename UnaryPredicate,
          REQUIRES(detail::is_input_range_v<Range> &&
                   detail::is_unary_predicate_v<UnaryPredicate, range_value_type_t<Range>>)>
bool all_of(Range&& range, UnaryPredicate pred)
{
    return std::all_of(detail::adl_begin(range), detail::adl_end(range), std::move(pred));
}

template <typename Iter, typename UnaryPredicate,
          REQUIRES(detail::is_input_iterator_v<Iter> &&
                   detail::is_unary_predicate_v<UnaryPredicate, value_type_t<Iter>>)>
bool any_of(Iter first, Iter last, UnaryPredicate pred)
{
    return std::any_of(std::move(first), std::move(last), std::move(pred));
}

template <typename Range, typename UnaryPredicate,
          REQUIRES(detail::is_input_range_v<Range> &&
                   detail::is_unary_predicate_v<UnaryPredicate, range_value_type_t<Range>>)>
bool any_of(Range&& range, UnaryPredicate pred)
{
    return std::any_of(detail::adl_begin(range), detail::adl_end(range), std::move(pred));
}

template <typename Iter, typename UnaryPredicate,
          REQUIRES(detail::is_input_iterator_v<Iter> &&
                   detail::is_unary_predicate_v<UnaryPredicate, value_type_t<Iter>>)>
bool none_of(Iter first, Iter last, UnaryPredicate pred)
{
    return std::none_of(std::move(first), std::move(last), std::move(pred));
}

template <typename Range, typename UnaryPredicate,
          REQUIRES(detail::is_input_range_v<Range> &&
                   detail::is_unary_predicate_v<UnaryPredicate, range_value_type_t<Range>>)>
bool none_of(Range&& range, UnaryPredicate pred)
{
    return std::none_of(detail::adl_begin(range), detail::adl_end(range), std::move(pred));
}

template <typename Iter, typename UnaryFunction,
          REQUIRES(detail::is_input_iterator_v<Iter>)>
UnaryFunction for_each(Iter first, Iter last, UnaryFunction func)
{
    return std::for_each(std::move(first), std::move(last), std::move(func));
}

template <typename Range, typename UnaryFunction,
          REQUIRES(detail::is_input_range_v<Range>)>
UnaryFunction for_each(Range&& range, UnaryFunction func)
{
    return std::for_each(detail::adl_begin(range), detail::adl_end(range),
                         std::move(func));
}

template <typename Iter, typename T,
          REQUIRES(detail::is_input_iterator_v<Iter>)>
difference_type_t<Iter>
count(Iter first, Iter last, const T& value)
{
    return std::count(std::move(first), std::move(last), value);
}

template <typename Range, typename T,
          REQUIRES(detail::is_input_range_v<Range>)>
range_difference_type_t<Range>
count(Range&& rng, const T& value)
{
    return std::count(detail::adl_begin(rng), detail::adl_end(rng), value);
}

template <typename Iter, typename UnaryPredicate,
          REQUIRES(detail::is_input_iterator_v<Iter> &&
                   detail::is_unary_predicate_v<UnaryPredicate, value_type_t<Iter>>)>
difference_type_t<Iter>
count_if(Iter first, Iter last, UnaryPredicate pred)
{
    return std::count_if(std::move(first), std::move(last), std::move(pred));
}

template <typename Range, typename UnaryPredicate,
          REQUIRES(detail::is_input_range_v<Range> &&
                   detail::is_unary_predicate_v<UnaryPredicate, range_value_type_t<Range>>)>
range_difference_type_t<Range>
count_if(Range&& range, UnaryPredicate pred)
{
    return std::count_if(detail::adl_begin(range), detail::adl_end(range), std::move(pred));
}

// N.B. The "three-legged" forms of mismatch() are dangerous and should be
// avoided, so we mark them as deprecated
template <typename Iter1, typename Iter2, typename BinaryPredicate = std::equal_to<>,
          REQUIRES(detail::is_input_iterator_v<Iter1> &&
                   detail::is_input_iterator_v<Iter2> &&
                   detail::is_binary_predicate_v<BinaryPredicate, value_type_t<Iter1>, value_type_t<Iter2>>)>
TCB_RANGES_DEPRECATED
std::pair<Iter1, Iter2>
mismatch(Iter1 first1, Iter2 last1, Iter2 first2, BinaryPredicate pred = {})
{
    return std::mismatch(std::move(first1), std::move(last1), std::move(first2), std::move(pred));
}

template <typename Iter1, typename Iter2, typename BinaryPredicate = std::equal_to<>,
        REQUIRES(detail::is_input_iterator_v<Iter1> &&
                 detail::is_input_iterator_v<Iter2> &&
                 detail::is_binary_predicate_v<BinaryPredicate, value_type_t<Iter1>, value_type_t<Iter2>>)>
std::pair<Iter1, Iter2>
mismatch(Iter1 first1, Iter1 last1, Iter2 first2, Iter2 last2, BinaryPredicate pred = {})
{
    return std::mismatch(std::move(first1), std::move(last1),
                         std::move(first2), std::move(last2),
                         std::move(pred));
}

template <typename Range1, typename Range2, typename BinaryPredicate = std::equal_to<>,
        REQUIRES(detail::is_input_range_v<Range1> &&
                 detail::is_input_range_v<Range2> &&
                 detail::is_binary_predicate_v<BinaryPredicate, range_value_type_t<Range1>, range_value_type_t<Range2>>)>
std::pair<iterator_t<Range1>, iterator_t<Range2>>
mismatch(Range1&& range1, Range2&& range2, BinaryPredicate pred = {})
{
    static_assert(!std::is_rvalue_reference<Range1&&>::value &&
                          !std::is_rvalue_reference<Range2&&>::value,
                  "lvalue ranges required for mismatch()");
    return std::mismatch(detail::adl_begin(range1), detail::adl_end(range1),
                         detail::adl_begin(range2), detail::adl_end(range2),
                         std::move(pred));
}

// Again, the three-legged form of equal() is discouraged
template <typename Iter1, typename Iter2, typename BinaryPredicate = std::equal_to<>,
        REQUIRES(detail::is_input_iterator_v<Iter1> &&
                 detail::is_input_iterator_v<Iter2> &&
                 detail::is_binary_predicate_v<BinaryPredicate, value_type_t<Iter1>, value_type_t<Iter2>>)>
TCB_RANGES_DEPRECATED
bool equal(Iter1 first1, Iter1 last1, Iter2 first2, BinaryPredicate pred = {})
{
    return std::equal(std::move(first1), std::move(last1),
                      std::move(first2), std::move(pred));
}

template <typename Iter1, typename Iter2, typename BinaryPredicate = std::equal_to<>,
        REQUIRES(detail::is_input_iterator_v<Iter1> &&
                 detail::is_input_iterator_v<Iter2> &&
                 detail::is_binary_predicate_v<BinaryPredicate, value_type_t<Iter1>, value_type_t<Iter2>>)>
bool equal(Iter1 first1, Iter1 last1, Iter2 first2, Iter2 last2, BinaryPredicate pred = {})
{
    return std::equal(std::move(first1), std::move(last1),
                      std::move(first2), std::move(last2),
                      std::move(pred));
}

template <typename Range1, typename Range2, typename BinaryPredicate = std::equal_to<>,
        REQUIRES(detail::is_input_range_v<Range1> &&
                 detail::is_input_range_v<Range2> &&
                  detail::is_binary_predicate_v<BinaryPredicate, range_value_type_t<Range1>, range_value_type_t<Range2>>)>
bool equal(Range1&& range1, Range2&& range2, BinaryPredicate pred = {})
{
    return std::equal(detail::adl_begin(range1), detail::adl_end(range1),
                      detail::adl_begin(range2), detail::adl_end(range2),
                      std::move(pred));
}

template <typename Iter, typename T,
          REQUIRES(detail::is_input_iterator_v<Iter>)>
Iter find(Iter first, Iter last, const T& value)
{
    return std::find(std::move(first), std::move(last), value);
}

template <typename Range, typename T,
          REQUIRES(detail::is_input_range_v<Range>)>
iterator_t<Range>
find(Range&& range, const T& value)
{
    static_assert(!std::is_rvalue_reference<Range&&>::value,
                  "lvalue range required for find()");
    return std::find(detail::adl_begin(range), detail::adl_end(range), value);
}

template <typename Iter, typename UnaryPredicate,
          REQUIRES(detail::is_input_iterator_v<Iter> &&
                   detail::is_unary_predicate_v<UnaryPredicate, value_type_t<Iter>>)>
Iter find_if(Iter first, Iter last, UnaryPredicate pred)
{
    return std::find_if(std::move(first), std::move(last), std::move(pred));
}

template <typename Range, typename UnaryPredicate,
          REQUIRES(detail::is_input_range_v<Range> &&
                   detail::is_unary_predicate_v<UnaryPredicate, range_value_type_t<Range>>)>
iterator_t<Range>
find_if(Range&& range, UnaryPredicate pred)
{
    static_assert(!std::is_rvalue_reference<Range&&>::value,
                  "lvalue range required as argument to find_if()");
    return std::find_if(detail::adl_begin(range), detail::adl_end(range),
                        std::move(pred));
}

template <typename Iter, typename UnaryPredicate,
          REQUIRES(detail::is_input_iterator_v<Iter> &&
                   detail::is_unary_predicate_v<UnaryPredicate, value_type_t<Iter>>)>
Iter find_if_not(Iter first, Iter last, UnaryPredicate&& pred)
{
    return std::find_if_not(std::move(first), std::move(last), std::move(pred));
}

template <typename Range, typename UnaryPredicate,
          REQUIRES(detail::is_input_range_v<Range> &&
                   detail::is_unary_predicate_v<UnaryPredicate, range_value_type_t<Range>>)>
iterator_t<Range>
find_if_not(Range&& range, UnaryPredicate pred)
{
    static_assert(!std::is_rvalue_reference<Range&&>::value,
                  "lvalue range required as argument to find_if_not()");
    return std::find_if_not(detail::adl_begin(range), detail::adl_end(range),
                            std::move(pred));
}

template <typename Iter1, typename Iter2, typename BinaryPredicate = std::equal_to<>,
        REQUIRES(detail::is_forward_iterator_v<Iter1> &&
                 detail::is_forward_iterator_v<Iter2> &&
                 detail::is_binary_predicate_v<BinaryPredicate, value_type_t<Iter1>, value_type_t<Iter2>>)>
Iter1 find_end(Iter1 first1, Iter1 last1, Iter2 first2, Iter2 last2, BinaryPredicate pred = {})
{
    return std::find_end(std::move(first1), std::move(last1),
                         std::move(first2), std::move(last2),
                         std::move(pred));
}

template <typename Range1, typename Range2, typename BinaryPredicate = std::equal_to<>,
        REQUIRES(detail::is_forward_range_v<Range1> &&
                 detail::is_forward_range_v<Range2> &&
                 detail::is_binary_predicate_v<BinaryPredicate, range_value_type_t<Range1>, range_value_type_t<Range2>>)>
iterator_t<Range1> find_end(Range1&& range1, Range2&& range2, BinaryPredicate pred = {})
{
    static_assert(!std::is_rvalue_reference<Range1&&>::value,
                  "lvalue range required as first argument to find_end()");
    return std::find_end(detail::adl_begin(range1), detail::adl_end(range1),
                         detail::adl_begin(range2), detail::adl_end(range2),
                         std::move(pred));
}

template <typename Iter1, typename Iter2, typename Pred = std::equal_to<>,
        REQUIRES(detail::is_input_iterator_v<Iter1> &&
                 detail::is_forward_iterator_v<Iter2> &&
                 detail::is_binary_predicate_v<Pred, value_type_t<Iter1>, value_type_t<Iter2>>)>
Iter1 find_first_of(Iter1 first1, Iter1 last1, Iter2 first2, Iter2 last2, Pred pred = {})
{
    return std::find_first_of(std::move(first1), std::move(last1),
                              std::move(first2), std::move(last2),
                              std::move(pred));
}

template <typename Range1, typename Range2, typename Pred = std::equal_to<>,
        REQUIRES(detail::is_input_range_v<Range1> &&
                         detail::is_input_range_v<Range2> &&
                 detail::is_binary_predicate_v<Pred, range_value_type_t<Range1>, range_value_type_t<Range2>>)>
iterator_t<Range1> find_first_of(Range1&& range1, Range2&& range2, Pred pred = {})
{
    static_assert(!std::is_rvalue_reference<Range1&&>::value,
                  "lvalue range required as first argument to find_first_of()");
    return std::find_first_of(detail::adl_begin(range1), detail::adl_end(range1),
                              detail::adl_begin(range2), detail::adl_end(range2),
                              std::move(pred));
}

template <typename Iter, typename Pred = std::equal_to<>,
        REQUIRES(detail::is_forward_iterator_v<Iter> &&
                 detail::is_binary_predicate_v<Pred, value_type_t<Iter>, value_type_t<Iter>>)>
Iter adjacent_find(Iter first, Iter last, Pred pred = {})
{
    return std::adjacent_find(std::move(first), std::move(last), std::forward<Pred>(pred));
}

template <typename Range, typename Pred = std::equal_to<>,
        REQUIRES(detail::is_forward_range_v<Range> &&
                 detail::is_binary_predicate_v<Pred, range_value_type_t<Range>, range_value_type_t<Range>>)>
iterator_t<Range> adjacent_find(Range&& range, Pred pred = {})
{
    static_assert(!std::is_rvalue_reference<Range&&>::value,
                  "lvalue range required as argument to adjacent_find()");
    return std::adjacent_find(detail::adl_begin(range), detail::adl_end(range),
                              std::move(pred));
};

template <typename Iter1, typename Iter2, typename Pred = std::equal_to<>,
        REQUIRES(detail::is_forward_iterator_v<Iter1> &&
                 detail::is_forward_iterator_v<Iter2> &&
                 detail::is_binary_predicate_v<Pred, value_type_t<Iter1>, value_type_t<Iter2>>)>
Iter1 search(Iter1 first1, Iter1 last1, Iter2 first2, Iter2 last2, Pred pred = {})
{
    return std::search(std::move(first1), std::move(last1),
                       std::move(first2), std::move(last2), std::move(pred));
}

template <typename Range1, typename Range2, typename Pred = std::equal_to<>,
        REQUIRES(detail::is_forward_range_v<Range1> &&
                         detail::is_forward_range_v<Range2> &&
                 detail::is_binary_predicate_v<Pred, range_value_type_t<Range1>, range_value_type_t<Range2>>)>
iterator_t<Range1> search(Range1&& range1, Range2&& range2, Pred pred = {})
{
    static_assert(!std::is_rvalue_reference<Range1&&>::value,
                  "lvalue range required as first argument to search()");
    return std::search(detail::adl_begin(range1), detail::adl_end(range1),
                       detail::adl_begin(range2), detail::adl_end(range2),
                       std::move(pred));
}

template <typename Iter, typename T, typename Pred = std::equal_to<>,
        REQUIRES(detail::is_forward_iterator_v<Iter> &&
                 detail::is_binary_predicate_v<Pred, value_type_t<Iter>, value_type_t<Iter>>)>
Iter search_n(Iter first, Iter last, difference_type_t<Iter> count, const T& value, Pred pred = {})
{
    return std::search_n(std::move(first), std::move(last), count, value, std::move(pred));
}

template <typename Range, typename T, typename Pred = std::equal_to<>,
        REQUIRES(detail::is_forward_range_v<Range> &&
                 detail::is_binary_predicate_v<Pred, range_value_type_t<Range>, range_value_type_t<Range>>)>
iterator_t<Range>
search_n(Range&& range, difference_type_t<iterator_t<Range>> count, const T& value, Pred pred = {})
{
    static_assert(!std::is_rvalue_reference<Range&&>::value,
                  "lvalue range required as argument to search_n()");
    return std::search_n(detail::adl_begin(range), detail::adl_end(range), count, value, std::move(pred));
}


/*
 * Modifying sequence algorithms
 */

template <typename Iter1, typename Iter2,
          REQUIRES(detail::is_input_iterator_v<Iter1> &&
                   detail::is_output_iterator_v<Iter2, value_type_t<Iter1>>)>
Iter2 copy(Iter1 first, Iter1 last, Iter2 ofirst)
{
    return std::copy(std::move(first), std::move(last), std::move(ofirst));
}

template <typename Range1, typename Iter2,
          REQUIRES(detail::is_input_range_v<Range1> &&
                   detail::is_output_iterator_v<Iter2, range_value_type_t<Range1>>)>
Iter2 copy(Range1&& range, Iter2 ofirst)
{
    return std::copy(detail::adl_begin(range), detail::adl_end(range), std::move(ofirst));
}

template <typename Iter1, typename Iter2, typename Pred,
          REQUIRES(detail::is_input_iterator_v<Iter1> &&
                   detail::is_output_iterator_v<Iter2, value_type_t<Iter1>> &&
                   detail::is_unary_predicate_v<Pred, value_type_t<Iter1>>)>
Iter2 copy_if(Iter1 first, Iter1 last, Iter2 ofirst, Pred pred)
{
    return std::copy_if(std::move(first), std::move(last), std::move(ofirst), std::move(pred));
}

template <typename Range1, typename Iter2, typename Pred,
          REQUIRES(detail::is_input_range_v<Range1> &&
                   detail::is_output_iterator_v<Iter2, range_value_type_t<Range1>> &&
                   detail::is_unary_predicate_v<Pred, range_value_type_t<Range1>>)>
Iter2 copy_if(Range1&& range, Iter2 ofirst, Pred&& pred)
{
    return std::copy_if(detail::adl_begin(range), detail::adl_end(range), std::move(ofirst),
                        std::move(pred));
}

template <typename Iter1, typename Iter2,
          REQUIRES(detail::is_input_iterator_v<Iter1> &&
                   detail::is_output_iterator_v<Iter2, value_type_t<Iter1>>)>
Iter2 copy_n(Iter1 first, difference_type_t<Iter1> count, Iter2 ofirst)
{
    return std::copy_n(std::move(first), count, std::move(ofirst));
}

template <typename Iter1, typename Iter2,
          REQUIRES(detail::is_bidirectional_iterator_v<Iter1> &&
                   detail::is_bidirectional_iterator_v<Iter2> &&
                   detail::is_writable_v<Iter2, value_type_t<Iter1>>)>
Iter2 copy_backward(Iter1 first, Iter1 last, Iter2 olast)
{
    return std::copy_backward(std::move(first), std::move(last), std::move(olast));
}

template <typename Range1, typename Iter2,
          REQUIRES(detail::is_bidirectional_range_v<Range1> &&
                   detail::is_bidirectional_iterator_v<Iter2> &&
                   detail::is_writable_v<Iter2, range_value_type_t<Range1>>)>
Iter2 copy_backward(Range1&& range, Iter2 olast)
{
    return std::copy_backward(detail::adl_begin(range), detail::adl_end(range), std::move(olast));
}

template <typename Iter1, typename Iter2,
          REQUIRES(detail::is_input_iterator_v<Iter1> &&
                   detail::is_output_iterator_v<Iter2, value_type_t<Iter1>>)>
Iter2 move(Iter1 first, Iter1 last, Iter2 ofirst)
{
    return std::move(std::move(first), std::move(last), std::move(ofirst));
}

template <typename Range1, typename Iter2,
           REQUIRES(detail::is_input_range_v<Range1> &&
                    detail::is_output_iterator_v<Iter2, range_value_type_t<Range1>>)>
Iter2 move(Range1&& range, Iter2 ofirst)
{
    return std::move(detail::adl_begin(range), detail::adl_end(range), std::move(ofirst));
}

template <typename Iter1, typename Iter2,
          REQUIRES(detail::is_bidirectional_iterator_v<Iter1> &&
                   detail::is_bidirectional_iterator_v<Iter2> &&
                   detail::is_writable_v<Iter2, value_type_t<Iter1>>)>
Iter2 move_backward(Iter1 first, Iter1 last, Iter2 olast)
{
    return std::move_backward(std::move(first), std::move(last), std::move(olast));
}

template <typename Range1, typename Iter2,
          REQUIRES(detail::is_bidirectional_range_v<Range1> &&
                   detail::is_bidirectional_iterator_v<Iter2> &&
                   detail::is_writable_v<Iter2, range_value_type_t<Range1>>)>
Iter2 move_backward(Range1&& range, Iter2 olast)
{
    return std::move_backward(detail::adl_begin(range), detail::adl_end(range), std::move(olast));
}

template <typename Iter, typename T,
          REQUIRES(detail::is_forward_iterator_v<Iter> &&
                   detail::is_writable_v<Iter, const T&>)>
void fill(Iter first, Iter last, const T& value)
{
    std::fill(std::move(first), std::move(last), value);
}

template <typename Range, typename T,
          REQUIRES(detail::is_forward_range_v<Range> &&
                   detail::is_writable_v<iterator_t<Range>, const T&>)>
void fill(Range&& range, const T& value)
{
    std::fill(detail::adl_begin(range), detail::adl_end(range), value);
}

template <typename Iter, typename T,
          REQUIRES(detail::is_output_iterator_v<Iter, const T&>)>
Iter fill_n(Iter first, difference_type_t<Iter> count, const T& value)
{
    return std::fill_n(std::move(first), count, value);
}

template <typename Iter1, typename Iter2, typename UnaryOp,
          REQUIRES(detail::is_input_iterator_v<Iter1> &&
                   detail::is_output_iterator_v<Iter2, std::result_of_t<UnaryOp(value_type_t<Iter1>)>>)>
Iter2 transform(Iter1 first, Iter1 last, Iter2 ofirst, UnaryOp op)
{
    return std::transform(std::move(first), std::move(last), std::move(ofirst), std::move(op));
}

template <typename Range1, typename Iter2, typename UnaryOp,
          REQUIRES(detail::is_input_range_v<Range1> &&
                   detail::is_output_iterator_v<Iter2, std::result_of_t<UnaryOp(range_value_type_t<Range1>)>>)>
Iter2 transform(Range1&& range, Iter2 ofirst, UnaryOp op)
{
    return std::transform(detail::adl_begin(range), detail::adl_end(range),
                          std::move(ofirst), std::move(op));
}

template <typename Iter1, typename Iter2, typename Iter3, typename BinOp,
          REQUIRES(detail::is_input_iterator_v<Iter1> &&
                   detail::is_input_iterator_v<Iter2> &&
                   detail::is_output_iterator_v<Iter3, std::result_of_t<BinOp(value_type_t<Iter1>, value_type_t<Iter2>)>>)>
TCB_RANGES_DEPRECATED
Iter3 transform(Iter1 first1, Iter1 last1, Iter2 first2, Iter3 ofirst, BinOp op)
{
    return std::transform(std::move(first1), std::move(last1),
                          std::move(first2), std::move(ofirst), std::move(op));
}

// Just for the hell of it, let's do a 6-parameter overload too
template <typename Iter1, typename Iter2, typename Iter3, typename BinOp,
        REQUIRES(detail::is_input_iterator_v<Iter1> &&
                         detail::is_input_iterator_v<Iter2> &&
                         detail::is_output_iterator_v<Iter3, std::result_of_t<BinOp(value_type_t<Iter1>, value_type_t<Iter2>)>>)>
Iter3 transform(Iter1 first1, Iter1 last1, Iter2 first2, Iter2 last2, Iter3 ofirst, BinOp op)
{
    while (first1 != last1 && first2 != last2) {
        *ofirst = op(*first1, *first2);
        ++ofirst; ++first1; ++first2;
    }
    return std::move(ofirst);
}

template <typename Range1, typename Range2, typename Iter3, typename BinOp,
        REQUIRES(detail::is_input_range_v<Range1> &&
                         detail::is_input_range_v<Range2> &&
                         detail::is_output_iterator_v<Iter3, std::result_of_t<BinOp(range_value_type_t<Range1>, range_value_type_t<Range2>)>>)>
Iter3 transform(Range1&& range1, Range2&& range2, Iter3 ofirst, BinOp op)
{
    return tcb::ranges::transform(detail::adl_begin(range1), detail::adl_end(range1),
                                  detail::adl_begin(range2), detail::adl_end(range2),
                                  std::move(ofirst), std::move(op));
}

template <typename Iter, typename Generator,
          REQUIRES(detail::is_forward_iterator_v<Iter> &&
                   detail::is_writable_v<Iter, std::result_of_t<Generator()>>)>
void generate(Iter first, Iter last, Generator gen)
{
    std::generate(std::move(first), std::move(last), std::move(gen));
}

template <typename Range, typename Generator,
          REQUIRES(detail::is_forward_range_v<Range> &&
                   detail::is_writable_v<iterator_t<Range>, std::result_of_t<Generator()>>)>
void generate(Range&& range, Generator gen)
{
    std::generate(detail::adl_begin(range), detail::adl_end(range), std::move(gen));
}

template <typename Iter, typename Generator,
          REQUIRES(detail::is_forward_iterator_v<Iter> &&
                   detail::is_writable_v<Iter, std::result_of_t<Generator()>>)>
Iter generate_n(Iter first, difference_type_t<Iter> count, Generator generator)
{
    return std::generate_n(std::move(first), count, std::move(generator));
}

template <typename Iter, typename T,
          REQUIRES(detail::is_forward_iterator_v<Iter>)>
Iter remove(Iter first, Iter last, const T& value)
{
    return std::remove(std::move(first), std::move(last), value);
}

template <typename Range, typename T,
          REQUIRES(detail::is_forward_range_v<Range>)>
iterator_t<Range> remove(Range&& range, const T& value)
{
    static_assert(!std::is_rvalue_reference<Range&&>::value,
                  "lvalue range required as argument to remove()");
    return std::remove(detail::adl_begin(range), detail::adl_end(range), value);
}

template <typename Iter, typename Pred,
          REQUIRES(detail::is_forward_iterator_v<Iter> &&
                   detail::is_unary_predicate_v<Pred, value_type_t<Iter>>)>
Iter remove_if(Iter first, Iter last, Pred pred)
{
    return std::remove_if(std::move(first), std::move(last), std::move(pred));
}

template <typename Range, typename Pred,
          REQUIRES(detail::is_forward_range_v<Range> &&
                   detail::is_unary_predicate_v<Pred, range_value_type_t<Range>>)>
iterator_t<Range> remove_if(Range&& range, Pred pred)
{
    static_assert(!std::is_rvalue_reference<Range&&>::value,
                  "lvalue range required as argument to remove_if()");
    return std::remove_if(detail::adl_begin(range), detail::adl_end(range), std::move(pred));
}

template <typename Iter1, typename Iter2, typename T,
          REQUIRES(detail::is_forward_iterator_v<Iter1> &&
                   detail::is_output_iterator_v<Iter2, value_type_t<Iter1>>)>
Iter2 remove_copy(Iter1 first, Iter1 last, Iter2 ofirst, const T& value)
{
    return std::remove_copy(std::move(first), std::move(last),
                            std::move(ofirst), value);
}

template <typename Range1, typename Iter2, typename T,
        REQUIRES(detail::is_forward_range_v<Range1> &&
                 detail::is_output_iterator_v<Iter2, range_value_type_t<Range1>>)>
Iter2 remove_copy(Range1&& range, Iter2 ofirst, const T& value)
{
    return std::remove_copy(detail::adl_begin(range), detail::adl_end(range),
                            std::move(ofirst), value);
}

template <typename Iter1, typename Iter2, typename Pred,
          REQUIRES(detail::is_forward_iterator_v<Iter1> &&
                   detail::is_output_iterator_v<Iter2, value_type_t<Iter1>> &&
                   detail::is_unary_predicate_v<Pred, value_type_t<Iter1>>)>
Iter2 remove_copy_if(Iter1 first, Iter1 last, Iter2 ofirst, Pred pred)
{
    return std::remove_copy_if(std::move(first), std::move(last),
                               std::move(ofirst), std::move(pred));
}

template <typename Range1, typename Iter2, typename Pred,
          REQUIRES(detail::is_forward_range_v<Range1> &&
                   detail::is_output_iterator_v<Iter2, range_value_type_t<Range1>> &&
                   detail::is_unary_predicate_v<Pred, range_value_type_t<Range1>>) >
Iter2 remove_copy_if(Range1&& range, Iter2 ofirst, Pred pred)
{
    return std::remove_copy_if(detail::adl_begin(range), detail::adl_end(range),
                               std::move(ofirst), std::move(pred));
}

template <typename Iter, typename T,
          REQUIRES(detail::is_forward_iterator_v<Iter> &&
                   detail::is_writable_v<Iter, const T&>)>
void replace(Iter first, Iter last, const T& old_value, const T& new_value)
{
    std::replace(std::move(first), std::move(last), old_value, new_value);
}

template <typename Range, typename T,
          REQUIRES(detail::is_forward_range_v<Range> &&
                   detail::is_writable_v<iterator_t<Range>, const T&>)>
void replace(Range&& range, const T& old_value, const T& new_value)
{
    std::replace(detail::adl_begin(range), detail::adl_end(range),
                 old_value, new_value);
}

template <typename Iter, typename Pred, typename T,
          REQUIRES(detail::is_forward_iterator_v<Iter> &&
                   detail::is_writable_v<Iter, const T&> &&
                   detail::is_unary_predicate_v<Pred, value_type_t<Iter>>)>
void replace_if(Iter first, Iter last, Pred pred, const T& new_value)
{
    std::replace_if(std::move(first), std::move(last), std::move(pred), new_value);
}

template <typename Range, typename Pred, typename T,
          REQUIRES(detail::is_forward_range_v<Range> &&
                   detail::is_writable_v<iterator_t<Range>, const T&> &&
                   detail::is_unary_predicate_v<Pred, range_value_type_t<Range>>)>
void replace_if(Range&& range, Pred pred, const T& new_value)
{
    std::replace_if(detail::adl_begin(range), detail::adl_end(range),
                    std::move(pred), new_value);
}

template <typename Iter1, typename Iter2, typename T,
          REQUIRES(detail::is_input_iterator_v<Iter1> &&
                   detail::is_output_iterator_v<Iter2, const T&>)>
Iter2 replace_copy(Iter1 first, Iter1 last, Iter2 ofirst, const T& old_value, const T& new_value)
{
    return std::replace_copy(std::move(first), std::move(last), std::move(ofirst),
                             old_value, new_value);
}

template <typename Range1, typename Iter2, typename T,
          REQUIRES(detail::is_input_range_v<Range1> &&
                   detail::is_output_iterator_v<Iter2, const T&>)>
Iter2 replace_copy(Range1&& range, Iter2 ofirst, const T& old_value, const T& new_value)
{
    return std::replace_copy(detail::adl_begin(range), detail::adl_end(range),
                             std::move(ofirst), old_value, new_value);
}

template <typename Iter1, typename Iter2, typename Pred, typename T,
          REQUIRES(detail::is_input_iterator_v<Iter1> &&
                   detail::is_output_iterator_v<Iter2, const T&> &&
                   detail::is_unary_predicate_v<Pred, value_type_t<Iter1>>)>
Iter2 replace_copy_if(Iter1 first, Iter1 last, Iter2 ofirst, Pred pred, const T& new_value)
{
    return std::replace_copy_if(std::move(first), std::move(last), std::move(ofirst),
                                std::move(pred), new_value);
}

template <typename Range1, typename Iter2, typename Pred, typename T,
          REQUIRES(detail::is_input_range_v<Range1> &&
                   detail::is_output_iterator_v<Iter2, const T&> &&
                   detail::is_unary_predicate_v<Pred, range_value_type_t<Range1>>)>
Iter2 replace_copy_if(Range1&& range, Iter2 ofirst, Pred pred, const T& new_value)
{
    return std::replace_copy_if(detail::adl_begin(range), detail::adl_end(range),
                                std::move(ofirst), std::move(pred), new_value);
}

template <typename Iter1, typename Iter2,
          REQUIRES(detail::is_forward_iterator_v<Iter1> &&
                   detail::is_forward_iterator_v<Iter2>)>
TCB_RANGES_DEPRECATED
Iter2 swap_ranges(Iter1 first1, Iter1 last1, Iter2 first2)
{
    return std::swap_ranges(std::move(first1), std::move(last1), std::move(first2));
}

template <typename Iter1, typename Iter2,
        REQUIRES(detail::is_forward_iterator_v<Iter1> &&
                 detail::is_forward_iterator_v<Iter2>)>
std::pair<Iter1, Iter2>
swap_ranges(Iter1 first1, Iter1 last1, Iter2 first2, Iter2 last2)
{
    while (first1 != last1 && first2 != last2) {
        std::iter_swap(first1, first2);
        ++first1; ++first2;
    }
    return std::make_pair(std::move(first1), std::move(first2));
}

template <typename Range1, typename Range2,
          REQUIRES(detail::is_forward_range_v<Range1> &&
                   detail::is_forward_range_v<Range2>)>
std::pair<iterator_t<Range1>, iterator_t<Range2>>
swap_ranges(Range1&& range1, Range2&& range2)
{
    static_assert(!std::is_rvalue_reference<Range1&&>::value &&
                  !std::is_rvalue_reference<Range2&&>::value,
                 "lvalue arguments required for swap_ranges()");
    return ranges::swap_ranges(detail::adl_begin(range1), detail::adl_end(range1),
                               detail::adl_begin(range2), detail::adl_end(range2));
}

template <typename Iter,
          REQUIRES(detail::is_bidirectional_iterator_v<Iter>)>
void reverse(Iter first, Iter last)
{
    std::reverse(std::move(first), std::move(last));
}

template <typename Range,
          REQUIRES(detail::is_bidirectional_range_v<Range>)>
void reverse(Range&& range)
{
    std::reverse(detail::adl_begin(range), detail::adl_end(range));
}

template <typename Iter1, typename Iter2,
          REQUIRES(detail::is_bidirectional_iterator_v<Iter1> &&
                   detail::is_output_iterator_v<Iter2, value_type_t<Iter1>>)>
Iter2 reverse_copy(Iter1 first, Iter1 last, Iter2 ofirst)
{
    return std::reverse_copy(std::move(first), std::move(last), std::move(ofirst));
}

template <typename Range1, typename Iter2,
          REQUIRES(detail::is_bidirectional_range_v<Range1> &&
                   detail::is_output_iterator_v<Iter2, range_value_type_t<Range1>>)>
Iter2 reverse_copy(Range1&& range, Iter2 ofirst)
{
    return std::reverse_copy(detail::adl_begin(range), detail::adl_end(range),
                             std::move(ofirst));
}

template <typename Iter,
          REQUIRES(detail::is_forward_iterator_v<Iter>)>
void rotate(Iter first, Iter middle, Iter last)
{
    std::rotate(std::move(first), std::move(middle), std::move(last));
}

template <typename Range,
          REQUIRES(detail::is_forward_range_v<Range>)>
void rotate(Range&& range, iterator_t<Range> middle)
{
    std::rotate(detail::adl_begin(range), std::move(middle), detail::adl_end(range));
}

template <typename Iter1, typename Iter2,
          REQUIRES(detail::is_forward_iterator_v<Iter1> &&
                   detail::is_output_iterator_v<Iter2, value_type_t<Iter1>>)>
Iter2 rotate_copy(Iter1 first, Iter1 middle, Iter1 last, Iter2 ofirst)
{
    return std::rotate_copy(std::move(first), std::move(middle),
                            std::move(last), std::move(ofirst));
}

template <typename Range1, typename Iter2,
        REQUIRES(detail::is_forward_range_v<Range1> &&
                 detail::is_output_iterator_v<Iter2, range_value_type_t<Range1>>)>
Iter2 rotate_copy(Range1&& range, iterator_t<Range1> middle, Iter2 ofirst)
{
    return std::rotate_copy(detail::adl_begin(range), std::move(middle),
                            detail::adl_end(range), std::move(ofirst));
}

template <typename Iter, typename URBG,
          REQUIRES(detail::is_random_access_iterator_v<Iter>)>
void shuffle(Iter first, Iter last, URBG&& generator)
{
    std::shuffle(std::move(first), std::move(last), std::forward<URBG>(generator));
}

template <typename Range, typename URBG,
          REQUIRES(detail::is_random_access_range_v<Range>)>
void shuffle(Range&& range, URBG&& generator)
{
    std::shuffle(detail::adl_begin(range), detail::adl_end(range), std::forward<URBG>(generator));
}

template <typename Iter, typename Pred = std::equal_to<>,
          REQUIRES(detail::is_forward_iterator_v<Iter> &&
                   detail::is_binary_predicate_v<Pred, value_type_t<Iter>, value_type_t<Iter>>)>
Iter unique(Iter first, Iter last, Pred pred = {})
{
    return std::unique(std::move(first), std::move(last), std::move(pred));
}

template <typename Range, typename Pred = std::equal_to<>,
          REQUIRES(detail::is_forward_range_v<Range> &&
                   detail::is_binary_predicate_v<Pred, range_value_type_t<Range>, range_value_type_t<Range>>)>
iterator_t<Range> unique(Range&& range, Pred pred = {})
{
    static_assert(!std::is_rvalue_reference<Range&&>::value,
                  "lvalue range required as argument to unique()");
    return std::unique(detail::adl_begin(range), detail::adl_end(range), std::move(pred));
}

template <typename Iter1, typename Iter2, typename Pred = std::equal_to<>,
        REQUIRES(detail::is_input_iterator_v<Iter1> &&
                 detail::is_output_iterator_v<Iter2, value_type_t<Iter1>> &&
                 detail::is_binary_predicate_v<Pred, value_type_t<Iter1>, value_type_t<Iter1>>)>
Iter2 unique_copy(Iter1 first, Iter1 last, Iter2 ofirst, Pred pred = {})
{
    return std::unique_copy(std::move(first), std::move(last), std::move(ofirst), std::move(pred));
}

template <typename Range1, typename Iter2, typename Pred = std::equal_to<>,
        REQUIRES(detail::is_input_range_v<Range1> &&
                 detail::is_output_iterator_v<Iter2, range_value_type_t<Range1>> &&
                 detail::is_binary_predicate_v<Pred, range_value_type_t<Range1>, range_value_type_t<Range1>>)>
Iter2 unique_copy(Range1&& range, Iter2 ofirst, Pred pred = {})
{
    return std::unique_copy(detail::adl_begin(range), detail::adl_end(range), std::move(ofirst), std::move(pred));
}

/*
 * Partitioning Operations
 */

template <typename Iter, typename Pred,
          REQUIRES(detail::is_input_iterator_v<Iter> &&
                   detail::is_unary_predicate_v<Pred, value_type_t<Iter>>)>
bool is_partitioned(Iter first, Iter last, Pred pred)
{
    return std::is_partitioned(std::move(first), std::move(last), std::move(pred));
}

template <typename Range, typename Pred,
          REQUIRES(detail::is_input_range_v<Range> &&
                   detail::is_unary_predicate_v<Pred, range_value_type_t<Range>>)>
bool is_partitioned(Range&& range, Pred pred)
{
    return std::is_partitioned(detail::adl_begin(range), detail::adl_end(range), std::move(pred));
}

template <typename Iter, typename Pred,
          REQUIRES(detail::is_forward_iterator_v<Iter> &&
                   detail::is_unary_predicate_v<Pred, value_type_t<Iter>>)>
Iter partition(Iter first, Iter last, Pred pred)
{
    return std::partition(std::move(first), std::move(last), std::move(pred));
}

template <typename Range, typename Pred,
          REQUIRES(detail::is_forward_range_v<Range> &&
                   detail::is_unary_predicate_v<Pred, range_value_type_t<Range>>)>
iterator_t<Range> partition(Range&& range, Pred pred)
{
    static_assert(!std::is_rvalue_reference<Range&&>::value,
                  "lvalue range required as argument to partition()");
    return std::partition(detail::adl_begin(range), detail::adl_end(range), std::move(pred));
}


template <typename Iter1, typename Iter2, typename Iter3, typename Pred,
          REQUIRES(detail::is_input_iterator_v<Iter1> &&
                   detail::is_output_iterator_v<Iter2, value_type_t<Iter1>> &&
                   detail::is_output_iterator_v<Iter3, value_type_t<Iter1>> &&
                   detail::is_unary_predicate_v<Pred, value_type_t<Iter1>>)>
std::pair<Iter2, Iter3>
partition_copy(Iter1 first, Iter1 last, Iter2 otrue, Iter3 ofalse, Pred pred)
{
    return std::partition_copy(std::move(first), std::move(last),
                               std::move(otrue), std::move(ofalse), std::move(pred));
}

template <typename Range1, typename Iter2, typename Iter3, typename Pred,
        REQUIRES(detail::is_input_range_v<Range1> &&
                         detail::is_output_iterator_v<Iter2, range_value_type_t<Range1>> &&
                         detail::is_output_iterator_v<Iter3, range_value_type_t<Range1>> &&
                         detail::is_unary_predicate_v<Pred, range_value_type_t<Range1>>)>
std::pair<Iter2, Iter3>
partition_copy(Range1&& range, Iter2 otrue, Iter3 ofalse, Pred pred)
{
    return std::partition_copy(detail::adl_begin(range), detail::adl_end(range),
                               std::move(otrue), std::move(ofalse), std::move(pred));
}


template <typename Iter, typename Pred,
          REQUIRES(detail::is_bidirectional_iterator_v<Iter> &&
                   detail::is_unary_predicate_v<Pred, value_type_t<Iter>>)>
Iter stable_partition(Iter first, Iter last, Pred pred)
{
    return std::stable_partition(std::move(first), std::move(last), std::move(pred));
}

template <typename Range, typename Pred,
        REQUIRES(detail::is_bidirectional_range_v<Range> &&
                 detail::is_unary_predicate_v<Pred, range_value_type_t<Range>>)>
iterator_t<Range> stable_partition(Range&& range, Pred pred)
{
    static_assert(!std::is_rvalue_reference<Range&&>::value,
                   "lvalue range required as argument to stable_partition()");
    return std::stable_partition(detail::adl_begin(range), detail::adl_end(range), std::move(pred));
}

template <typename Iter, typename Pred,
          REQUIRES(detail::is_forward_iterator_v<Iter> &&
                   detail::is_unary_predicate_v<Pred, value_type_t<Iter>>)>
Iter partition_point(Iter first, Iter last, Pred pred)
{
    return std::partition_point(std::move(first), std::move(last), std::move(pred));
}

template <typename Range, typename Pred,
        REQUIRES(detail::is_forward_range_v<Range> &&
                         detail::is_unary_predicate_v<Pred, range_value_type_t<Range>>)>
iterator_t<Range> partition_point(Range&& range, Pred pred)
{
    static_assert(!std::is_rvalue_reference<Range&&>::value,
                  "lvalue argument required for partition_point()");
    return std::partition_point(detail::adl_begin(range), detail::adl_end(range), std::move(pred));
}

/*
 * Sorting operations
 */

template <typename Iter, typename Comp = std::less<>,
        REQUIRES(detail::is_forward_iterator_v<Iter> &&
                 detail::is_comparator_v<Comp, value_type_t<Iter>, value_type_t<Iter>>)>
bool is_sorted(Iter first, Iter last, Comp comp = {})
{
    return std::is_sorted(std::move(first), std::move(last), std::move(comp));
}

template <typename Range, typename Comp = std::less<>,
        REQUIRES(detail::is_forward_range_v<Range> &&
                 detail::is_comparator_v<Comp, range_value_type_t<Range>, range_value_type_t<Range>>)>
bool is_sorted(Range&& range, Comp comp = {})
{
    return std::is_sorted(detail::adl_begin(range), detail::adl_end(range), std::move(comp));
}

template <typename Iter, typename Comp = std::less<>,
        REQUIRES(detail::is_forward_iterator_v<Iter> &&
                 detail::is_comparator_v<Comp, value_type_t<Iter>>)>
Iter is_sorted_until(Iter first, Iter last, Comp comp = {})
{
    return std::is_sorted_until(std::move(first), std::move(last), std::move(comp));
}

template <typename Range, typename Comp = std::less<>,
        REQUIRES(detail::is_forward_range_v<Range> &&
                 detail::is_comparator_v<Comp, range_value_type_t<Range>>)>
iterator_t<Range> is_sorted_until(Range&& range, Comp comp = {})
{
    static_assert(!std::is_rvalue_reference<Range&&>::value,
                  "lvalue range required for is_sorted_until()");
    return std::is_sorted_until(detail::adl_begin(range), detail::adl_end(range), std::move(comp));
}

template <typename Iter, typename Comp = std::less<>,
        REQUIRES(detail::is_random_access_iterator_v<Iter> &&
                 detail::is_comparator_v<Comp, value_type_t<Iter>>)>
void sort(Iter first, Iter last, Comp comp = {})
{
    std::sort(std::move(first), std::move(last), std::move(comp));
}

template <typename Range, typename Comp = std::less<>,
        REQUIRES(detail::is_random_access_range_v<Range> &&
                 detail::is_comparator_v<Comp, range_value_type_t<Range>>)>
void sort(Range&& range, Comp comp = {})
{
    std::sort(detail::adl_begin(range), detail::adl_end(range), std::move(comp));
}

template <typename Iter, typename Comp = std::less<>,
        REQUIRES(detail::is_random_access_iterator_v<Iter> &&
                         detail::is_comparator_v<Comp, value_type_t<Iter>>)>
void partial_sort(Iter first, Iter middle, Iter last, Comp comp = {})
{
    std::partial_sort(std::move(first), std::move(middle), std::move(last), std::move(comp));
}

template <typename Range, typename Comp = std::less<>,
        REQUIRES(detail::is_random_access_range_v<Range> &&
                         detail::is_comparator_v<Comp, range_value_type_t<Range>>)>
void partial_sort(Range&& range, iterator_t<Range> middle, Comp comp = {})
{
    std::partial_sort(detail::adl_begin(range), std::move(middle), detail::adl_end(range), std::move(comp));
}

template <typename Iter1, typename Iter2, typename Comp = std::less<>,
        REQUIRES(detail::is_input_iterator_v<Iter1> &&
                 detail::is_random_access_iterator_v<Iter2> &&
                 detail::is_writable_v<Iter2, value_type_t<Iter1>> &&
                 detail::is_comparator_v<Comp, value_type_t<Iter2>>)>
Iter2 partial_sort_copy(Iter1 first, Iter1 last, Iter2 rfirst, Iter2 rlast, Comp comp = {})
{
    return std::partial_sort(std::move(first), std::move(last), std::move(rfirst), std::move(rlast), std::move(comp));
}

template <typename Range1, typename Range2, typename Comp = std::less<>,
        REQUIRES(detail::is_input_range_v<Range1> &&
                 detail::is_random_access_range_v<Range2> &&
                 detail::is_writable_v<iterator_t<Range2>, range_value_type_t<Range1>> &&
                 detail::is_comparator_v<Comp, range_value_type_t<Range2>>)>
iterator_t<Range2> partial_sort_copy(Range1&& input, Range2&& result, Comp comp = {})
{
    static_assert(!std::is_rvalue_reference<Range2&&>::value,
                  "lvalue range required as second argument to partial_sort_copy()");
    return std::partial_sort_copy(detail::adl_begin(input), detail::adl_end(input),
                                  detail::adl_begin(result), detail::adl_end(result),
                                  std::move(comp));
}

template <typename Iter, typename Comp = std::less<>,
        REQUIRES(detail::is_random_access_iterator_v<Iter> &&
                 detail::is_comparator_v<Comp, value_type_t<Iter>>)>
void stable_sort(Iter first, Iter last, Comp comp = {})
{
    std::stable_sort(std::move(first), std::move(last), std::move(comp));
}

template <typename Range, typename Comp = std::less<>,
        REQUIRES(detail::is_random_access_range_v<Range> &&
                         detail::is_comparator_v<Comp, range_value_type_t<Range>>)>
void stable_sort(Range&& range, Comp comp = {})
{
    std::stable_sort(detail::adl_begin(range), detail::adl_end(range), std::move(comp));
}

template <typename Iter, typename Comp = std::less<>,
        REQUIRES(detail::is_random_access_iterator_v<Iter> &&
                 detail::is_comparator_v<Comp, value_type_t<Iter>>)>
void nth_element(Iter first, Iter nth, Iter last, Comp comp = {})
{
    std::nth_element(std::move(first), std::move(nth), std::move(last), std::move(comp));
}

template <typename Range, typename Comp = std::less<>,
        REQUIRES(detail::is_random_access_range_v<Range> &&
                 detail::is_comparator_v<Comp, range_value_type_t<Range>>)>
void nth_element(Range&& range, iterator_t<Range> nth, Comp comp = {})
{
    std::nth_element(detail::adl_begin(range), detail::adl_end(range), std::move(nth), std::move(comp));
}

/*
 * Binary search operations
 */

template <typename Iter, typename T, typename Comp = std::less<>,
        REQUIRES(detail::is_forward_iterator_v<Iter> &&
                 detail::is_binary_predicate_v<Comp, value_type_t<Iter>, const T&>)>
Iter lower_bound(Iter first, Iter last, const T& value, Comp comp = {})
{
    return std::lower_bound(std::move(first), std::move(last), value, std::move(comp));
}

template <typename Range, typename T, typename Comp = std::less<>,
        REQUIRES(detail::is_forward_range_v<Range> &&
                 detail::is_binary_predicate_v<Comp, range_value_type_t<Range>, const T&>)>
iterator_t<Range> lower_bound(Range&& range, const T& value, Comp comp = {})
{
    static_assert(!std::is_rvalue_reference<Range&&>::value,
                  "lvalue range required as argument to lower_bound()");
    return std::lower_bound(detail::adl_begin(range), detail::adl_end(range), value, std::move(comp));
}

template <typename Iter, typename T, typename Comp = std::less<>,
        REQUIRES(detail::is_forward_iterator_v<Iter> &&
                         detail::is_binary_predicate_v<Comp, const T&, value_type_t<Iter>>)>
Iter upper_bound(Iter first, Iter last, const T& value, Comp comp = {})
{
    return std::upper_bound(std::move(first), std::move(last), value, std::move(comp));
}

template <typename Range, typename T, typename Comp = std::less<>,
        REQUIRES(detail::is_forward_range_v<Range> &&
                         detail::is_binary_predicate_v<Comp, const T&, range_value_type_t<Range>>)>
iterator_t<Range> upper_bound(Range&& range, const T& value, Comp comp = {})
{
    static_assert(!std::is_rvalue_reference<Range&&>::value,
                  "lvalue range required as argument to lower_bound()");
    return std::upper_bound(detail::adl_begin(range), detail::adl_end(range), value, std::move(comp));
}

template <typename Iter, typename T, typename Comp = std::less<>,
        REQUIRES(detail::is_forward_iterator_v<Iter> &&
                  detail::is_binary_predicate_v<Comp, const T&, value_type_t<Iter>> &&
                 detail::is_binary_predicate_v<Comp, value_type_t<Iter>, const T&>)>
bool binary_search(Iter first, Iter last, const T& value, Comp comp = {})
{
    return std::binary_search(std::move(first), std::move(last), value, std::move(comp));
}

template <typename Range, typename T, typename Comp = std::less<>,
        REQUIRES(detail::is_forward_range_v<Range> &&
                 detail::is_binary_predicate_v<Comp, const T&, range_value_type_t<Range>> &&
                 detail::is_binary_predicate_v<Comp, range_value_type_t<Range>, const T&>)>
bool upper_bound(Range&& range, const T& value, Comp comp = {})
{
    return std::binary_search(detail::adl_begin(range), detail::adl_end(range), value, std::move(comp));
}

template <typename Iter, typename T, typename Comp = std::less<>,
          REQUIRES(detail::is_forward_iterator_v<Iter> &&
                   detail::is_comparator_v<Comp, value_type_t<Iter>, const T&> &&
                   detail::is_comparator_v<Comp, const T&, value_type_t<Iter>>)>
std::pair<Iter, Iter>
equal_range(Iter first, Iter last, const T& value, Comp comp = {})
{
    return std::equal_range(std::move(first), std::move(last), value, std::move(comp));
}

template <typename Range, typename T, typename Comp = std::less<>,
          REQUIRES(detail::is_forward_range_v<Range> &&
                   detail::is_comparator_v<Comp, range_value_type_t<Range>, const T&> &&
                   detail::is_comparator_v<Comp, const T&, range_value_type_t<Range>>)>
std::pair<iterator_t<Range>, iterator_t<Range>>
equal_range(Range&& range, const T& value, Comp comp = {})
{
    static_assert(!std::is_rvalue_reference<Range&&>::value,
                  "lvalue range required as argument to equal_range()");
    return std::equal_range(detail::adl_begin(range), detail::adl_end(range), value, std::move(comp));
}

/*
 * Set operations
 */

template <typename InputIt1, typename InputIt2, typename OutputIt, typename Comp = std::less<>,
        REQUIRES(detail::is_input_iterator_v<InputIt1> &&
                 detail::is_input_iterator_v<InputIt2> &&
                 detail::is_output_iterator_v<OutputIt, value_type_t<InputIt1>> &&
                 detail::is_output_iterator_v<OutputIt, value_type_t<InputIt2>> &&
                 detail::is_comparator_v<Comp, value_type_t<InputIt1>, value_type_t<InputIt2>> &&
                 detail::is_comparator_v<Comp, value_type_t<InputIt2>, value_type_t<InputIt1>>)>
OutputIt merge(InputIt1 first1, InputIt1 last1, InputIt2 first2, InputIt2 last2,  OutputIt ofirst, Comp comp = {})
{
    return std::merge(std::move(first1), std::move(last1),
                      std::move(first2), std::move(last2),
                      std::move(ofirst), std::move(comp));
}

template <typename InputRng1, typename InputRng2, typename OutputIt, typename Comp = std::less<>,
        REQUIRES(detail::is_input_range_v<InputRng1> &&
                         detail::is_input_range_v<InputRng2> &&
                         detail::is_output_iterator_v<OutputIt, range_value_type_t<InputRng1>> &&
                         detail::is_output_iterator_v<OutputIt, range_value_type_t<InputRng2>> &&
                         detail::is_comparator_v<Comp, range_value_type_t<InputRng1>, range_value_type_t<InputRng2>> &&
                         detail::is_comparator_v<Comp, range_value_type_t<InputRng2>, range_value_type_t<InputRng1>>)>
OutputIt merge(InputRng1&& range1, InputRng2&& range2, OutputIt ofirst, Comp comp = {})
{
    return std::merge(detail::adl_begin(range1), detail::adl_end(range1),
                      detail::adl_begin(range2), detail::adl_end(range2),
                      std::move(ofirst), std::move(comp));
}

template <typename BidirIt, typename Comp = std::less<>,
        REQUIRES(detail::is_bidirectional_iterator_v<BidirIt> &&
                 detail::is_comparator_v<Comp, value_type_t<BidirIt>>)>
void inplace_merge(BidirIt first, BidirIt middle, BidirIt last, Comp comp = {})
{
    std::inplace_merge(std::move(first), std::move(middle), std::move(last), std::move(comp));
}

template <typename BidirRng, typename Comp = std::less<>,
        REQUIRES(detail::is_bidirectional_range_v<BidirRng> &&
                 detail::is_comparator_v<Comp, range_value_type_t<BidirRng>>)>
void inplace_merge(BidirRng&& range, iterator_t<BidirRng> middle, Comp comp = {})
{
    std::inplace_merge(detail::adl_begin(range), std::move(middle), detail::adl_end(range), std::move(comp));
}

template <typename InputIt1, typename InputIt2, typename Comp = std::less<>,
        REQUIRES(detail::is_input_iterator_v<InputIt1> &&
                 detail::is_input_iterator_v<InputIt2> &&
                 detail::is_comparator_v<Comp, value_type_t<InputIt1>, value_type_t<InputIt2>> &&
                 detail::is_comparator_v<Comp, value_type_t<InputIt2>, value_type_t<InputIt1>>)>
bool includes(InputIt1 first1, InputIt1 last1, InputIt2 first2, InputIt2 last2, Comp comp = {})
{
    return std::includes(std::move(first1), std::move(last1),
                         std::move(first2), std::move(last2),
                         std::move(comp));
}

template <typename InputRng1, typename InputRng2, typename Comp = std::less<>,
        REQUIRES(detail::is_input_range_v<InputRng1> &&
                 detail::is_input_range_v<InputRng2> &&
                 detail::is_comparator_v<Comp, range_value_type_t<InputRng1>, range_value_type_t<InputRng2>> &&
                 detail::is_comparator_v<Comp, range_value_type_t<InputRng2>, range_value_type_t<InputRng1>>)>
bool includes(InputRng1&& range1, InputRng2&& range2, Comp comp = {})
{
    return std::includes(detail::adl_begin(range1), detail::adl_end(range1),
                         detail::adl_begin(range2), detail::adl_end(range2),
                         std::move(comp));
}

template <typename InputIt1, typename InputIt2, typename OutputIt, typename Comp = std::less<>,
          REQUIRES(detail::is_input_iterator_v<InputIt1> &&
                   detail::is_input_iterator_v<InputIt2> &&
                   detail::is_output_iterator_v<OutputIt, value_type_t<InputIt1>> &&
                   detail::is_output_iterator_v<OutputIt, value_type_t<InputIt2>> &&
                   detail::is_comparator_v<Comp, value_type_t<InputIt1>, value_type_t<InputIt2>> &&
                   detail::is_comparator_v<Comp, value_type_t<InputIt2>, value_type_t<InputIt2>>)>
OutputIt set_difference(InputIt1 first1, InputIt1 last1, InputIt2 first2, InputIt2 last2,
                        OutputIt ofirst, Comp comp = {})
{
    return std::set_difference(std::move(first1), std::move(last1),
                               std::move(first2), std::move(last2),
                               std::move(ofirst), std::move(comp));
}

template <typename InputRng1, typename InputRng2, typename OutputIt, typename Comp = std::less<>,
        REQUIRES(detail::is_input_range_v<InputRng1> &&
                         detail::is_input_range_v<InputRng2> &&
                         detail::is_output_iterator_v<OutputIt, range_value_type_t<InputRng1>> &&
                         detail::is_output_iterator_v<OutputIt, range_value_type_t<InputRng2>> &&
                         detail::is_comparator_v<Comp, range_value_type_t<InputRng1>, range_value_type_t<InputRng2>> &&
                         detail::is_comparator_v<Comp, range_value_type_t<InputRng2>, range_value_type_t<InputRng2>>)>
OutputIt set_difference(InputRng1&& range1, InputRng2&& range2, OutputIt ofirst, Comp comp = {})
{
    return std::set_difference(detail::adl_begin(range1), detail::adl_end(range1),
                               detail::adl_begin(range2), detail::adl_end(range2),
                               std::move(ofirst), std::move(comp));
}

template <typename InputIt1, typename InputIt2, typename OutputIt, typename Comp = std::less<>,
        REQUIRES(detail::is_input_iterator_v<InputIt1> &&
                         detail::is_input_iterator_v<InputIt2> &&
                         detail::is_output_iterator_v<OutputIt, value_type_t<InputIt1>> &&
                         detail::is_output_iterator_v<OutputIt, value_type_t<InputIt2>> &&
                         detail::is_comparator_v<Comp, value_type_t<InputIt1>, value_type_t<InputIt2>> &&
                         detail::is_comparator_v<Comp, value_type_t<InputIt2>, value_type_t<InputIt2>>)>
OutputIt set_intersection(InputIt1 first1, InputIt1 last1, InputIt2 first2, InputIt2 last2,
                        OutputIt ofirst, Comp comp = {})
{
    return std::set_intersection(std::move(first1), std::move(last1),
                                 std::move(first2), std::move(last2),
                                 std::move(ofirst), std::move(comp));
}

template <typename InputRng1, typename InputRng2, typename OutputIt, typename Comp = std::less<>,
        REQUIRES(detail::is_input_range_v<InputRng1> &&
                         detail::is_input_range_v<InputRng2> &&
                         detail::is_output_iterator_v<OutputIt, range_value_type_t<InputRng1>> &&
                         detail::is_output_iterator_v<OutputIt, range_value_type_t<InputRng2>> &&
                         detail::is_comparator_v<Comp, range_value_type_t<InputRng1>, range_value_type_t<InputRng2>> &&
                         detail::is_comparator_v<Comp, range_value_type_t<InputRng2>, range_value_type_t<InputRng2>>)>
OutputIt set_intersection(InputRng1&& range1, InputRng2&& range2, OutputIt ofirst, Comp comp = {})
{
    return std::set_intersection(detail::adl_begin(range1), detail::adl_end(range1),
                                 detail::adl_begin(range2), detail::adl_end(range2),
                                 std::move(ofirst), std::move(comp));
}

template <typename InputIt1, typename InputIt2, typename OutputIt, typename Comp = std::less<>,
        REQUIRES(detail::is_input_iterator_v<InputIt1> &&
                         detail::is_input_iterator_v<InputIt2> &&
                         detail::is_output_iterator_v<OutputIt, value_type_t<InputIt1>> &&
                         detail::is_output_iterator_v<OutputIt, value_type_t<InputIt2>> &&
                         detail::is_comparator_v<Comp, value_type_t<InputIt1>, value_type_t<InputIt2>> &&
                         detail::is_comparator_v<Comp, value_type_t<InputIt2>, value_type_t<InputIt2>>)>
OutputIt set_symmetric_difference(InputIt1 first1, InputIt1 last1, InputIt2 first2, InputIt2 last2,
                        OutputIt ofirst, Comp comp = {})
{
    return std::set_symmetric_difference(std::move(first1), std::move(last1),
                                         std::move(first2), std::move(last2),
                                         std::move(ofirst), std::move(comp));
}

template <typename InputRng1, typename InputRng2, typename OutputIt, typename Comp = std::less<>,
        REQUIRES(detail::is_input_range_v<InputRng1> &&
                         detail::is_input_range_v<InputRng2> &&
                         detail::is_output_iterator_v<OutputIt, range_value_type_t<InputRng1>> &&
                         detail::is_output_iterator_v<OutputIt, range_value_type_t<InputRng2>> &&
                         detail::is_comparator_v<Comp, range_value_type_t<InputRng1>, range_value_type_t<InputRng2>> &&
                         detail::is_comparator_v<Comp, range_value_type_t<InputRng2>, range_value_type_t<InputRng2>>)>
OutputIt set_symmetric_difference(InputRng1&& range1, InputRng2&& range2, OutputIt ofirst, Comp comp = {})
{
    return std::set_symmetric_difference(detail::adl_begin(range1), detail::adl_end(range1),
                                         detail::adl_begin(range2), detail::adl_end(range2),
                                         std::move(ofirst), std::move(comp));
}

template <typename InputIt1, typename InputIt2, typename OutputIt, typename Comp = std::less<>,
        REQUIRES(detail::is_input_iterator_v<InputIt1> &&
                         detail::is_input_iterator_v<InputIt2> &&
                         detail::is_output_iterator_v<OutputIt, value_type_t<InputIt1>> &&
                         detail::is_output_iterator_v<OutputIt, value_type_t<InputIt2>> &&
                         detail::is_comparator_v<Comp, value_type_t<InputIt1>, value_type_t<InputIt2>> &&
                         detail::is_comparator_v<Comp, value_type_t<InputIt2>, value_type_t<InputIt2>>)>
OutputIt set_union(InputIt1 first1, InputIt1 last1, InputIt2 first2, InputIt2 last2,
                        OutputIt ofirst, Comp comp = {})
{
    return std::set_union(std::move(first1), std::move(last1),
                          std::move(first2), std::move(last2),
                          std::move(ofirst), std::move(comp));
}

template <typename InputRng1, typename InputRng2, typename OutputIt, typename Comp = std::less<>,
        REQUIRES(detail::is_input_range_v<InputRng1> &&
                         detail::is_input_range_v<InputRng2> &&
                         detail::is_output_iterator_v<OutputIt, range_value_type_t<InputRng1>> &&
                         detail::is_output_iterator_v<OutputIt, range_value_type_t<InputRng2>> &&
                         detail::is_comparator_v<Comp, range_value_type_t<InputRng1>, range_value_type_t<InputRng2>> &&
                         detail::is_comparator_v<Comp, range_value_type_t<InputRng2>, range_value_type_t<InputRng2>>)>
OutputIt set_union(InputRng1&& range1, InputRng2&& range2, OutputIt ofirst, Comp comp = {})
{
    return std::set_union(detail::adl_begin(range1), detail::adl_end(range1),
                          detail::adl_begin(range2), detail::adl_end(range2),
                          std::move(ofirst), std::move(comp));
}

/*
 * Heap operations
 */

template <typename RandomIt, typename Comp = std::less<>,
          REQUIRES(detail::is_random_access_iterator_v<RandomIt> &&
                   detail::is_comparator_v<Comp, value_type_t<RandomIt>>)>
bool is_heap(RandomIt first, RandomIt last, Comp comp = {})
{
    return std::is_heap(std::move(first), std::move(last), std::move(comp));
}

template <typename RandomRng, typename Comp = std::less<>,
        REQUIRES(detail::is_random_access_range_v<RandomRng> &&
                         detail::is_comparator_v<Comp, range_value_type_t<RandomRng>>)>
bool is_heap(RandomRng&& range, Comp comp = {})
{
    return std::is_heap(detail::adl_begin(range), detail::adl_end(range), std::move(comp));
}

template <typename RandomIt, typename Comp = std::less<>,
        REQUIRES(detail::is_random_access_iterator_v<RandomIt> &&
                         detail::is_comparator_v<Comp, value_type_t<RandomIt>>)>
RandomIt is_heap_until(RandomIt first, RandomIt last, Comp comp = {})
{
    return std::is_heap_until(std::move(first), std::move(last), std::move(comp));
}

template <typename RandomRng, typename Comp = std::less<>,
        REQUIRES(detail::is_random_access_range_v<RandomRng> &&
                         detail::is_comparator_v<Comp, range_value_type_t<RandomRng>>)>
iterator_t<RandomRng> is_heap_until(RandomRng&& range, Comp comp = {})
{
    static_assert(!std::is_rvalue_reference<RandomRng&&>::value,
                  "lvalue range required as argument to is_heap_until()");
    return std::is_heap_until(detail::adl_begin(range), detail::adl_end(range), std::move(comp));
}

template <typename RandomIt, typename Comp = std::less<>,
          REQUIRES(detail::is_random_access_iterator_v<RandomIt> &&
                   detail::is_comparator_v<Comp, value_type_t<RandomIt>>)>
void make_heap(RandomIt first, RandomIt last, Comp comp = {})
{
    std::make_heap(std::move(first), std::move(last), std::move(comp));
}

template <typename RandomRng, typename Comp = std::less<>,
        REQUIRES(detail::is_random_access_range_v<RandomRng> &&
                         detail::is_comparator_v<Comp, range_value_type_t<RandomRng>>)>
void make_heap(RandomRng&& range, Comp comp = {})
{
    std::make_heap(detail::adl_begin(range), detail::adl_end(range), std::move(comp));
}

template <typename RandomIt, typename Comp = std::less<>,
        REQUIRES(detail::is_random_access_iterator_v<RandomIt> &&
                         detail::is_comparator_v<Comp, value_type_t<RandomIt>>)>
void push_heap(RandomIt first, RandomIt last, Comp comp = {})
{
    std::push_heap(std::move(first), std::move(last), std::move(comp));
}

template <typename RandomRng, typename Comp = std::less<>,
        REQUIRES(detail::is_random_access_range_v<RandomRng> &&
                         detail::is_comparator_v<Comp, range_value_type_t<RandomRng>>)>
void push_heap(RandomRng&& range, Comp comp = {})
{
    std::push_heap(detail::adl_begin(range), detail::adl_end(range), std::move(comp));
}

template <typename RandomIt, typename Comp = std::less<>,
        REQUIRES(detail::is_random_access_iterator_v<RandomIt> &&
                         detail::is_comparator_v<Comp, value_type_t<RandomIt>>)>
void pop_heap(RandomIt first, RandomIt last, Comp comp = {})
{
    std::pop_heap(std::move(first), std::move(last), std::move(comp));
}

template <typename RandomRng, typename Comp = std::less<>,
        REQUIRES(detail::is_random_access_range_v<RandomRng> &&
                         detail::is_comparator_v<Comp, range_value_type_t<RandomRng>>)>
void pop_heap(RandomRng&& range, Comp comp = {})
{
    std::pop_heap(detail::adl_begin(range), detail::adl_end(range), std::move(comp));
}

template <typename RandomIt, typename Comp = std::less<>,
        REQUIRES(detail::is_random_access_iterator_v<RandomIt> &&
                         detail::is_comparator_v<Comp, value_type_t<RandomIt>>)>
void sort_heap(RandomIt first, RandomIt last, Comp comp = {})
{
    std::sort_heap(std::move(first), std::move(last), std::move(comp));
}

template <typename RandomRng, typename Comp = std::less<>,
        REQUIRES(detail::is_random_access_range_v<RandomRng> &&
                         detail::is_comparator_v<Comp, range_value_type_t<RandomRng>>)>
void sort_heap(RandomRng&& range, Comp comp = {})
{
    std::sort_heap(detail::adl_begin(range), detail::adl_end(range), std::move(comp));
}

/*
 * Min/max operations
 */

template <typename T, typename Comp = std::less<>,
          REQUIRES(detail::is_comparator_v<Comp, const T&>)>
constexpr const T& max(const T& a, const T& b, Comp comp = {})
{
    return std::max(a, b, std::move(comp));
}

template <typename T, typename Comp = std::less<>,
          REQUIRES(detail::is_comparator_v<Comp, const T&>)>
constexpr T max(std::initializer_list<T> ilist, Comp comp = {})
{
    return std::max(ilist, std::move(comp));
}

template <typename ForwardRng, typename Comp = std::less<>,
          REQUIRES(detail::is_forward_range_v<ForwardRng> &&
                   detail::is_comparator_v<Comp, range_value_type_t<ForwardRng>>)>
value_type_t<iterator_t<ForwardRng>>
max(ForwardRng&& range, Comp comp = {})
{
    return *std::max_element(detail::adl_begin(range), detail::adl_end(range), std::move(comp));
}

template <typename ForwardIt, typename Comp = std::less<>,
          REQUIRES(detail::is_forward_iterator_v<ForwardIt> &&
                   detail::is_comparator_v<Comp, value_type_t<ForwardIt>>)>
ForwardIt max_element(ForwardIt first, ForwardIt last, Comp comp = {})
{
    return std::max_element(std::move(first), std::move(last), std::move(comp));
}

template <typename ForwardRng, typename Comp = std::less<>,
          REQUIRES(detail::is_forward_range_v<ForwardRng> &&
                   detail::is_comparator_v<Comp, range_value_type_t<ForwardRng>>)>
iterator_t<ForwardRng>
max_element(ForwardRng&& range, Comp comp = {})
{
    static_assert(!std::is_rvalue_reference<ForwardRng&&>::value,
                  "lvalue range required as argument to max_element()");
    return std::max_element(detail::adl_begin(range), detail::adl_end(range), std::move(comp));
}

template <typename T, typename Comp = std::less<>,
        REQUIRES(detail::is_comparator_v<Comp, const T&>)>
constexpr const T& min(const T& a, const T& b, Comp comp = {})
{
    return std::min(a, b, std::move(comp));
}

template <typename T, typename Comp = std::less<>,
        REQUIRES(detail::is_comparator_v<Comp, const T&>)>
constexpr T min(std::initializer_list<T> ilist, Comp comp = {})
{
    return std::min(ilist, std::move(comp));
}

template <typename ForwardRng, typename Comp = std::less<>,
        REQUIRES(detail::is_forward_range_v<ForwardRng> &&
                         detail::is_comparator_v<Comp, range_value_type_t<ForwardRng>>)>
value_type_t<iterator_t<ForwardRng>>
min(ForwardRng&& range, Comp comp = {})
{
    return *std::min_element(detail::adl_begin(range), detail::adl_end(range), std::move(comp));
}

template <typename ForwardRng, typename Comp = std::less<>,
        REQUIRES(detail::is_forward_range_v<ForwardRng> &&
                         detail::is_comparator_v<Comp, range_value_type_t<ForwardRng>>)>
iterator_t<ForwardRng>
min_element(ForwardRng&& range, Comp comp = {})
{
    static_assert(!std::is_rvalue_reference<ForwardRng&&>::value,
                  "lvalue range required as argument to min_element()");
    return std::min_element(detail::adl_begin(range), detail::adl_end(range), std::move(comp));
}

template <typename T, typename Comp = std::less<>,
          REQUIRES(detail::is_comparator_v<Comp, const T&>)>
constexpr std::pair<const T&, const T&> minmax(const T& a, const T& b, Comp comp)
{
    return std::minmax(a, b, std::move(comp));
}

template <typename T, typename Comp = std::less<>,
        REQUIRES(detail::is_comparator_v<Comp, const T&>)>
constexpr std::pair<T, T> minmax(std::initializer_list<T> ilist, Comp comp)
{
    return std::minmax(ilist, std::move(comp));
}

template <typename ForwardRng, typename Comp = std::less<>,
          REQUIRES(detail::is_comparator_v<Comp, range_value_type_t<ForwardRng>>)>
constexpr std::pair<range_value_type_t<ForwardRng>, range_value_type_t<ForwardRng>>
minmax(ForwardRng&& range, Comp comp = {})
{
    const auto p = std::minmax_element(detail::adl_begin(range), detail::adl_end(range), std::move(comp));
    return {*p.first, *p.second};
}

template <typename ForwardIt, typename Comp = std::less<>,
          REQUIRES(detail::is_forward_iterator_v<ForwardIt> &&
                   detail::is_comparator_v<Comp, value_type_t<ForwardIt>>)>
constexpr std::pair<ForwardIt, ForwardIt>
minmax_element(ForwardIt first, ForwardIt last, Comp comp = {})
{
    return std::minmax_element(std::move(first), std::move(last), std::move(comp));
}

template <typename ForwardRng, typename Comp = std::less<>,
          REQUIRES(detail::is_forward_range_v<ForwardRng> &&
                   detail::is_comparator_v<Comp, range_value_type_t<ForwardRng>>)>
constexpr std::pair<iterator_t<ForwardRng>, iterator_t<ForwardRng>>
minmax_element(ForwardRng&& range, Comp comp = {})
{
    static_assert(!std::is_rvalue_reference<ForwardRng&&>::value,
                  "lvalue range required as argument to minmax_element()");
    return std::minmax_element(detail::adl_begin(range), detail::adl_end(range), std::move(comp));
}

template <typename InputIt1, typename InputIt2, typename Comp = std::less<>,
          REQUIRES(detail::is_input_iterator_v<InputIt1> &&
                   detail::is_input_iterator_v<InputIt2> &&
                   detail::is_comparator_v<Comp, value_type_t<InputIt1>, value_type_t<InputIt2>> &&
                   detail::is_comparator_v<Comp, value_type_t<InputIt2>, value_type_t<InputIt2>>)>
bool lexicographical_compare(InputIt1 first1, InputIt2 last1,
                             InputIt2 first2, InputIt2 last2, Comp comp = {})
{
    return std::lexicographical_compare(std::move(first1), std::move(last1),
                                        std::move(first2), std::move(last2),
                                        std::move(comp));
}

template <typename InputRng1, typename InputRng2, typename Comp = std::less<>,
        REQUIRES(detail::is_input_range_v<InputRng1> &&
                         detail::is_input_range_v<InputRng2> &&
                         detail::is_comparator_v<Comp, range_value_type_t<InputRng1>, value_type_t<InputRng2>> &&
                         detail::is_comparator_v<Comp, range_value_type_t<InputRng2>, value_type_t<InputRng2>>)>
bool lexicographical_compare(InputRng1&& range1, InputRng2&& range2, Comp comp = {})
{
    return std::lexicographical_compare(detail::adl_begin(range1), detail::adl_end(range1),
                                        detail::adl_begin(range2), detail::adl_end(range2),
                                        std::move(comp));
}

template <typename ForwardIt1, typename ForwardIt2, typename Pred = std::equal_to<>,
          REQUIRES(detail::is_forward_iterator_v<ForwardIt1> &&
                   detail::is_forward_iterator_v<ForwardIt2> &&
                   detail::is_binary_predicate_v<Pred, value_type_t<ForwardIt1>, value_type_t<ForwardIt2>> &&
                   detail::is_binary_predicate_v<Pred, value_type_t<ForwardIt2>, value_type_t<ForwardIt1>>)>
TCB_RANGES_DEPRECATED
bool is_permutation(ForwardIt1 first1, ForwardIt1 last1, ForwardIt2 first2, Pred pred = {})
{
    return std::is_permutation(std::move(first1), std::move(last1), std::move(first2), std::move(pred));
}

template <typename ForwardIt1, typename ForwardIt2, typename Pred = std::equal_to<>,
        REQUIRES(detail::is_forward_iterator_v<ForwardIt1> &&
                         detail::is_forward_iterator_v<ForwardIt2> &&
                         detail::is_binary_predicate_v<Pred, value_type_t<ForwardIt1>, value_type_t<ForwardIt2>> &&
                         detail::is_binary_predicate_v<Pred, value_type_t<ForwardIt2>, value_type_t<ForwardIt1>>)>
bool is_permutation(ForwardIt1 first1, ForwardIt1 last1, ForwardIt2 first2, ForwardIt2 last2, Pred pred = {})
{
    return std::is_permutation(std::move(first1), std::move(last1), std::move(first2), std::move(last2), std::move(pred));
}

template <typename ForwardRng1, typename ForwardRng2, typename Pred = std::equal_to<>,
        REQUIRES(detail::is_forward_range_v<ForwardRng1> &&
                         detail::is_forward_range_v<ForwardRng2> &&
                         detail::is_binary_predicate_v<Pred, range_value_type_t<ForwardRng1>, range_value_type_t<ForwardRng2>> &&
                         detail::is_binary_predicate_v<Pred, range_value_type_t<ForwardRng2>, range_value_type_t<ForwardRng1>>)>
bool is_permutation(ForwardRng1&& range1, ForwardRng2&& range2, Pred pred = {})
{
    return std::is_permutation(detail::adl_begin(range1), detail::adl_end(range2),
                               detail::adl_begin(range2), detail::adl_end(range2),
                               std::move(pred));
}

template <typename BidirIt, typename Comp = std::less<>,
          REQUIRES(detail::is_bidirectional_iterator_v<BidirIt> &&
                   detail::is_comparator_v<Comp, value_type_t<BidirIt>>)>
bool next_permutation(BidirIt first, BidirIt last, Comp comp = {})
{
    return std::next_permutation(std::move(first), std::move(last), std::move(comp));
}

template <typename BidirRng, typename Comp = std::less<>,
        REQUIRES(detail::is_bidirectional_range_v<BidirRng> &&
                 detail::is_comparator_v<Comp, range_value_type_t<BidirRng>>)>
bool next_permutation(BidirRng&& range, Comp comp = {})
{
    return std::next_permutation(detail::adl_begin(range), detail::adl_end(range), std::move(comp));
}

template <typename BidirIt, typename Comp = std::less<>,
        REQUIRES(detail::is_bidirectional_iterator_v<BidirIt> &&
                         detail::is_comparator_v<Comp, value_type_t<BidirIt>>)>
bool prev_permutation(BidirIt first, BidirIt last, Comp comp = {})
{
    return std::prev_permutation(std::move(first), std::move(last), std::move(comp));
}

template <typename BidirRng, typename Comp = std::less<>,
        REQUIRES(detail::is_bidirectional_range_v<BidirRng> &&
                         detail::is_comparator_v<Comp, range_value_type_t<BidirRng>>)>
bool prev_permutation(BidirRng&& range, Comp comp = {})
{
    return std::prev_permutation(detail::adl_begin(range), detail::adl_end(range), std::move(comp));
}

/*
 * Numeric algorithms
 */

template <typename ForwardIt, typename T,
          REQUIRES(detail::is_forward_iterator_v<ForwardIt> &&
                   detail::is_writable_v<ForwardIt, T>)>
void iota(ForwardIt first, ForwardIt last, T value)
{
    return std::iota(std::move(first), std::move(last), std::move(value));
}

template <typename ForwardRng, typename T,
          REQUIRES(detail::is_forward_range_v<ForwardRng> &&
                   detail::is_writable_v<iterator_t<ForwardRng>, T>)>
void iota(ForwardRng&& range, T value)
{
    return std::iota(detail::adl_begin(range), detail::adl_end(range), std::move(value));
}

template <typename InputIt, typename T, typename BinOp = std::plus<>,
          REQUIRES(detail::is_input_iterator_v<InputIt> &&
                   std::is_assignable<T,
                           std::result_of_t<BinOp&(const T&, value_type_t<InputIt>)>>::value)>
T accumulate(InputIt first, InputIt last, T init, BinOp op = {})
{
    return std::accumulate(std::move(first), std::move(last), std::move(init), std::move(op));
}

template <typename InputRng, typename T, typename BinOp = std::plus<>,
        REQUIRES(detail::is_input_range_v<InputRng> &&
                std::is_assignable<T,
                         std::result_of_t<BinOp&(const T&, range_value_type_t<InputRng>)>>::value)>
T accumulate(InputRng&& range, T init, BinOp op = {})
{
    return std::accumulate(detail::adl_begin(range), detail::adl_end(range), std::move(init), std::move(op));
}

// Oh boy
template <typename InputIt1, typename InputIt2, typename T,
          typename BinOp1 = std::plus<>, typename BinOp2 = std::multiplies<>,
          REQUIRES(detail::is_input_iterator_v<InputIt1> &&
                   detail::is_input_iterator_v<InputIt2> &&
                   std::is_assignable<T, std::result_of_t<
                       BinOp1&(T, std::result_of_t<
                           BinOp2&(value_type_t<InputIt1>, value_type_t<InputIt2>)>)>>::value)>
TCB_RANGES_DEPRECATED
T inner_product(InputIt1 first1, InputIt1 last1, InputIt2 first2,
                T value, BinOp1 op1 = {}, BinOp2 op2 = {})
{
    return std::inner_product(std::move(first1), std::move(last1), std::move(first2),
                              std::move(value), std::move(op1), std::move(op2));
}

template <typename InputIt1, typename InputIt2, typename T,
        typename BinOp1 = std::plus<>, typename BinOp2 = std::multiplies<>,
        REQUIRES(detail::is_input_iterator_v<InputIt1> &&
                         detail::is_input_iterator_v<InputIt2> &&
                                 std::is_assignable<T, std::result_of_t<
                                 BinOp1&(T, std::result_of_t<
                         BinOp2&(value_type_t<InputIt1>, value_type_t<InputIt2>)>)>>::value)>
T inner_product(InputIt1 first1, InputIt1 last1, InputIt2 first2, InputIt2 last2,
                T value, BinOp1 op1 = {}, BinOp2 op2 = {})
{
    // Thanks cppreference
    while (first1 != last1 && first2 != last2) {
        value = op1(value, op2(*first1, *first2));
        ++first1;
        ++first2;
    }
    return value;
}

template <typename InputRng1, typename InputRng2, typename T,
        typename BinOp1 = std::plus<>, typename BinOp2 = std::multiplies<>,
        REQUIRES(detail::is_input_range_v<InputRng1> &&
                         detail::is_input_range_v<InputRng2> &&
                                 std::is_assignable<T, std::result_of_t<
                                 BinOp1&(T, std::result_of_t<
                         BinOp2&(range_value_type_t<InputRng1>, range_value_type_t<InputRng2>)>)>>::value)>
T inner_product(InputRng1&& range1, InputRng2&& range2,
                T value, BinOp1 op1 = {}, BinOp2 op2 = {})
{
    return tcb::ranges::inner_product(
                 detail::adl_begin(range1), detail::adl_end(range1),
                 detail::adl_begin(range2), detail::adl_end(range2),
                 std::move(value), std::move(op1), std::move(op2));
}

template <typename InputIt,  typename OutputIt, typename BinOp = std::minus<>,
          REQUIRES(detail::is_input_iterator_v<InputIt> &&
                   detail::is_output_iterator_v<OutputIt,
                        std::result_of_t<BinOp&(value_type_t<InputIt>, value_type_t<InputIt>)>>)>
OutputIt adjacent_difference(InputIt first, InputIt last, OutputIt ofirst, BinOp op = {})
{
    return std::adjacent_difference(std::move(first), std::move(last), std::move(ofirst), std::move(op));
}

template <typename InputRng,  typename OutputIt, typename BinOp = std::minus<>,
        REQUIRES(detail::is_input_range_v<InputRng> &&
                detail::is_output_iterator_v<OutputIt,
                         std::result_of_t<BinOp&(range_value_type_t<InputRng>, range_value_type_t<InputRng>)>>)>
OutputIt adjacent_difference(InputRng&& range, OutputIt ofirst, BinOp op = {})
{
    return std::adjacent_difference(detail::adl_begin(range), detail::adl_end(range), std::move(ofirst), std::move(op));
}

template <typename InputIt, typename OutputIt, typename BinOp = std::plus<>,
          REQUIRES(detail::is_input_iterator_v<InputIt> &&
                   detail::is_output_iterator_v<OutputIt,
                       std::result_of_t<BinOp&(value_type_t<InputIt>, value_type_t<InputIt>)>>)>
OutputIt partial_sum(InputIt first, InputIt last, OutputIt ofirst, BinOp op = {})
{
    return std::partial_sum(std::move(first), std::move(last), std::move(ofirst), std::move(op));
}

template <typename InputRng, typename OutputIt, typename BinOp = std::plus<>,
        REQUIRES(detail::is_input_range_v<InputRng> &&
                detail::is_output_iterator_v<OutputIt,
                         std::result_of_t<BinOp&(range_value_type_t<InputRng>, range_value_type_t<InputRng>)>>)>
OutputIt partial_sum(InputRng&& range, OutputIt ofirst, BinOp op = {})
{
    return std::partial_sum(detail::adl_begin(range), detail::adl_end(range), std::move(ofirst), std::move(op));
}

#undef REQUIRES

} // end namespace ranges
} // end namespace tcb

#endif
