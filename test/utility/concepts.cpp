// Range v3 library
//
//  Copyright Eric Niebler 2014-present
//
//  Use, modification and distribution is subject to the
//  Boost Software License, Version 1.0. (See accompanying
//  file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt)
//
// Project home: https://github.com/ericniebler/range-v3

#include <sstream>
#include <vector>
#include <nanorange/ranges.hpp>
#include <nanorange/views/istream.hpp>
#include "../catch.hpp"

namespace ranges = nano::ranges;

struct moveonly
{
    moveonly(moveonly&&) = default;
    moveonly& operator=(moveonly&&) = default;
};

struct nonmovable
{
    nonmovable(nonmovable const &) = delete;
    nonmovable& operator=(nonmovable const &) = delete;
};

struct nondefaultconstructible
{
    nondefaultconstructible(int) {}
};

struct NotDestructible
{
    ~NotDestructible() = delete;
};

struct IntComparable
{
    operator int() const;

    friend bool operator==(IntComparable, IntComparable);
    friend bool operator!=(IntComparable, IntComparable);

    friend bool operator<(IntComparable, IntComparable);
    friend bool operator>(IntComparable, IntComparable);
    friend bool operator<=(IntComparable, IntComparable);
    friend bool operator>=(IntComparable, IntComparable);

    friend bool operator==(int, IntComparable);
    friend bool operator!=(int, IntComparable);
    friend bool operator==(IntComparable, int);
    friend bool operator!=(IntComparable, int);

    friend bool operator<(int, IntComparable);
    friend bool operator<(IntComparable, int);
    friend bool operator>(int, IntComparable);
    friend bool operator>(IntComparable, int);
    friend bool operator<=(int, IntComparable);
    friend bool operator<=(IntComparable, int);
    friend bool operator>=(int, IntComparable);
    friend bool operator>=(IntComparable, int);
};

struct IntSwappable
{
    operator int() const;

    friend void swap(int &, IntSwappable);
    friend void swap(IntSwappable, int &);
    friend void swap(IntSwappable, IntSwappable);
};

static_assert(ranges::destructible<int>, "");
static_assert(ranges::destructible<const int>, "");
static_assert(!ranges::destructible<void>, "");
static_assert(ranges::destructible<int&>, "");
static_assert(!ranges::destructible<void()>, "");
static_assert(ranges::destructible<void(*)()>, "");
static_assert(ranges::destructible<void(&)()>, "");
static_assert(!ranges::destructible<int[]>, "");
static_assert(ranges::destructible<int[2]>, "");
static_assert(ranges::destructible<int(*)[2]>, "");
static_assert(ranges::destructible<int(&)[2]>, "");
static_assert(ranges::destructible<moveonly>, "");
static_assert(ranges::destructible<nonmovable>, "");
static_assert(!ranges::destructible<NotDestructible>, "");

static_assert(ranges::constructible_from<int>, "");
static_assert(ranges::constructible_from<int const>, "");
static_assert(!ranges::constructible_from<void>, "");
static_assert(!ranges::constructible_from<int const &>, "");
static_assert(!ranges::constructible_from<int ()>, "");
static_assert(!ranges::constructible_from<int(&)()>, "");
static_assert(!ranges::constructible_from<int[]>, "");
static_assert(ranges::constructible_from<int[5]>, "");
static_assert(!ranges::constructible_from<nondefaultconstructible>, "");
static_assert(ranges::constructible_from<int const(&)[5], int(&)[5]>, "");
static_assert(!ranges::constructible_from<int, int(&)[3]>, "");

static_assert(ranges::constructible_from<int, int>, "");
static_assert(ranges::constructible_from<int, int&>, "");
static_assert(ranges::constructible_from<int, int&&>, "");
static_assert(ranges::constructible_from<int, const int>, "");
static_assert(ranges::constructible_from<int, const int&>, "");
static_assert(ranges::constructible_from<int, const int&&>, "");

static_assert(!ranges::constructible_from<int&, int>, "");
static_assert(ranges::constructible_from<int&, int&>, "");
static_assert(!ranges::constructible_from<int&, int&&>, "");
static_assert(!ranges::constructible_from<int&, const int>, "");
static_assert(!ranges::constructible_from<int&, const int&>, "");
static_assert(!ranges::constructible_from<int&, const int&&>, "");

static_assert(ranges::constructible_from<const int&, int>, "");
static_assert(ranges::constructible_from<const int&, int&>, "");
static_assert(ranges::constructible_from<const int&, int&&>, "");
static_assert(ranges::constructible_from<const int&, const int>, "");
static_assert(ranges::constructible_from<const int&, const int&>, "");
static_assert(ranges::constructible_from<const int&, const int&&>, "");

static_assert(ranges::constructible_from<int&&, int>, "");
static_assert(!ranges::constructible_from<int&&, int&>, "");
static_assert(ranges::constructible_from<int&&, int&&>, "");
static_assert(!ranges::constructible_from<int&&, const int>, "");
static_assert(!ranges::constructible_from<int&&, const int&>, "");
static_assert(!ranges::constructible_from<int&&, const int&&>, "");

static_assert(ranges::constructible_from<const int&&, int>, "");
static_assert(!ranges::constructible_from<const int&&, int&>, "");
static_assert(ranges::constructible_from<const int&&, int&&>, "");
static_assert(ranges::constructible_from<const int&&, const int>, "");
static_assert(!ranges::constructible_from<const int&&, const int&>, "");
static_assert(ranges::constructible_from<const int&&, const int&&>, "");

struct XXX
{
    XXX() = default;
    XXX(XXX&&) = delete;
    explicit XXX(int) {}
};

static_assert(ranges::constructible_from<XXX, int>, "");

static_assert(ranges::default_constructible<int>, "");
static_assert(ranges::default_constructible<int const>, "");
static_assert(!ranges::default_constructible<int const &>, "");
static_assert(!ranges::default_constructible<int ()>, "");
static_assert(!ranges::default_constructible<int(&)()>, "");
static_assert(!ranges::default_constructible<int[]>, "");
static_assert(ranges::default_constructible<int[5]>, "");
static_assert(!ranges::default_constructible<nondefaultconstructible>, "");

static_assert(ranges::move_constructible<int>, "");
static_assert(ranges::move_constructible<const int>, "");
static_assert(ranges::move_constructible<int &>, "");
static_assert(ranges::move_constructible<int &&>, "");
static_assert(ranges::move_constructible<const int &>, "");
static_assert(ranges::move_constructible<const int &&>, "");
static_assert(ranges::destructible<moveonly>, "");
static_assert(ranges::constructible_from<moveonly, moveonly>, "");
static_assert(ranges::move_constructible<moveonly>, "");
static_assert(!ranges::move_constructible<nonmovable>, "");
static_assert(ranges::move_constructible<nonmovable &>, "");
static_assert(ranges::move_constructible<nonmovable &&>, "");
static_assert(ranges::move_constructible<const nonmovable &>, "");
static_assert(ranges::move_constructible<const nonmovable &&>, "");

static_assert(ranges::copy_constructible<int>, "");
static_assert(ranges::copy_constructible<const int>, "");
static_assert(ranges::copy_constructible<int &>, "");
static_assert(!ranges::copy_constructible<int &&>, "");
static_assert(ranges::copy_constructible<const int &>, "");
static_assert(!ranges::copy_constructible<const int &&>, "");
static_assert(!ranges::copy_constructible<moveonly>, "");
static_assert(!ranges::copy_constructible<nonmovable>, "");
static_assert(ranges::copy_constructible<nonmovable &>, "");
static_assert(!ranges::copy_constructible<nonmovable &&>, "");
static_assert(ranges::copy_constructible<const nonmovable &>, "");
static_assert(!ranges::copy_constructible<const nonmovable &&>, "");

static_assert(ranges::movable<int>, "");
static_assert(!ranges::movable<int const>, "");
static_assert(ranges::movable<moveonly>, "");
static_assert(!ranges::movable<nonmovable>, "");

static_assert(ranges::copyable<int>, "");
static_assert(!ranges::copyable<int const>, "");
static_assert(!ranges::copyable<moveonly>, "");
static_assert(!ranges::copyable<nonmovable>, "");

static_assert(ranges::input_iterator<int*>, "");
static_assert(!ranges::input_iterator<int>, "");

static_assert(ranges::forward_iterator<int*>, "");
static_assert(!ranges::forward_iterator<int>, "");

static_assert(ranges::bidirectional_iterator<int*>, "");
static_assert(!ranges::bidirectional_iterator<int>, "");

static_assert(ranges::random_access_iterator<int*>, "");
static_assert(!ranges::random_access_iterator<int>, "");

static_assert(ranges::view<ranges::basic_istream_view<int, char>>, "");
static_assert(ranges::input_iterator<ranges::iterator_t<ranges::basic_istream_view<int, char>>>, "");
static_assert(!ranges::view<int>, "");

static_assert(ranges::common_range<std::vector<int> >, "");
//static_assert(!ranges::BoundedView<std::vector<int>>, "");
//static_assert(!ranges::BoundedView<std::vector<int> &>, "");
static_assert(ranges::random_access_iterator<ranges::iterator_t<std::vector<int> const &>>, "");
//static_assert(!ranges::BoundedView<ranges::istream_range<int>>, "");

static_assert(ranges::predicate<std::less<int>, int, int>, "");
static_assert(!ranges::predicate<std::less<int>, char*, int>, "");

static_assert(ranges::output_iterator<int *, int>, "");
static_assert(!ranges::output_iterator<int const *, int>, "");

static_assert(ranges::swappable<int &>, "");
// FIXME: Find out if this is intended to be false
// static_assert(!ranges::Swappable<int>, "");
static_assert(!ranges::swappable<int const &>, "");
static_assert(ranges::swappable<IntSwappable>, "");
static_assert(ranges::swappable_with<IntSwappable, int &>, "");
static_assert(!ranges::swappable_with<IntSwappable, int const &>, "");

//static_assert(ranges::WeaklyOrdered<int>, "");
static_assert(ranges::common_with<int, IntComparable>, "");
static_assert(ranges::common_reference_with<int &, IntComparable &>, "");
//static_assert(ranges::WeaklyOrdered<int, IntComparable>, "");
//static_assert(ranges::WeaklyOrdered<IntComparable, int>, "");
static_assert(ranges::detail::weakly_equality_comparable_with<int, int>, "");
static_assert(ranges::equality_comparable<int>, "");
static_assert(ranges::equality_comparable_with<int, int>, "");
static_assert(ranges::equality_comparable_with<int, IntComparable>, "");
static_assert(ranges::equality_comparable_with<int &, IntComparable &>, "");

/*static_assert(
    std::is_same<
        ranges::bounded_range_concept_t<std::vector<int>>,
        ranges::concepts::CommonRange
    >::value, "");

static_assert(
    std::is_same<
        ranges::sized_range_concept_t<std::vector<int>>,
        ranges::concepts::SizedRange
    >::value, "");

static_assert(
    std::is_same<
        ranges::bounded_view_concept_t<ranges::istream_range<int>>,
        ranges::concepts::View
    >::value, "");

static_assert(
    std::is_same<
        ranges::sized_view_concept_t<ranges::istream_range<int>>,
        ranges::concepts::View
    >::value, "");*/

struct myview {
    const char *begin();
    const char *end();
};
static_assert(ranges::view<myview>, "");

TEST_CASE("util.concepts")
{
}