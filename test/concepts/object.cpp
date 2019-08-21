// cmcstl2 - A concept-enabled C++ standard library
//
//  Copyright Casey Carter 2015
//
//  Use, modification and distribution is subject to the
//  Boost Software License, Version 1.0. (See accompanying
//  file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt)
//
// Project home: https://github.com/caseycarter/cmcstl2
//
#include "validate.hpp"

#include <nanorange/concepts.hpp>

namespace models = nano::ranges;

namespace {

struct copyable {};

struct moveonly {
	moveonly() = default;

	moveonly(moveonly&&) = default;

	moveonly& operator=(moveonly&&) = default;
};

struct copyonly {
	copyonly() = default;

	copyonly(const copyonly&) = default;

	copyonly& operator=(const copyonly&) = default;

	copyonly(copyonly&&) = delete;

	copyonly& operator=(copyonly&&) = delete;
};

struct nonmovable {
	nonmovable() = default;

	nonmovable(nonmovable&&) = delete;
};

struct nondefaultconstructible {
	nondefaultconstructible(int) {}
};

struct indestructible {
	~indestructible() = delete;
};

struct throwing_destructor {
	~throwing_destructor() noexcept(false);
};

struct explicit_default {
	explicit explicit_default() {}
};

struct deleted_default {
	deleted_default() = delete;
};

struct explicit_move {
	explicit_move() = default;

	explicit explicit_move(explicit_move&&) = default;
};

struct explicit_copy {
	explicit_copy() = default;

	explicit_copy(explicit_copy&&) = default;

	explicit explicit_copy(const explicit_copy&) = default;
};

struct partial_overloaded_address {
	partial_overloaded_address* operator&();
};

struct overloaded_address {
	overloaded_address* operator&();

	const overloaded_address* operator&() const;
};

struct bad_overloaded_address {
	void operator&() const;
};

struct bad_overloaded_const_address {
	bad_overloaded_const_address* operator&();

	void operator&() const;
};

struct semiregular {};

struct regular {
	friend constexpr bool operator==(const regular&, const regular&)
	{
		return true;
	}

	friend constexpr bool operator!=(const regular&, const regular&)
	{
		return false;
	}
};

struct XXX {
	XXX() = default;

	XXX(XXX&&) = delete;

	explicit XXX(int) {}
};

CONCEPT_ASSERT(models::destructible<int>);
CONCEPT_ASSERT(models::destructible<const int>);
CONCEPT_ASSERT(!models::destructible<void>);
CONCEPT_ASSERT(models::destructible<int&>);
CONCEPT_ASSERT(!models::destructible<void()>);
CONCEPT_ASSERT(models::destructible<void (*)()>);
CONCEPT_ASSERT(models::destructible<void (&)()>);
CONCEPT_ASSERT(!models::destructible<int[]>);
CONCEPT_ASSERT(models::destructible<int[2]>);
CONCEPT_ASSERT(models::destructible<int (*)[2]>);
//CONCEPT_ASSERT(!models::Addressable<int (&)[2]>);
CONCEPT_ASSERT(models::destructible<int (&)[2]>);
CONCEPT_ASSERT(models::destructible<moveonly>);
CONCEPT_ASSERT(models::destructible<nonmovable>);
CONCEPT_ASSERT(!models::destructible<indestructible>);
CONCEPT_ASSERT(!models::destructible<throwing_destructor>);

#if 0
// ill-formed (hard error)
struct incomplete;
CONCEPT_ASSERT(!models::destructible<incomplete>);
#endif

CONCEPT_ASSERT(models::destructible<partial_overloaded_address>);
CONCEPT_ASSERT(models::destructible<overloaded_address>);
// Hmmm, Destructible is exactly as spec'd, I wonder why these don't work
//CONCEPT_ASSERT(!models::destructible<bad_overloaded_address>);
//CONCEPT_ASSERT(!models::destructible<bad_overloaded_const_address>);

CONCEPT_ASSERT(models::constructible_from<int>);
CONCEPT_ASSERT(models::constructible_from<int const>);
CONCEPT_ASSERT(!models::constructible_from<int const&>);
CONCEPT_ASSERT(!models::constructible_from<int()>);
CONCEPT_ASSERT(!models::constructible_from<int (&)()>);
CONCEPT_ASSERT(!models::constructible_from<int[]>);
CONCEPT_ASSERT(models::constructible_from<int[5]>);
CONCEPT_ASSERT(!models::constructible_from<nondefaultconstructible>);
CONCEPT_ASSERT(models::constructible_from<int const (&)[5], int (&)[5]>);
CONCEPT_ASSERT(!models::constructible_from<int, int (&)[3]>);

CONCEPT_ASSERT(models::constructible_from<int, int>);
CONCEPT_ASSERT(models::constructible_from<int, int&>);
CONCEPT_ASSERT(models::constructible_from<int, int&&>);
CONCEPT_ASSERT(models::constructible_from<int, const int>);
CONCEPT_ASSERT(models::constructible_from<int, const int&>);
CONCEPT_ASSERT(models::constructible_from<int, const int&&>);

CONCEPT_ASSERT(models::constructible_from<copyable, copyable>);
CONCEPT_ASSERT(models::constructible_from<copyable, copyable&>);
CONCEPT_ASSERT(models::constructible_from<copyable, copyable&&>);
CONCEPT_ASSERT(models::constructible_from<copyable, const copyable>);
CONCEPT_ASSERT(models::constructible_from<copyable, const copyable&>);
CONCEPT_ASSERT(models::constructible_from<copyable, const copyable&&>);

CONCEPT_ASSERT(!models::constructible_from<int&, int>);
CONCEPT_ASSERT(models::constructible_from<int&, int&>);
CONCEPT_ASSERT(!models::constructible_from<int&, int&&>);
CONCEPT_ASSERT(!models::constructible_from<int&, const int>);
CONCEPT_ASSERT(!models::constructible_from<int&, const int&>);
CONCEPT_ASSERT(!models::constructible_from<int&, const int&&>);

CONCEPT_ASSERT(models::constructible_from<const int&, int>);
CONCEPT_ASSERT(models::constructible_from<const int&, int&>);
CONCEPT_ASSERT(models::constructible_from<const int&, int&&>);
CONCEPT_ASSERT(models::constructible_from<const int&, const int>);
CONCEPT_ASSERT(models::constructible_from<const int&, const int&>);
CONCEPT_ASSERT(models::constructible_from<const int&, const int&&>);

CONCEPT_ASSERT(models::constructible_from<int&&, int>);
CONCEPT_ASSERT(!models::constructible_from<int&&, int&>);
CONCEPT_ASSERT(models::constructible_from<int&&, int&&>);
CONCEPT_ASSERT(!models::constructible_from<int&&, const int>);
CONCEPT_ASSERT(!models::constructible_from<int&&, const int&>);
CONCEPT_ASSERT(!models::constructible_from<int&&, const int&&>);

CONCEPT_ASSERT(models::constructible_from<const int&&, int>);
CONCEPT_ASSERT(!models::constructible_from<const int&&, int&>);
CONCEPT_ASSERT(models::constructible_from<const int&&, int&&>);
CONCEPT_ASSERT(models::constructible_from<const int&&, const int>);
CONCEPT_ASSERT(!models::constructible_from<const int&&, const int&>);
CONCEPT_ASSERT(models::constructible_from<const int&&, const int&&>);

CONCEPT_ASSERT(models::constructible_from<XXX, int>);

CONCEPT_ASSERT(models::default_constructible<int>);
CONCEPT_ASSERT(models::default_constructible<int const>);
CONCEPT_ASSERT(!models::default_constructible<int&>);
CONCEPT_ASSERT(!models::default_constructible<int const&>);
CONCEPT_ASSERT(!models::default_constructible<int()>);
CONCEPT_ASSERT(!models::default_constructible<int (&)()>);
CONCEPT_ASSERT(models::default_constructible<double>);
CONCEPT_ASSERT(!models::default_constructible<void>);
CONCEPT_ASSERT(!models::default_constructible<int[]>);
CONCEPT_ASSERT(models::default_constructible<int[2]>);
CONCEPT_ASSERT(!models::default_constructible<nondefaultconstructible>);

// It's hard to catch explicit default constructors, see
// http://www.open-std.org/jtc1/sc22/wg21/docs/cwg_active.html#1518.
// CONCEPT_ASSERT(!models::default_constructible<explicit_default>);
CONCEPT_ASSERT(models::default_constructible<explicit_move>);
CONCEPT_ASSERT(models::default_constructible<explicit_copy>);
CONCEPT_ASSERT(!models::default_constructible<deleted_default>);

CONCEPT_ASSERT(!models::move_constructible<void>);
CONCEPT_ASSERT(models::move_constructible<int>);
CONCEPT_ASSERT(models::move_constructible<const int>);
CONCEPT_ASSERT(!models::move_constructible<int[4]>);
CONCEPT_ASSERT(!models::move_constructible<void()>);
CONCEPT_ASSERT(models::move_constructible<int&>);
CONCEPT_ASSERT(models::move_constructible<int&&>);
CONCEPT_ASSERT(models::move_constructible<const int&>);
CONCEPT_ASSERT(models::move_constructible<const int&&>);

CONCEPT_ASSERT(models::constructible_from<moveonly, moveonly>);
CONCEPT_ASSERT(models::move_constructible<copyable>);
CONCEPT_ASSERT(models::move_constructible<moveonly>);
CONCEPT_ASSERT(!models::move_constructible<nonmovable>);
CONCEPT_ASSERT(!models::move_constructible<copyonly>);
CONCEPT_ASSERT(!models::move_constructible<explicit_move>);
CONCEPT_ASSERT(models::move_constructible<explicit_copy>);

CONCEPT_ASSERT(models::move_constructible<nonmovable&>);
CONCEPT_ASSERT(models::move_constructible<nonmovable&&>);
CONCEPT_ASSERT(models::move_constructible<const nonmovable&>);
CONCEPT_ASSERT(models::move_constructible<const nonmovable&&>);

CONCEPT_ASSERT(!models::copy_constructible<void>);
CONCEPT_ASSERT(models::copy_constructible<int>);
CONCEPT_ASSERT(models::copy_constructible<const int>);
CONCEPT_ASSERT(models::copy_constructible<int&>);
CONCEPT_ASSERT(!models::copy_constructible<int&&>);
CONCEPT_ASSERT(models::copy_constructible<const int&>);
CONCEPT_ASSERT(!models::copy_constructible<const int&&>);
CONCEPT_ASSERT(!models::copy_constructible<int[4]>);
CONCEPT_ASSERT(!models::copy_constructible<void()>);

CONCEPT_ASSERT(models::copy_constructible<copyable>);
CONCEPT_ASSERT(!models::copy_constructible<moveonly>);
CONCEPT_ASSERT(!models::copy_constructible<nonmovable>);
CONCEPT_ASSERT(!models::copy_constructible<copyonly>);
CONCEPT_ASSERT(!models::copy_constructible<explicit_move>);
CONCEPT_ASSERT(!models::copy_constructible<explicit_copy>);
CONCEPT_ASSERT(models::copy_constructible<nonmovable&>);
CONCEPT_ASSERT(!models::copy_constructible<nonmovable&&>);
CONCEPT_ASSERT(models::copy_constructible<const nonmovable&>);
CONCEPT_ASSERT(!models::copy_constructible<const nonmovable&&>);

// https://github.com/ericniebler/stl2/issues/301
// FIXME MSVC
#ifndef _MSC_VER
struct not_mutable_ref {
	not_mutable_ref() = default;

	not_mutable_ref(const not_mutable_ref&) = default;

	not_mutable_ref(not_mutable_ref&&) = default;

	not_mutable_ref(not_mutable_ref&) = delete;
};

struct not_const_ref_ref {
	not_const_ref_ref() = default;

	not_const_ref_ref(const not_const_ref_ref&) = default;

	not_const_ref_ref(not_const_ref_ref&&) = default;

	not_const_ref_ref(const not_const_ref_ref&&) = delete;
};

CONCEPT_ASSERT(!models::copy_constructible<not_mutable_ref>);
CONCEPT_ASSERT(!models::copy_constructible<not_const_ref_ref>);
#endif

CONCEPT_ASSERT(models::Movable<int>);
CONCEPT_ASSERT(!models::Movable<const int>);
CONCEPT_ASSERT(models::Movable<double>);
CONCEPT_ASSERT(!models::Movable<void>);
CONCEPT_ASSERT(models::Movable<copyable>);
CONCEPT_ASSERT(models::Movable<moveonly>);
CONCEPT_ASSERT(!models::Movable<nonmovable>);
CONCEPT_ASSERT(!models::Movable<copyonly>);

CONCEPT_ASSERT(models::Copyable<int>);
CONCEPT_ASSERT(!models::Copyable<const int>);
CONCEPT_ASSERT(models::Copyable<double>);
CONCEPT_ASSERT(!models::Copyable<void>);
CONCEPT_ASSERT(models::Copyable<copyable>);
CONCEPT_ASSERT(!models::Copyable<moveonly>);
CONCEPT_ASSERT(!models::Copyable<nonmovable>);
CONCEPT_ASSERT(!models::Copyable<copyonly>);

CONCEPT_ASSERT(models::Semiregular<int>);
CONCEPT_ASSERT(models::Semiregular<double>);
CONCEPT_ASSERT(!models::Semiregular<void>);
CONCEPT_ASSERT(!models::Semiregular<int&>);
CONCEPT_ASSERT(models::Semiregular<semiregular>);
CONCEPT_ASSERT(models::Semiregular<regular>);
CONCEPT_ASSERT(models::Semiregular<copyable>);
CONCEPT_ASSERT(!models::Semiregular<moveonly>);
CONCEPT_ASSERT(!models::Semiregular<nonmovable>);
CONCEPT_ASSERT(!models::Semiregular<copyonly>);
CONCEPT_ASSERT(!models::Semiregular<explicit_move>);
CONCEPT_ASSERT(!models::Semiregular<explicit_copy>);

CONCEPT_ASSERT(models::Regular<int>);
CONCEPT_ASSERT(models::Regular<double>);
CONCEPT_ASSERT(!models::Regular<void>);
CONCEPT_ASSERT(!models::Regular<int&>);
CONCEPT_ASSERT(!models::Regular<semiregular>);
CONCEPT_ASSERT(models::Regular<regular>);
CONCEPT_ASSERT(!models::Regular<copyable>);
CONCEPT_ASSERT(!models::Regular<moveonly>);
CONCEPT_ASSERT(!models::Regular<nonmovable>);
CONCEPT_ASSERT(!models::Regular<copyonly>);
CONCEPT_ASSERT(!models::Regular<explicit_move>);
CONCEPT_ASSERT(!models::Regular<explicit_copy>);

CONCEPT_ASSERT(models::constructible_from<std::initializer_list<int>>);
CONCEPT_ASSERT(models::default_constructible<std::initializer_list<int>>);

CONCEPT_ASSERT(models::constructible_from<int*>);
CONCEPT_ASSERT(models::default_constructible<int*>);

// https://github.com/ericniebler/stl2/issues/301
CONCEPT_ASSERT(!models::constructible_from<int&, long&>);

// https://github.com/ericniebler/stl2/issues/310
CONCEPT_ASSERT(!models::Movable<int&&>);

}
