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

CONCEPT_ASSERT(models::default_initializable<int>);
// FIXME (MSVC)
#ifndef _MSC_VER
CONCEPT_ASSERT(!models::default_initializable<int const>);
#endif
CONCEPT_ASSERT(!models::default_initializable<int&>);
CONCEPT_ASSERT(!models::default_initializable<int const&>);
CONCEPT_ASSERT(!models::default_initializable<int()>);
CONCEPT_ASSERT(!models::default_initializable<int (&)()>);
CONCEPT_ASSERT(models::default_initializable<double>);
CONCEPT_ASSERT(!models::default_initializable<void>);
CONCEPT_ASSERT(!models::default_initializable<int[]>);
CONCEPT_ASSERT(models::constructible_from<int[2]>);
CONCEPT_ASSERT(models::default_initializable<int[2]>);
CONCEPT_ASSERT(!models::default_initializable<nondefaultconstructible>);

// It's hard to catch explicit default constructors, see
// http://www.open-std.org/jtc1/sc22/wg21/docs/cwg_active.html#1518.
// CONCEPT_ASSERT(!models::default_initializable<explicit_default>);
CONCEPT_ASSERT(models::default_initializable<explicit_move>);
CONCEPT_ASSERT(models::default_initializable<explicit_copy>);
CONCEPT_ASSERT(!models::default_initializable<deleted_default>);

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

CONCEPT_ASSERT(models::movable<int>);
CONCEPT_ASSERT(!models::movable<const int>);
CONCEPT_ASSERT(models::movable<double>);
CONCEPT_ASSERT(!models::movable<void>);
CONCEPT_ASSERT(models::movable<copyable>);
CONCEPT_ASSERT(models::movable<moveonly>);
CONCEPT_ASSERT(!models::movable<nonmovable>);
CONCEPT_ASSERT(!models::movable<copyonly>);

CONCEPT_ASSERT(models::copyable<int>);
CONCEPT_ASSERT(!models::copyable<const int>);
CONCEPT_ASSERT(models::copyable<double>);
CONCEPT_ASSERT(!models::copyable<void>);
CONCEPT_ASSERT(models::copyable<copyable>);
CONCEPT_ASSERT(!models::copyable<moveonly>);
CONCEPT_ASSERT(!models::copyable<nonmovable>);
CONCEPT_ASSERT(!models::copyable<copyonly>);

CONCEPT_ASSERT(models::semiregular<int>);
CONCEPT_ASSERT(models::semiregular<double>);
CONCEPT_ASSERT(!models::semiregular<void>);
CONCEPT_ASSERT(!models::semiregular<int&>);
CONCEPT_ASSERT(models::semiregular<semiregular>);
CONCEPT_ASSERT(models::semiregular<regular>);
CONCEPT_ASSERT(models::semiregular<copyable>);
CONCEPT_ASSERT(!models::semiregular<moveonly>);
CONCEPT_ASSERT(!models::semiregular<nonmovable>);
CONCEPT_ASSERT(!models::semiregular<copyonly>);
CONCEPT_ASSERT(!models::semiregular<explicit_move>);
CONCEPT_ASSERT(!models::semiregular<explicit_copy>);

CONCEPT_ASSERT(models::regular<int>);
CONCEPT_ASSERT(models::regular<double>);
CONCEPT_ASSERT(!models::regular<void>);
CONCEPT_ASSERT(!models::regular<int&>);
CONCEPT_ASSERT(!models::regular<semiregular>);
CONCEPT_ASSERT(models::regular<regular>);
CONCEPT_ASSERT(!models::regular<copyable>);
CONCEPT_ASSERT(!models::regular<moveonly>);
CONCEPT_ASSERT(!models::regular<nonmovable>);
CONCEPT_ASSERT(!models::regular<copyonly>);
CONCEPT_ASSERT(!models::regular<explicit_move>);
CONCEPT_ASSERT(!models::regular<explicit_copy>);

CONCEPT_ASSERT(models::constructible_from<std::initializer_list<int>>);
CONCEPT_ASSERT(models::default_initializable<std::initializer_list<int>>);

CONCEPT_ASSERT(models::constructible_from<int*>);
CONCEPT_ASSERT(models::default_initializable<int*>);

// https://github.com/ericniebler/stl2/issues/301
CONCEPT_ASSERT(!models::constructible_from<int&, long&>);

// https://github.com/ericniebler/stl2/issues/310
CONCEPT_ASSERT(!models::movable<int&&>);

}
