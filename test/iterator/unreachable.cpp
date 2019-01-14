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
#include <nanorange/iterator/unreachable.hpp>
#include <nanorange/iterator/common_iterator.hpp>
#include <nanorange/algorithm/find.hpp>
#include <algorithm>
#include "../catch.hpp"

// Believe it or not, this generates reasonable code:
// _Z11strlen_testPKc:
// .LFB9467:
// 	.cfi_startproc
// 	cmpb	$0, (%rdi)
// 	movq	%rdi, %rax
// 	je	.L4
// 	.p2align 4,,10
// 	.p2align 3
// .L5:
// 	addq	$1, %rax
// 	cmpb	$0, (%rax)
// 	jne	.L5
// .L4:
// 	subq	%rdi, %rax
// 	ret
// 	.cfi_endproc

namespace __stl2 = ::nano::ranges;

static
int strlen_test(const char* p) noexcept {
	using C = __stl2::common_iterator<const char*, __stl2::unreachable_sentinel_t>;
	return __stl2::distance(C{p}, std::find(C{p}, C{__stl2::unreachable_sentinel}, '\0'));
}

static constexpr int constexpr_strlen_test(const char* p) noexcept {
	using C = __stl2::common_iterator<const char*, __stl2::unreachable_sentinel_t>;
	return __stl2::distance(C{p}, nano::find(C{p}, C{__stl2::unreachable_sentinel}, '\0'));
}

TEST_CASE("iter.unreachable") {
	static_assert(constexpr_strlen_test("Hello, world!") == 13, "");
	CHECK(strlen_test("123This is a test, this is only a test.456") == 42);
}
