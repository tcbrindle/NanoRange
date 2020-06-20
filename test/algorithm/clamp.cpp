// nanorange/algorithm/clamp.hpp
//
//===----------------------------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include <nanorange/algorithm/clamp.hpp>

#include "../catch.hpp"

namespace ranges = nano;

struct Tag {
	Tag() : val(0), tag("Default") {}
	Tag(int a, const char *b) : val(a), tag(b) {}
	~Tag() {}

	int val;
	const char *tag;
};

TEST_CASE("alg.clamp")
{
	{
		// Default comparison and projection
		{
			int value = 0;
			int low = 0;
			int high = 0;
			CHECK(&nano::clamp(value, low, high) == &value);
			CHECK(&nano::clamp(low, value, high) == &low);
		}
		{
			int value = 0;
			int low = 1;
			int high = 2;
			CHECK(&nano::clamp(value, low, high) == &low);
			CHECK(&nano::clamp(low, value, high) == &low);
		}
		{
			int value = 1;
			int low = 0;
			int high = 1;
			CHECK(&nano::clamp(value, low, high) == &value);
			CHECK(&nano::clamp(low, value, high) == &value);
		}
	}
	{
		// Custom comparison, default projection
		{
			//  If they're all the same, we should get the value back.
			auto comp = [](const Tag& a, const Tag& b) { return a.val < b.val; };
			Tag x{0, "Zero-x"};
			Tag y{0, "Zero-y"};
			Tag z{0, "Zero-z"};
			CHECK(&nano::clamp(x, y, z, comp) ==  &x);
			CHECK(&nano::clamp(y, x, z, comp) ==  &y);
		}

		{
			//  If it's the same as the lower bound, we get the value back.
			auto comp = [](const Tag& a, const Tag& b) { return a.val < b.val; };
			Tag x{0, "Zero-x"};
			Tag y{0, "Zero-y"};
			Tag z{1, "One-z"};
			CHECK(&nano::clamp(x, y, z, comp) == &x);
			CHECK(&nano::clamp(y, x, z, comp) == &y);
		}

		{
			//  If it's the same as the upper bound, we get the value back.
			auto comp = [](const Tag& a, const Tag& b) { return a.val < b.val; };
			Tag x{1, "One-x"};
			Tag y{0, "Zero-y"};
			Tag z{1, "One-z"};
			CHECK(&nano::clamp(x, y, z, comp) == &x);
			CHECK(&nano::clamp(z, y, x, comp) == &z);
		}

		{
			//  If the value is between, we should get the value back
			auto comp = [](const Tag& a, const Tag& b) { return a.val < b.val; };
			Tag x{1, "One-x"};
			Tag y{0, "Zero-y"};
			Tag z{2, "Two-z"};
			CHECK(&nano::clamp(x, y, z, comp) == &x);
			CHECK(&nano::clamp(y, x, z, comp) == &x);
		}

		{
			//  If the value is less than the 'lo', we should get the lo back.
			auto comp = [](const Tag& a, const Tag& b) { return a.val < b.val; };
			Tag x{0, "Zero-x"};
			Tag y{1, "One-y"};
			Tag z{2, "Two-z"};
			CHECK(&nano::clamp(x, y, z, comp) == &y);
			CHECK(&nano::clamp(y, x, z, comp) == &y);
		}
		{
			//  If the value is greater than 'hi', we should get hi back.
			auto comp = [](const Tag& a, const Tag& b) { return a.val < b.val; };
			Tag x{2, "Two-x"};
			Tag y{0, "Zero-y"};
			Tag z{1, "One-z"};
			CHECK(&nano::clamp(x, y, z, comp) == &z);
			CHECK(&nano::clamp(y, z, x, comp) == &z);
		}
	}
	{
		// Default comparison, custom projection
		{
			//  If they're all the same, we should get the value back.
			Tag x{0, "Zero-x"};
			Tag y{0, "Zero-y"};
			Tag z{0, "Zero-z"};
			CHECK(&nano::clamp(x, y, z, {}, &Tag::val) ==  &x);
			CHECK(&nano::clamp(y, x, z, {}, &Tag::val) ==  &y);
		}

		{
			//  If it's the same as the lower bound, we get the value back.
			Tag x{0, "Zero-x"};
			Tag y{0, "Zero-y"};
			Tag z{1, "One-z"};
			CHECK(&nano::clamp(x, y, z, {}, &Tag::val) == &x);
			CHECK(&nano::clamp(y, x, z, {}, &Tag::val) == &y);
		}

		{
			//  If it's the same as the upper bound, we get the value back.
			Tag x{1, "One-x"};
			Tag y{0, "Zero-y"};
			Tag z{1, "One-z"};
			CHECK(&nano::clamp(x, y, z, {}, &Tag::val) == &x);
			CHECK(&nano::clamp(z, y, x, {}, &Tag::val) == &z);
		}

		{
			//  If the value is between, we should get the value back
			Tag x{1, "One-x"};
			Tag y{0, "Zero-y"};
			Tag z{2, "Two-z"};
			CHECK(&nano::clamp(x, y, z, {}, &Tag::val) == &x);
			CHECK(&nano::clamp(y, x, z, {}, &Tag::val) == &x);
		}

		{
			//  If the value is less than the 'lo', we should get the lo back.
			Tag x{0, "Zero-x"};
			Tag y{1, "One-y"};
			Tag z{2, "Two-z"};
			CHECK(&nano::clamp(x, y, z, {}, &Tag::val) == &y);
			CHECK(&nano::clamp(y, x, z, {}, &Tag::val) == &y);
		}
		{
			//  If the value is greater than 'hi', we should get hi back.
			Tag x{2, "Two-x"};
			Tag y{0, "Zero-y"};
			Tag z{1, "One-z"};
			CHECK(&nano::clamp(x, y, z, {}, &Tag::val) == &z);
			CHECK(&nano::clamp(y, z, x, {}, &Tag::val) == &z);
		}
	}
}
