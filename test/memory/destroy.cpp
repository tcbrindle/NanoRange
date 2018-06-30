// cmcstl2 - A concept-enabled C++ standard library
//
//  Copyright Casey Carter 2016
//  Copyright Christopher Di Bella 2016
//
//  Use, modification and distribution is subject to the
//  Boost Software License, Version 1.0. (See accompanying
//  file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt)
//
// Project home: https://github.com/caseycarter/cmcstl2
//
#include <stl2/detail/memory/destroy.hpp>
#include <stl2/detail/memory/uninitialized_default_construct.hpp>
#include "../simple_test.hpp"
#include "common.hpp"

namespace ranges = __stl2;

namespace {
	class Construct {
	public:
		Construct()
		{
			++instantiated;
		}

		Construct(const Construct&)
		{
			++instantiated;
		}

		Construct(Construct&&) noexcept
		{
			++instantiated;
		}

		~Construct()
		{
			--instantiated;
		}

		static int get_instantiated()
		{
			return instantiated;
		}

		Construct& operator=(Construct&&) noexcept = default;
		Construct& operator=(const Construct&) noexcept = default;
	private:
		static int instantiated;
	};

	int Construct::instantiated{0};
}

int main()
{
	auto independent = make_buffer<Construct>(1 << 20);
	auto test = [&independent](const auto& p) {
		CHECK(Construct::get_instantiated() == 0);
		CHECK(p == independent.end());
	};

	ranges::uninitialized_default_construct(independent);
	test(ranges::destroy(independent.begin(), independent.end()));

	ranges::uninitialized_default_construct(independent);
	test(ranges::destroy(independent.cbegin(), independent.cend()));

	ranges::uninitialized_default_construct(independent);
	test(ranges::destroy(independent));

	ranges::uninitialized_default_construct(independent);
	test(ranges::destroy_n(independent.begin(), independent.size()));

	ranges::uninitialized_default_construct(independent);
	test(ranges::destroy_n(independent.cbegin(), independent.size()));

	return ::test_result();
}
