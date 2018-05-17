// Range v3 library
//
//  Copyright Eric Niebler 2014
//
//  Use, modification and distribution is subject to the
//  Boost Software License, Version 1.0. (See accompanying
//  file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt)
//
// Project home: https://github.com/ericniebler/range-v3
//

#include "set_union.hpp"

int main()
{
	test<forward_iterator<const int*>, input_iterator<const int*>, output_iterator<int*> >();
	test<forward_iterator<const int*>, input_iterator<const int*>, forward_iterator<int*> >();
	test<forward_iterator<const int*>, input_iterator<const int*>, bidirectional_iterator<int*> >();
	test<forward_iterator<const int*>, input_iterator<const int*>, random_access_iterator<int*> >();
	test<forward_iterator<const int*>, input_iterator<const int*>, int*>();

	test<forward_iterator<const int*>, forward_iterator<const int*>, output_iterator<int*> >();
	test<forward_iterator<const int*>, forward_iterator<const int*>, forward_iterator<int*> >();
	test<forward_iterator<const int*>, forward_iterator<const int*>, bidirectional_iterator<int*> >();
	test<forward_iterator<const int*>, forward_iterator<const int*>, random_access_iterator<int*> >();
	test<forward_iterator<const int*>, forward_iterator<const int*>, int*>();

	test<forward_iterator<const int*>, bidirectional_iterator<const int*>, output_iterator<int*> >();
	test<forward_iterator<const int*>, bidirectional_iterator<const int*>, forward_iterator<int*> >();
	test<forward_iterator<const int*>, bidirectional_iterator<const int*>, bidirectional_iterator<int*> >();
	test<forward_iterator<const int*>, bidirectional_iterator<const int*>, random_access_iterator<int*> >();
	test<forward_iterator<const int*>, bidirectional_iterator<const int*>, int*>();

	test<forward_iterator<const int*>, random_access_iterator<const int*>, output_iterator<int*> >();
	test<forward_iterator<const int*>, random_access_iterator<const int*>, forward_iterator<int*> >();
	test<forward_iterator<const int*>, random_access_iterator<const int*>, bidirectional_iterator<int*> >();
	test<forward_iterator<const int*>, random_access_iterator<const int*>, random_access_iterator<int*> >();
	test<forward_iterator<const int*>, random_access_iterator<const int*>, int*>();

	test<forward_iterator<const int*>, const int*, output_iterator<int*> >();
	test<forward_iterator<const int*>, const int*, forward_iterator<int*> >();
	test<forward_iterator<const int*>, const int*, bidirectional_iterator<int*> >();
	test<forward_iterator<const int*>, const int*, random_access_iterator<int*> >();
	test<forward_iterator<const int*>, const int*, int*>();

	return ::test_result();
}
