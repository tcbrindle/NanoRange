// nanorange/algorithm.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_ALGORITHM_HPP_INCLUDED
#define NANORANGE_ALGORITHM_HPP_INCLUDED

// Algorithms reimplemented in Nanorange
#include <nanorange/algorithm/adjacent_find.hpp>
#include <nanorange/algorithm/all_of.hpp>
#include <nanorange/algorithm/any_of.hpp>
#include <nanorange/algorithm/copy.hpp>
#include <nanorange/algorithm/count.hpp>
#include <nanorange/algorithm/equal.hpp>
#include <nanorange/algorithm/fill.hpp>
#include <nanorange/algorithm/fill_n.hpp>
#include <nanorange/algorithm/find.hpp>
#include <nanorange/algorithm/find_end.hpp>
#include <nanorange/algorithm/find_first_of.hpp>
#include <nanorange/algorithm/for_each.hpp>
#include <nanorange/algorithm/generate.hpp>
#include <nanorange/algorithm/generate_n.hpp>
#include <nanorange/algorithm/includes.hpp>
#include <nanorange/algorithm/is_partitioned.hpp>
#include <nanorange/algorithm/is_sorted.hpp>
#include <nanorange/algorithm/is_sorted_until.hpp>
#include <nanorange/algorithm/lexicographical_compare.hpp>
#include <nanorange/algorithm/max.hpp>
#include <nanorange/algorithm/max_element.hpp>
#include <nanorange/algorithm/merge.hpp>
#include <nanorange/algorithm/min.hpp>
#include <nanorange/algorithm/min_element.hpp>
#include <nanorange/algorithm/minmax.hpp>
#include <nanorange/algorithm/minmax_element.hpp>
#include <nanorange/algorithm/mismatch.hpp>
#include <nanorange/algorithm/move.hpp>
#include <nanorange/algorithm/none_of.hpp>
#include <nanorange/algorithm/partition.hpp>
#include <nanorange/algorithm/partition_copy.hpp>
#include <nanorange/algorithm/remove.hpp>
#include <nanorange/algorithm/remove_copy.hpp>
#include <nanorange/algorithm/remove_copy_if.hpp>
#include <nanorange/algorithm/remove_if.hpp>
#include <nanorange/algorithm/replace.hpp>
#include <nanorange/algorithm/replace_copy.hpp>
#include <nanorange/algorithm/replace_copy_if.hpp>
#include <nanorange/algorithm/replace_if.hpp>
#include <nanorange/algorithm/reverse.hpp>
#include <nanorange/algorithm/reverse_copy.hpp>
#include <nanorange/algorithm/rotate.hpp>
#include <nanorange/algorithm/rotate_copy.hpp>
#include <nanorange/algorithm/search.hpp>
#include <nanorange/algorithm/search_n.hpp>
#include <nanorange/algorithm/set_difference.hpp>
#include <nanorange/algorithm/set_intersection.hpp>
#include <nanorange/algorithm/set_union.hpp>
#include <nanorange/algorithm/shuffle.hpp>
#include <nanorange/algorithm/swap_ranges.hpp>
#include <nanorange/algorithm/transform.hpp>
#include <nanorange/algorithm/unique.hpp>

// Algorithms which reuse the STL implementation
#include <nanorange/algorithm/stl/binary_search.hpp>
#include <nanorange/algorithm/stl/equal_range.hpp>
#include <nanorange/algorithm/stl/inplace_merge.hpp>
#include <nanorange/algorithm/stl/is_heap.hpp>
#include <nanorange/algorithm/stl/is_heap_until.hpp>
#include <nanorange/algorithm/stl/is_permutation.hpp>
#include <nanorange/algorithm/stl/lower_bound.hpp>
#include <nanorange/algorithm/stl/make_heap.hpp>
#include <nanorange/algorithm/stl/next_permutation.hpp>
#include <nanorange/algorithm/stl/nth_element.hpp>
#include <nanorange/algorithm/stl/partial_sort.hpp>
#include <nanorange/algorithm/stl/partial_sort_copy.hpp>
#include <nanorange/algorithm/stl/partition_point.hpp>
#include <nanorange/algorithm/stl/pop_heap.hpp>
#include <nanorange/algorithm/stl/prev_permutation.hpp>
#include <nanorange/algorithm/stl/push_heap.hpp>
#include <nanorange/algorithm/stl/set_symmetric_difference.hpp>
#include <nanorange/algorithm/stl/sort.hpp>
#include <nanorange/algorithm/stl/sort_heap.hpp>
#include <nanorange/algorithm/stl/stable_partition.hpp>
#include <nanorange/algorithm/stl/stable_sort.hpp>
#include <nanorange/algorithm/stl/unique_copy.hpp>
#include <nanorange/algorithm/stl/upper_bound.hpp>

#endif
