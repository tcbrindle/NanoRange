
cmake_minimum_required(VERSION 3.1)

project(nanorange CXX)

set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_SOURCE_DIR}/cmake")

enable_testing()

set(CMAKE_CXX_EXTENSIONS Off)

add_library(nanorange INTERFACE)
target_sources(nanorange INTERFACE
        ${CMAKE_CURRENT_SOURCE_DIR}/include/nanorange.hpp

        ${CMAKE_CURRENT_SOURCE_DIR}/include/nanorange/algorithm/adjacent_find.hpp
        ${CMAKE_CURRENT_SOURCE_DIR}/include/nanorange/algorithm/all_of.hpp
        ${CMAKE_CURRENT_SOURCE_DIR}/include/nanorange/algorithm/any_of.hpp
        ${CMAKE_CURRENT_SOURCE_DIR}/include/nanorange/algorithm/binary_search.hpp
        ${CMAKE_CURRENT_SOURCE_DIR}/include/nanorange/algorithm/copy.hpp
        ${CMAKE_CURRENT_SOURCE_DIR}/include/nanorange/algorithm/count.hpp
        ${CMAKE_CURRENT_SOURCE_DIR}/include/nanorange/algorithm/equal.hpp
        ${CMAKE_CURRENT_SOURCE_DIR}/include/nanorange/algorithm/equal_range.hpp
        ${CMAKE_CURRENT_SOURCE_DIR}/include/nanorange/algorithm/fill.hpp
        ${CMAKE_CURRENT_SOURCE_DIR}/include/nanorange/algorithm/fill_n.hpp
        ${CMAKE_CURRENT_SOURCE_DIR}/include/nanorange/algorithm/find.hpp
        ${CMAKE_CURRENT_SOURCE_DIR}/include/nanorange/algorithm/find_end.hpp
        ${CMAKE_CURRENT_SOURCE_DIR}/include/nanorange/algorithm/find_first_of.hpp
        ${CMAKE_CURRENT_SOURCE_DIR}/include/nanorange/algorithm/for_each.hpp
        ${CMAKE_CURRENT_SOURCE_DIR}/include/nanorange/algorithm/generate.hpp
        ${CMAKE_CURRENT_SOURCE_DIR}/include/nanorange/algorithm/generate_n.hpp
        ${CMAKE_CURRENT_SOURCE_DIR}/include/nanorange/algorithm/includes.hpp
        ${CMAKE_CURRENT_SOURCE_DIR}/include/nanorange/algorithm/is_heap.hpp
        ${CMAKE_CURRENT_SOURCE_DIR}/include/nanorange/algorithm/is_heap_until.hpp
        ${CMAKE_CURRENT_SOURCE_DIR}/include/nanorange/algorithm/is_partitioned.hpp
        ${CMAKE_CURRENT_SOURCE_DIR}/include/nanorange/algorithm/is_sorted.hpp
        ${CMAKE_CURRENT_SOURCE_DIR}/include/nanorange/algorithm/is_sorted_until.hpp
        ${CMAKE_CURRENT_SOURCE_DIR}/include/nanorange/algorithm/lexicographical_compare.hpp
        ${CMAKE_CURRENT_SOURCE_DIR}/include/nanorange/algorithm/lower_bound.hpp
        ${CMAKE_CURRENT_SOURCE_DIR}/include/nanorange/algorithm/make_heap.hpp
        ${CMAKE_CURRENT_SOURCE_DIR}/include/nanorange/algorithm/max.hpp
        ${CMAKE_CURRENT_SOURCE_DIR}/include/nanorange/algorithm/max_element.hpp
        ${CMAKE_CURRENT_SOURCE_DIR}/include/nanorange/algorithm/merge.hpp
        ${CMAKE_CURRENT_SOURCE_DIR}/include/nanorange/algorithm/min.hpp
        ${CMAKE_CURRENT_SOURCE_DIR}/include/nanorange/algorithm/min_element.hpp
        ${CMAKE_CURRENT_SOURCE_DIR}/include/nanorange/algorithm/minmax.hpp
        ${CMAKE_CURRENT_SOURCE_DIR}/include/nanorange/algorithm/minmax_element.hpp
        ${CMAKE_CURRENT_SOURCE_DIR}/include/nanorange/algorithm/mismatch.hpp
        ${CMAKE_CURRENT_SOURCE_DIR}/include/nanorange/algorithm/move.hpp
        ${CMAKE_CURRENT_SOURCE_DIR}/include/nanorange/algorithm/next_permutation.hpp
        ${CMAKE_CURRENT_SOURCE_DIR}/include/nanorange/algorithm/none_of.hpp
        ${CMAKE_CURRENT_SOURCE_DIR}/include/nanorange/algorithm/partial_sort.hpp
        ${CMAKE_CURRENT_SOURCE_DIR}/include/nanorange/algorithm/partial_sort_copy.hpp
        ${CMAKE_CURRENT_SOURCE_DIR}/include/nanorange/algorithm/partition.hpp
        ${CMAKE_CURRENT_SOURCE_DIR}/include/nanorange/algorithm/partition_copy.hpp
        ${CMAKE_CURRENT_SOURCE_DIR}/include/nanorange/algorithm/partition_point.hpp
        ${CMAKE_CURRENT_SOURCE_DIR}/include/nanorange/algorithm/pop_heap.hpp
        ${CMAKE_CURRENT_SOURCE_DIR}/include/nanorange/algorithm/prev_permutation.hpp
        ${CMAKE_CURRENT_SOURCE_DIR}/include/nanorange/algorithm/push_heap.hpp
        ${CMAKE_CURRENT_SOURCE_DIR}/include/nanorange/algorithm/remove.hpp
        ${CMAKE_CURRENT_SOURCE_DIR}/include/nanorange/algorithm/remove_copy.hpp
        ${CMAKE_CURRENT_SOURCE_DIR}/include/nanorange/algorithm/remove_copy_if.hpp
        ${CMAKE_CURRENT_SOURCE_DIR}/include/nanorange/algorithm/remove_if.hpp
        ${CMAKE_CURRENT_SOURCE_DIR}/include/nanorange/algorithm/replace.hpp
        ${CMAKE_CURRENT_SOURCE_DIR}/include/nanorange/algorithm/replace_copy.hpp
        ${CMAKE_CURRENT_SOURCE_DIR}/include/nanorange/algorithm/replace_copy_if.hpp
        ${CMAKE_CURRENT_SOURCE_DIR}/include/nanorange/algorithm/replace_if.hpp
        ${CMAKE_CURRENT_SOURCE_DIR}/include/nanorange/algorithm/reverse.hpp
        ${CMAKE_CURRENT_SOURCE_DIR}/include/nanorange/algorithm/reverse_copy.hpp
        ${CMAKE_CURRENT_SOURCE_DIR}/include/nanorange/algorithm/rotate.hpp
        ${CMAKE_CURRENT_SOURCE_DIR}/include/nanorange/algorithm/rotate_copy.hpp
        ${CMAKE_CURRENT_SOURCE_DIR}/include/nanorange/algorithm/search.hpp
        ${CMAKE_CURRENT_SOURCE_DIR}/include/nanorange/algorithm/search_n.hpp
        ${CMAKE_CURRENT_SOURCE_DIR}/include/nanorange/algorithm/set_difference.hpp
        ${CMAKE_CURRENT_SOURCE_DIR}/include/nanorange/algorithm/set_intersection.hpp
        ${CMAKE_CURRENT_SOURCE_DIR}/include/nanorange/algorithm/set_symmetric_difference.hpp
        ${CMAKE_CURRENT_SOURCE_DIR}/include/nanorange/algorithm/set_union.hpp
        ${CMAKE_CURRENT_SOURCE_DIR}/include/nanorange/algorithm/shuffle.hpp
        ${CMAKE_CURRENT_SOURCE_DIR}/include/nanorange/algorithm/sort.hpp
        ${CMAKE_CURRENT_SOURCE_DIR}/include/nanorange/algorithm/sort_heap.hpp
        ${CMAKE_CURRENT_SOURCE_DIR}/include/nanorange/algorithm/swap_ranges.hpp
        ${CMAKE_CURRENT_SOURCE_DIR}/include/nanorange/algorithm/transform.hpp
        ${CMAKE_CURRENT_SOURCE_DIR}/include/nanorange/algorithm/unique.hpp
        ${CMAKE_CURRENT_SOURCE_DIR}/include/nanorange/algorithm/upper_bound.hpp

        ${CMAKE_CURRENT_SOURCE_DIR}/include/nanorange/algorithm/stl/inplace_merge.hpp
        ${CMAKE_CURRENT_SOURCE_DIR}/include/nanorange/algorithm/stl/is_permutation.hpp
        ${CMAKE_CURRENT_SOURCE_DIR}/include/nanorange/algorithm/stl/nth_element.hpp
        ${CMAKE_CURRENT_SOURCE_DIR}/include/nanorange/algorithm/stl/stable_partition.hpp
        ${CMAKE_CURRENT_SOURCE_DIR}/include/nanorange/algorithm/stl/stable_sort.hpp
        ${CMAKE_CURRENT_SOURCE_DIR}/include/nanorange/algorithm/stl/unique_copy.hpp

        ${CMAKE_CURRENT_SOURCE_DIR}/include/nanorange/detail/algorithm/heap_sift.hpp
        ${CMAKE_CURRENT_SOURCE_DIR}/include/nanorange/detail/algorithm/pdqsort.hpp
        ${CMAKE_CURRENT_SOURCE_DIR}/include/nanorange/detail/concepts/comparison.hpp
        ${CMAKE_CURRENT_SOURCE_DIR}/include/nanorange/detail/concepts/core.hpp
        ${CMAKE_CURRENT_SOURCE_DIR}/include/nanorange/detail/concepts/movable.hpp
        ${CMAKE_CURRENT_SOURCE_DIR}/include/nanorange/detail/concepts/object.hpp
        ${CMAKE_CURRENT_SOURCE_DIR}/include/nanorange/detail/concepts/swappable.hpp
        ${CMAKE_CURRENT_SOURCE_DIR}/include/nanorange/detail/functional/comparisons.hpp
        ${CMAKE_CURRENT_SOURCE_DIR}/include/nanorange/detail/functional/decay_copy.hpp
        ${CMAKE_CURRENT_SOURCE_DIR}/include/nanorange/detail/functional/identity.hpp
        ${CMAKE_CURRENT_SOURCE_DIR}/include/nanorange/detail/functional/invoke.hpp
        ${CMAKE_CURRENT_SOURCE_DIR}/include/nanorange/detail/iterator/algorithm_requirements.hpp
        ${CMAKE_CURRENT_SOURCE_DIR}/include/nanorange/detail/iterator/associated_types.hpp
        ${CMAKE_CURRENT_SOURCE_DIR}/include/nanorange/detail/iterator/concepts.hpp
        ${CMAKE_CURRENT_SOURCE_DIR}/include/nanorange/detail/iterator/dereferenceable.hpp
        ${CMAKE_CURRENT_SOURCE_DIR}/include/nanorange/detail/iterator/indirect_callable_concepts.hpp
        ${CMAKE_CURRENT_SOURCE_DIR}/include/nanorange/detail/iterator/iter_move.hpp
        ${CMAKE_CURRENT_SOURCE_DIR}/include/nanorange/detail/iterator/iter_swap.hpp
        ${CMAKE_CURRENT_SOURCE_DIR}/include/nanorange/detail/iterator/projected.hpp
        ${CMAKE_CURRENT_SOURCE_DIR}/include/nanorange/detail/iterator/traits.hpp
        ${CMAKE_CURRENT_SOURCE_DIR}/include/nanorange/detail/range/access.hpp
        ${CMAKE_CURRENT_SOURCE_DIR}/include/nanorange/detail/range/concepts.hpp
        ${CMAKE_CURRENT_SOURCE_DIR}/include/nanorange/detail/range/dangling.hpp
        ${CMAKE_CURRENT_SOURCE_DIR}/include/nanorange/detail/range/primitives.hpp
        ${CMAKE_CURRENT_SOURCE_DIR}/include/nanorange/detail/common_reference.hpp
        ${CMAKE_CURRENT_SOURCE_DIR}/include/nanorange/detail/common_type.hpp
        ${CMAKE_CURRENT_SOURCE_DIR}/include/nanorange/detail/macros.hpp
        ${CMAKE_CURRENT_SOURCE_DIR}/include/nanorange/detail/swap.hpp
        ${CMAKE_CURRENT_SOURCE_DIR}/include/nanorange/detail/type_traits.hpp

        ${CMAKE_CURRENT_SOURCE_DIR}/include/nanorange/iterator/back_insert_iterator.hpp
        ${CMAKE_CURRENT_SOURCE_DIR}/include/nanorange/iterator/common_iterator.hpp
        ${CMAKE_CURRENT_SOURCE_DIR}/include/nanorange/iterator/concepts.hpp
        ${CMAKE_CURRENT_SOURCE_DIR}/include/nanorange/iterator/counted_iterator.hpp
        ${CMAKE_CURRENT_SOURCE_DIR}/include/nanorange/iterator/default_sentinel.hpp
        ${CMAKE_CURRENT_SOURCE_DIR}/include/nanorange/iterator/front_insert_iterator.hpp
        ${CMAKE_CURRENT_SOURCE_DIR}/include/nanorange/iterator/insert_iterator.hpp
        ${CMAKE_CURRENT_SOURCE_DIR}/include/nanorange/iterator/istream_iterator.hpp
        ${CMAKE_CURRENT_SOURCE_DIR}/include/nanorange/iterator/istreambuf_iterator.hpp
        ${CMAKE_CURRENT_SOURCE_DIR}/include/nanorange/iterator/move_iterator.hpp
        ${CMAKE_CURRENT_SOURCE_DIR}/include/nanorange/iterator/operations.hpp
        ${CMAKE_CURRENT_SOURCE_DIR}/include/nanorange/iterator/ostream_iterator.hpp
        ${CMAKE_CURRENT_SOURCE_DIR}/include/nanorange/iterator/ostreambuf_iterator.hpp
        ${CMAKE_CURRENT_SOURCE_DIR}/include/nanorange/iterator/reverse_iterator.hpp
        ${CMAKE_CURRENT_SOURCE_DIR}/include/nanorange/iterator/unreachable.hpp

        ${CMAKE_CURRENT_SOURCE_DIR}/include/nanorange/view/interface.hpp
        ${CMAKE_CURRENT_SOURCE_DIR}/include/nanorange/view/subrange.hpp

        ${CMAKE_CURRENT_SOURCE_DIR}/include/nanorange/algorithm.hpp
        ${CMAKE_CURRENT_SOURCE_DIR}/include/nanorange/concepts.hpp
        ${CMAKE_CURRENT_SOURCE_DIR}/include/nanorange/functional.hpp
        ${CMAKE_CURRENT_SOURCE_DIR}/include/nanorange/iterator.hpp
        ${CMAKE_CURRENT_SOURCE_DIR}/include/nanorange/random.hpp
        ${CMAKE_CURRENT_SOURCE_DIR}/include/nanorange/range.hpp
        ${CMAKE_CURRENT_SOURCE_DIR}/include/nanorange/type_traits.hpp
        ${CMAKE_CURRENT_SOURCE_DIR}/include/nanorange/utility.hpp
        ${CMAKE_CURRENT_SOURCE_DIR}/include/nanorange/view.hpp
)
target_include_directories(nanorange INTERFACE ${CMAKE_CURRENT_SOURCE_DIR}/include)
target_compile_features(nanorange INTERFACE cxx_std_14)


if (MSVC)
    target_compile_options(nanorange INTERFACE /permissive-)
endif()

add_subdirectory(test)
add_subdirectory(tools)
