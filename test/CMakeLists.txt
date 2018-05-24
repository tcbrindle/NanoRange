
add_executable(test_nanorange
    catch_main.cpp
    test_concepts.cpp
    test_non_modifying_seq_ops.cpp
    #test_modifying_seq_ops.cpp
    #test_partitioning_ops.cpp
    #test_sorting_ops.cpp
    #test_type_traits.cpp

    # Range-V3/CMCSTL2 tests
    algorithm/adjacent_find.cpp
    algorithm/all_of.cpp
    algorithm/any_of.cpp
    #algorithm/binary_search.cpp
    algorithm/copy.cpp
    algorithm/copy_backward.cpp
    algorithm/copy_if.cpp
    algorithm/copy_n.cpp
    algorithm/count.cpp
    algorithm/count_if.cpp
    algorithm/equal.cpp
    #algorithm/equal_range.cpp
    #algorithm/fill.cpp
    #algorithm/fill_n.cpp
    algorithm/find.cpp
    #algorithm/find_end.cpp
    algorithm/find_first_of.cpp
    algorithm/find_if.cpp
    algorithm/find_if_not.cpp
    algorithm/for_each.cpp
    #algorithm/generate.cpp
    #algorithm/generate_n.cpp
    #algorithm/includes.cpp
    #algorithm/inplace_merge.cpp
    #algorithm/is_heap1.cpp
    #algorithm/is_heap2.cpp
    #algorithm/is_heap3.cpp
    #algorithm/is_heap4.cpp
    #algorithm/is_heap_until1.cpp
    #algorithm/is_heap_until2.cpp
    #algorithm/is_heap_until3.cpp
    #algorithm/is_heap_until4.cpp
    #algorithm/is_partitioned.cpp
    #algorithm/is_permutation.cpp
    #algorithm/is_sorted.cpp
    #algorithm/is_sorted_until.cpp
    #algorithm/lexicographical_compare.cpp
    #algorithm/lower_bound.cpp
    #algorithm/make_heap.cpp
    #algorithm/max.cpp
    #algorithm/max_element.cpp
    #algorithm/merge.cpp
    #algorithm/min.cpp
    #algorithm/min_element.cpp
    #algorithm/minmax.cpp
    #algorithm/minmax_element.cpp
    algorithm/mismatch.cpp
    algorithm/move.cpp
    algorithm/move_backward.cpp
    #algorithm/next_permutation.cpp
    algorithm/none_of.cpp
    #algorithm/nth_element.cpp
    #algorithm/partial_sort.cpp
    #algorithm/partial_sort_copy.cpp
    #algorithm/partition.cpp
    #algorithm/partition_copy.cpp
    #algorithm/partition_point.cpp
    #algorithm/pop_heap.cpp
    #algorithm/prev_permutation.cpp
    #algorithm/push_heap.cpp
    #algorithm/remove.cpp
    #algorithm/remove_copy.cpp
    #algorithm/remove_copy_if.cpp
    #algorithm/remove_if.cpp
    #algorithm/replace.cpp
    #algorithm/replace_copy.cpp
    #algorithm/replace_copy_if.cpp
    #algorithm/replace_if.cpp
    #algorithm/reverse.cpp
    #algorithm/reverse_copy.cpp
    #algorithm/rotate.cpp
    #algorithm/rotate_copy.cpp
    #algorithm/sample.cpp
    algorithm/search.cpp
    algorithm/search_n.cpp
    #algorithm/set_difference1.cpp
    #algorithm/set_difference2.cpp
    #algorithm/set_difference3.cpp
    #algorithm/set_difference4.cpp
    #algorithm/set_difference5.cpp
    #algorithm/set_difference6.cpp
    #algorithm/set_intersection1.cpp
    #algorithm/set_intersection2.cpp
    #algorithm/set_intersection3.cpp
    #algorithm/set_intersection4.cpp
    #algorithm/set_intersection5.cpp
    #algorithm/set_intersection6.cpp
    #algorithm/set_symmetric_difference1.cpp
    #algorithm/set_symmetric_difference2.cpp
    #algorithm/set_symmetric_difference3.cpp
    #algorithm/set_symmetric_difference4.cpp
    #algorithm/set_symmetric_difference5.cpp
    #algorithm/set_symmetric_difference6.cpp
    #algorithm/set_union1.cpp
    #algorithm/set_union2.cpp
    #algorithm/set_union3.cpp
    #algorithm/set_union4.cpp
    #algorithm/set_union5.cpp
    #algorithm/set_union6.cpp
    #algorithm/shuffle.cpp
    #algorithm/sort.cpp
    #algorithm/sort_heap.cpp
    #algorithm/stable_partition.cpp
    #algorithm/stable_sort.cpp
    algorithm/swap_ranges.cpp
    #algorithm/transform.cpp
    #algorithm/unique.cpp
    #algorithm/unique_copy.cpp
    #algorithm/upper_bound.cpp

    concepts/compare.cpp
    concepts/core.cpp
    concepts/fundamental.cpp
    concepts/iterator.cpp
    concepts/object.cpp
    concepts/range.cpp
    concepts/swap.cpp

    functional/invoke.cpp

    iterator/common_iterator.cpp
    #iterator/counted_iterator.cpp
    #iterator/incomplete.cpp
    #iterator/istream_iterator.cpp
    #iterator/istreambuf_iterator.cpp
    #iterator/iterator.cpp
    #iterator/move_iterator.cpp
    iterator/operations.cpp
    iterator/ostream_iterator.cpp
    iterator/ostreambuf_iterator.cpp
    #iterator/reverse_iterator.cpp
    iterator/unreachable.cpp

    range_access.cpp

    utility/common_type.cpp
    utility/concepts.cpp

    #view/bounded_view.cpp
    #view/counted_view.cpp
    #view/filter_view.cpp
    #view/join_view.cpp
    #view/move_view.cpp
    #view/repeat_n_view.cpp
    #view/repeat_view.cpp
    #view/reverse_view.cpp
    #view/span.cpp
    #view/split_view.cpp
    view/subrange.cpp
    #view/take_view.cpp
    #view/transform_view.cpp
)
target_compile_definitions(test_nanorange PRIVATE "-DNANORANGE_NO_DEPRECATION_WARNINGS")
target_link_libraries(test_nanorange PRIVATE nanorange)

if (CMAKE_COMPILER_IS_GNUCXX OR CMAKE_CXX_COMPILER_ID MATCHES "Clang")
    target_compile_options(test_nanorange PRIVATE -Wall -Wextra)
endif()

if (CMAKE_CXX_COMPILER_ID MATCHES "Clang")
    target_compile_options(test_nanorange PRIVATE "-Wno-unneeded-internal-declaration")
endif()

