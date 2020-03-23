
cmake_minimum_required(VERSION 3.1)

project(nanorange CXX)

set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_SOURCE_DIR}/cmake")

option(ENABLE_BENCHMARKS "Build benchmarking support" Off)
option(BUILD_TOOLS "Build tools" $ENV{BUILD_SINGLE_HEADER})
option(USE_SINGLE_HEADER "Use single header" FALSE)
option(DO_INSTALL "Install the library" FALSE)

set(CMAKE_CXX_EXTENSIONS Off)

add_library(nanorange INTERFACE)
if (USE_SINGLE_HEADER)
    target_include_directories(nanorange
    INTERFACE
        $<BUILD_INTERFACE:${CMAKE_CURRENT_LIST_DIR}/single_include>
        $<INSTALL_INTERFACE:${CMAKE_INSTALL_INCLUDEDIR}>
    )
else ()
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
        ${CMAKE_CURRENT_SOURCE_DIR}/include/nanorange/algorithm/inplace_merge.hpp
        ${CMAKE_CURRENT_SOURCE_DIR}/include/nanorange/algorithm/is_heap.hpp
        ${CMAKE_CURRENT_SOURCE_DIR}/include/nanorange/algorithm/is_heap_until.hpp
        ${CMAKE_CURRENT_SOURCE_DIR}/include/nanorange/algorithm/is_partitioned.hpp
        ${CMAKE_CURRENT_SOURCE_DIR}/include/nanorange/algorithm/is_permutation.hpp
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
        ${CMAKE_CURRENT_SOURCE_DIR}/include/nanorange/algorithm/nth_element.hpp
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
        ${CMAKE_CURRENT_SOURCE_DIR}/include/nanorange/algorithm/stable_partition.hpp
        ${CMAKE_CURRENT_SOURCE_DIR}/include/nanorange/algorithm/stable_sort.hpp
        ${CMAKE_CURRENT_SOURCE_DIR}/include/nanorange/algorithm/swap_ranges.hpp
        ${CMAKE_CURRENT_SOURCE_DIR}/include/nanorange/algorithm/transform.hpp
        ${CMAKE_CURRENT_SOURCE_DIR}/include/nanorange/algorithm/unique.hpp
        ${CMAKE_CURRENT_SOURCE_DIR}/include/nanorange/algorithm/unique_copy.hpp
        ${CMAKE_CURRENT_SOURCE_DIR}/include/nanorange/algorithm/upper_bound.hpp

        ${CMAKE_CURRENT_SOURCE_DIR}/include/nanorange/detail/algorithm/heap_sift.hpp
        ${CMAKE_CURRENT_SOURCE_DIR}/include/nanorange/detail/algorithm/pdqsort.hpp
        ${CMAKE_CURRENT_SOURCE_DIR}/include/nanorange/detail/concepts/comparison.hpp
        ${CMAKE_CURRENT_SOURCE_DIR}/include/nanorange/detail/concepts/core.hpp
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
        ${CMAKE_CURRENT_SOURCE_DIR}/include/nanorange/detail/memory/concepts.hpp
        ${CMAKE_CURRENT_SOURCE_DIR}/include/nanorange/detail/memory/temporary_vector.hpp
        ${CMAKE_CURRENT_SOURCE_DIR}/include/nanorange/detail/ranges/access.hpp
        ${CMAKE_CURRENT_SOURCE_DIR}/include/nanorange/detail/ranges/begin_end.hpp
        ${CMAKE_CURRENT_SOURCE_DIR}/include/nanorange/detail/ranges/concepts.hpp
        ${CMAKE_CURRENT_SOURCE_DIR}/include/nanorange/detail/ranges/primitives.hpp
        ${CMAKE_CURRENT_SOURCE_DIR}/include/nanorange/detail/views/range_adaptors.hpp
        ${CMAKE_CURRENT_SOURCE_DIR}/include/nanorange/detail/views/semiregular_box.hpp
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

        ${CMAKE_CURRENT_SOURCE_DIR}/include/nanorange/memory/destroy.hpp
        ${CMAKE_CURRENT_SOURCE_DIR}/include/nanorange/memory/uninitialized_copy.hpp
        ${CMAKE_CURRENT_SOURCE_DIR}/include/nanorange/memory/uninitialized_default_construct.hpp
        ${CMAKE_CURRENT_SOURCE_DIR}/include/nanorange/memory/uninitialized_fill.hpp
        ${CMAKE_CURRENT_SOURCE_DIR}/include/nanorange/memory/uninitialized_move.hpp
        ${CMAKE_CURRENT_SOURCE_DIR}/include/nanorange/memory/uninitialized_value_construct.hpp

        ${CMAKE_CURRENT_SOURCE_DIR}/include/nanorange/views/all.hpp
        ${CMAKE_CURRENT_SOURCE_DIR}/include/nanorange/views/common.hpp
        ${CMAKE_CURRENT_SOURCE_DIR}/include/nanorange/views/counted.hpp
        ${CMAKE_CURRENT_SOURCE_DIR}/include/nanorange/views/drop.hpp
        ${CMAKE_CURRENT_SOURCE_DIR}/include/nanorange/views/drop_while.hpp
        ${CMAKE_CURRENT_SOURCE_DIR}/include/nanorange/views/elements.hpp
        ${CMAKE_CURRENT_SOURCE_DIR}/include/nanorange/views/empty.hpp
        ${CMAKE_CURRENT_SOURCE_DIR}/include/nanorange/views/filter.hpp
        ${CMAKE_CURRENT_SOURCE_DIR}/include/nanorange/views/interface.hpp
        ${CMAKE_CURRENT_SOURCE_DIR}/include/nanorange/views/iota.hpp
        ${CMAKE_CURRENT_SOURCE_DIR}/include/nanorange/views/istream.hpp
        ${CMAKE_CURRENT_SOURCE_DIR}/include/nanorange/views/join.hpp
        ${CMAKE_CURRENT_SOURCE_DIR}/include/nanorange/views/ref.hpp
        ${CMAKE_CURRENT_SOURCE_DIR}/include/nanorange/views/reverse.hpp
        ${CMAKE_CURRENT_SOURCE_DIR}/include/nanorange/views/single.hpp
        ${CMAKE_CURRENT_SOURCE_DIR}/include/nanorange/views/split.hpp
        ${CMAKE_CURRENT_SOURCE_DIR}/include/nanorange/views/subrange.hpp
        ${CMAKE_CURRENT_SOURCE_DIR}/include/nanorange/views/take.hpp
        ${CMAKE_CURRENT_SOURCE_DIR}/include/nanorange/views/take_while.hpp
        ${CMAKE_CURRENT_SOURCE_DIR}/include/nanorange/views/transform.hpp

        ${CMAKE_CURRENT_SOURCE_DIR}/include/nanorange/algorithm.hpp
        ${CMAKE_CURRENT_SOURCE_DIR}/include/nanorange/concepts.hpp
        ${CMAKE_CURRENT_SOURCE_DIR}/include/nanorange/functional.hpp
        ${CMAKE_CURRENT_SOURCE_DIR}/include/nanorange/iterator.hpp
        ${CMAKE_CURRENT_SOURCE_DIR}/include/nanorange/memory.hpp
        ${CMAKE_CURRENT_SOURCE_DIR}/include/nanorange/random.hpp
        ${CMAKE_CURRENT_SOURCE_DIR}/include/nanorange/ranges.hpp
        ${CMAKE_CURRENT_SOURCE_DIR}/include/nanorange/type_traits.hpp
        ${CMAKE_CURRENT_SOURCE_DIR}/include/nanorange/utility.hpp
        ${CMAKE_CURRENT_SOURCE_DIR}/include/nanorange/views.hpp
    )
    target_include_directories(nanorange INTERFACE ${CMAKE_CURRENT_SOURCE_DIR}/include)
endif()

target_compile_features(nanorange INTERFACE cxx_std_17)

if (MSVC)
    target_compile_options(nanorange INTERFACE /permissive-)
endif()

if (DO_INSTALL)
    set(NANORANGE_CMAKE_CONFIG_DESTINATION "${CMAKE_INSTALL_LIBDIR}/cmake/nanorange")

    include(CMakePackageConfigHelpers)
    configure_package_config_file(
        ${CMAKE_CURRENT_LIST_DIR}/cmake/nanorangeConfig.cmake.in
        ${CMAKE_CURRENT_BINARY_DIR}/nanorangeConfig.cmake
        INSTALL_DESTINATION
            ${NANORANGE_CMAKE_CONFIG_DESTINATION}
    )


    # create and install an export set for nanorange target as nanorange::nanorange
    install(
        TARGETS
            nanorange
        EXPORT
            nanorangeTargets
        DESTINATION
            ${CMAKE_INSTALL_LIBDIR}
    )


    install(
        EXPORT
            nanorangeTargets
        NAMESPACE
            nanorange::
        DESTINATION
            ${NANORANGE_CMAKE_CONFIG_DESTINATION}
    )

    # By default, FooConfigVersion is tied to architecture that it was
    # generated on. Because nanorange is header-only, it is arch-independent
    # and thus nanorangeConfigVersion should not be tied to the architecture
    # it was generated on.
    #
    # CMake does not provide a direct customization point for this in
    # `write_basic_package_version_file`, but it can be accomplished
    # indirectly by temporarily redefining `CMAKE_SIZEOF_VOID_P` to an
    # empty string. Note that just undefining the variable could be
    # insufficient in cases where the variable was already in CMake cache
    # set(NANORANGE_CMAKE_SIZEOF_VOID_P ${CMAKE_SIZEOF_VOID_P})
    # set(CMAKE_SIZEOF_VOID_P "")
    # write_basic_package_version_file(
    #   "${CMAKE_CURRENT_BINARY_DIR}/nanorangeConfigVersion.cmake"
    #   COMPATIBILITY
    #     SameMajorVersion
    # )
    # set(CMAKE_SIZEOF_VOID_P ${NANORANGE_CMAKE_SIZEOF_VOID_P})

    install(
        DIRECTORY
            "single_include/"
        DESTINATION
            "${CMAKE_INSTALL_INCLUDEDIR}"
    )
    
    install(
        FILES
            "${CMAKE_CURRENT_BINARY_DIR}/nanorangeConfig.cmake"
            # "${CMAKE_CURRENT_BINARY_DIR}/nanorangeConfigVersion.cmake"
        DESTINATION
            ${NANORANGE_CMAKE_CONFIG_DESTINATION}
    )

    install(
        FILES
            LICENSE_1_0.txt
        DESTINATION
            "${CMAKE_INSTALL_PREFIX}/licenses"
    )
endif()

if (ENABLE_BENCHMARKS)
    add_subdirectory(benchmarks)
endif()

include(CTest)
if (BUILD_TESTING)
    add_subdirectory(test)
endif()

if (BUILD_TOOLS)
    add_subdirectory(tools)
endif()
