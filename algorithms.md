# Algorithms in NanoRange #

## Fully implemented ##

The following algorithms are fully reimplemented in Nanorange,
meeting the requirements of the Ranges papers. In particular, they accept
iterators and sentinels of different types, and allow the use of projections.
In addition, they are all available for use in `constexpr` functions.

#### Non-modifying sequence ops ####

* all_of
* any_of
* none_of
* for_each
* find
* find_if
* find_if_not
* find_end
* find_first_of
* adjacent_find
* count
* count_if
* mismatch
* equal
* search
* search_n
* lexicographical_compare

#### Modifying sequence ops ####

* copy
* copy_n
* copy_if
* copy_backward
* move
* move_backward
* swap_ranges
* transform
* replace
* replace_if
* replace_copy
* replace_copy_if
* fill
* fill_n
* generate
* generate_n
* remove
* remove_if
* remove_copy
* remove_copy_if
* unique
* reverse
* reverse_copy
* rotate
* rotate_copy
* shuffle

#### Partition operations ####

* is_partitioned
* partition
* partition_copy

#### Sorting ####

* is_sorted
* is_sorted_until

#### Minimum and maximum ####

* min
* max
* minmax
* min_element
* max_element
* minmax_element

## Partially implemented (using STL) ##

The following algorithms are implemented in Nanorange as wrappers around the
existing standard library implementation. Constraints and return types are based
on the C++98 specification. This means that the iterator and sentinel types
must be the same, or for the range-based overloads, the range must model
CommonRange. Projections cannot be used.

### Non-modifying sequence ops ##

* is_permutation


## Unimplemented ##

The following algorithms have not yet been implemented in Nanorange:

#### Modifying sequence ops ####

* unique_copy

#### partitions ####

* stable_partition
* partition_point

#### sorting ####
* sort
* stable_sort
* partial_sort
* partial_sort_copy
* nth_element

#### binary search ####

* lower_bound
* upper_bound
* equal_range
* binary_search

#### merge ####

* merge
* inplace_merge

#### set operations ####

* includes
* set_union
* set_intersection
* set_difference
* set_symmetric_difference

#### heap operations ####

* push_heap
* pop_heap
* make_heap
* sort_heap
* is_heap
* is_heap_until

#### permutations ####

* next_permutation
* prev_permutation
