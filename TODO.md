
# Nanorange TODO list #

In rough order or priority:

* Add Travis/Appveyor automatic testing
* Automatically generate single include header when testing
* Move the rewrite branch to master
* Add Cpp98Iterator concept, so we can call std:: algorithms safely
* Implement the remaining algorithms as constrained wrappers around the existing std:: versions
* Use the new names from P1037 (iter_value_t, iter_difference_t etc)
* Implement the remaining range access and primitive CPOs, namely:
  * [c]rbegin
  * [c]rend
  * data
  * cdata
* Implement the remaining comparison classes using constraints, namely
  * not_equal_to
  * less
  * greater
  * less_equal
  * greater_equal
* Fix common_iterator (use variant-like data store)
* Work out which of the iterator adaptors we need to implement:
  * reverse_iterator
  * back_insert_iterator
  * front_insert_iterator
  * insert_iterator
  * move_iterator
  * counted_iterator
  * istream_iterator
  * istreambuf_iterator
  * ostreambuf_iterator
* Re-implement the algorithms properly, including projections etc
* Implement the views from P0789

# Algorithms List #

## Fully implemented ##

The following algorithms are fully reimplemented in Nanorange,
meeting the requirements of the Ranges papers. In particular, they accept
iterators and sentinels of different types, and allow the use of projections.

#### Non-modifying sequence ops ####

* all_of
* any_of
* none_of
* for_each
* find
* find_if
* find_if_not
* find_first_of
* adjacent_find
* count
* count_if
* mismatch
* equal
* search

#### Modifying sequence ops ####

* copy
* copy_n
* copy_if
* copy_backward
* move
* move_backward
* swap_ranges

## Partially implemented (using STL) ##

The following algorithms are implemented in Nanorange as wrappers around the
existing standard library implementation. Constraints and return types are based
on the C++98 specification. This means that the iterator and sentinel types
must be the same, or for the range-based overloads, the range must model
CommonRange. Projections cannot be used.

### Non-modifying sequence ops ##

* find_end

## Unimplemented ##

The following algorithms have not yet been implemented in Nanorange:

#### Non-modifying sequence ops ####

* is_permutation
* search_n

#### Modifying sequence ops ####

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
* unique_copy
* reverse
* reverse_copy
* rotate
* rotate_copy
* shuffle

#### partitions ####

* is_partitioned
* partition
* stable_partition
* partition_copy
* partition_point

#### sorting ####
* sort
* stable_sort
* partial_sort
* partial_sort_copy
* is_sorted
* is_sorted_until
* nth_element

#### binary search ####

* lower_bound
* upper_bound
* equal_range
* binary_search

#### merge ####

* merge
* inplace_mergs

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

#### minimum and maximum ####

* min
* max
* minmax
* min_element
* max_element
* minmax_element
* lexicographical_compare

#### permutations ####

* next_permutation
* prev_permutation
