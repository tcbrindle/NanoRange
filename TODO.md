
# Nanorange TODO list #

In rough order or priority:

* Reimplement common_type for compatibility with older libc++ versions
* Use the new names from P1037 (iter_value_t, iter_difference_t etc)
* Handle ContiguousIterators and ContiguousRanges
* Fix common_iterator (use variant-like data store)
* Implement the remaining iterator adaptors, namely:
  * reverse_iterator
  * move_iterator
  * istream_iterator
  * istreambuf_iterator
* Implement istream_range from P1035
* Re-implement the remaining algorithms properly, including projections etc
* Implement the views from P0789
* Implement the tagged_tuple machinery
* Implement the uninitialized_foo algorithms from P1033
