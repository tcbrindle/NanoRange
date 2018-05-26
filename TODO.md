
# Nanorange TODO list #

In rough order or priority:

* Add Travis/Appveyor automatic testing
* Automatically generate single include header when testing
* Move the rewrite branch to master
* Implement the remaining algorithms as constrained wrappers around the existing std:: versions
* Use the new names from P1037 (iter_value_t, iter_difference_t etc)
* Handle ContiguousIterators and ContiguousRanges
* Fix common_iterator (use variant-like data store)
* Implement the remaining iterator adaptors, namely:
  * reverse_iterator
  * move_iterator
  * istream_iterator
  * istreambuf_iterator
* Re-implement the remaining algorithms properly, including projections etc
* Implement the views from P0789
* Implement the tagged_tuple machinery

