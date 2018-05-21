
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
  * ostream_iterator
  * istreambuf_iterator
  * ostreambuf_iterator
* Finish subrange
* Re-implement the algorithms properly, including projections etc
* Implement the views from P0789
