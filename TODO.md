
# Nanorange TODO list #

In rough order or priority:

* Add Cpp98Iterator concept, so we can call std:: algorithms safely
* Implement the remaining algorithms as constrained wrappers around
  the existing std:: versions
* Fix common_reference (again)
* Use the new names from P1037
* Implement the remaining CPOs according to P0970 (i.e. no rvalues)
* Implement the remaining utility classes, like reverse and counted iterators
* Implement (some of) the Views from P0789
* Move the rewrite branch to master
* Add Travis/Appveyor automatic testing
* Re-implement the algorithms properly, including projections etc
