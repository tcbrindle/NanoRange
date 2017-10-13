
# NanoRange #

NanoRange is a single-header C++14 implementation of (a subset of) the
[Ranges TS](http://open-std.org/JTC1/SC22/WG21/docs/papers/2017/n4684.pdf).
It provides SFINAE-based implementations of the Concepts from the TS, and
constrained and range-based wrappers for the algorithms in the `<algorithm>`
and `<numeric>` standard library headers.

It is intended for users who want range-based goodness in their C++, but don't
want to (or can't) add a dependency on the full-blown
 [Range-V3](https://github.com/ericniebler/range-v3/).

## Usage ##

No installation is required. Simply copy the header `nanorange.hpp` into your
own sources and use it as a replacement for your existing standard library
algorithm calls.

All functions and "concepts" are defined in namespace `nanorange`. You may find
it convenient to introduce a namespace alias such as

```cpp
namespace rng = nanorange;
```

to reduce typing and make it easier to upgrade to Range-V3 or STL2 in future.

## Examples ##

In the simplest case, NanoRange provides constrained wrappers for the existing
algorithms in the standard library. For example, the call

```cpp
std::list<int> list{2, 3, 1};
rng::sort(list.begin(), list.end());
```

will fail to compile with a (relatively) simple "no matching function call" error,
rather than pages of template backtraces as would be the case for an equivalent
call to `std::sort()`.

NanoRange also provides range-based overloads for all the existing algorithms, so
you can simply say

```cpp
std::vector<int> vec{2, 3, 1};
rng::sort(vec);
```

or

```cpp
std::vector<int> in1{1, 2, 3};
std::array<int, 3> in2{4, 5, 6};
std::vector<int> out;

rng::transform(in1, in2, rng::back_inserter(out), std::plus<>{});
```

for example, rather than having to write `begin()` and `end()` everywhere.

Lastly, NanoRange provides implementations of the Ranges TS concepts in the form
of `constexpr bool` (or `inline constexpr bool` in C++17) variable templates.
You could use these for `static_assert`s, or to constrain your own algorithm
implementations, for example by

```cpp
template <typename RandomIt, typename Comparator = std::less<>,
          std::enable_if_t<
              rng::RandomAccessIterator<RandomIt> &&
              rng::Sortable<RandomIt, Comparator>, int> = 0>
void my_sort(RandomIt first, RandomIt last, Comparator = {})
{
    // implementation
}
```

You could also use the concepts in C++17 `if constexpr` blocks, for example

```cpp
if constexpr (rng::RandomAccessIterator<I>) {
    // do something
} else if constexpr (rng::BidirectionalIterator<I>) {
    // do something else
} else if constexpr (rng::ForwardIterator<I>) {
    // do a third thing
}
```

## Differences from the Ranges TS ##

Because NanoRange wraps the existing algorithm implementations from your standard
library headers rather than providing its own, it deviates from the Ranges TS in
a number of ways.

 * Most importantly, NanoRange requires that objects of the same type are passed
   as the `first` and `last` iterator arguments to all calls. That is, NanoRange's
   `Sentinel<S, I>` concept requires `Same<S, I>`.

 * This means that NanoRange requires that a valid `Range` returns the same
   type from calls to `begin()` and `end()`. Put another way, NanoRange's
   `Range` concept is the same as the TS's `BoundedRange`.

 * NanoRange does not support projections.

 * For compatibility with the existing standard library `Iterator` concept,
   NanoRange's `Iterator` trait requires that a specialisation of `std::iterator_traits<I>` exists
   and contains all 5 required typedefs.

 * In the Ranges TS, the return types of various algorithms are changed to
   return more information in the form of a pair or tuple of iterators.
   NanoRange's return types instead match those of the existing standard library
   functions.

 * Because of the above, NanoRange does not provide implementations of the
   `tagged_pair` and `tagged_tuple` types from the TS.

 * In certain cases the Ranges TS allows weaker constraints than are currently mandated by
   the standard library. For example, `fill()` only requires `OutputIterator`
   in the TS, whereas the existing version requires a (mutable) `ForwardIterator`.
   In such cases, NanoRange uses the stronger contstraints.

 * Not all of the Ranges TS's "customisation point objects" have yet been implemented in
   NanoRange.

 * NanoRange's implementation of `common_type` and `common_reference` do not meet
   the specification in the Ranges TS. This is largely because the specification of 
   `common_reference` is so complicated that attempting to implement it made me 
   want to cry actual tears.

 * NanoRange does not currently implement the adaptor classes `reverse_iterator`,
   `move_iterator`, `common_iterator` or `counted_iterator`.

 * As an extension, NanoRange provides constrained and range-based overloads
   for algorithms from `<numeric>`, such as `accumulate()` and `iota()`. These are
   not part of the TS, and the constraints that NanoRange uses
   are very likely incorrect.

## Differences from the existing standard library ##

In many cases, the Range TS requirements on types are more stringent than those
currently (implicitly) used by the standard library.

In particular, the TS requires that all `Iterator`s are `DefaultConstructible`.
This means that certain commonly-used standard library types such as
`std::back_insert_iterator` and `std::ostream_iterator` no longer qualify as valid
`Iterator`s, and cannot be used with constrained calls. NanoRange provides
replacements for

   * `ostream_iterator`
   * `ostreambuf_iterator`
   * `back_insert_iterator` and `back_inserter()`
   * `front_insert_iterator` and `front_inserter()`
   * `insert_iterator` and `inserter()`

which meet both the old and new iterator requirements.

Secondly, the Ranges TS deprecates the "three-legged" forms of algorithms
such as `equal()` and `mismatch()`. These are at best potentially incorrect, and
at worse potentially dangerous. For example:

```cpp
std::vector<int> vec1{1, 2, 3}
std::vector<int> vec2{1, 2, 3, 4, 5};

assert(vec1 != vec2);
assert(std::equal(vec1.begin(), vec1.end(), vec2.begin()); // passes!?
assert(std::equal(vec2.begin(), vec2.end(), vec1.begin()); // Crash!
```

These functions are annotated with the `[[deprecated]]` attribute in Nanorange.
If you find these deprecation warnings annoying, you
can disable them by defining the preprocessor symbol  `NANORANGE_NO_DEPRECATION_WARNINGS`
before `#include`-ing `nanorange.hpp`.


## Licence ##

NanoRange is provided under the Boost licence. See LICENSE_1_0.txt for details.

## Acknowledgements ##

Many thanks to the following:

 * Eric Niebler and Casey Carter for the Ranges TS, Range-V3
   and CMCSTL2. You guys are awesome.

 * All the contributors to the Palo Alto TR, for laying the foundation upon which the TS is built

 * Phil Nash for the fantastic Catch testing framework

 * The editors of [cppreference.com](https://cppreference.com) for painstakingly
   detailing the existing requirements of standard library algorithms, and
   more generally for maintaining the C++ programmer's bible.

