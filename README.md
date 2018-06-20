
[![Standard](https://img.shields.io/badge/c%2B%2B-14/17/20-blue.svg)](https://en.wikipedia.org/wiki/C%2B%2B#Standardization)
[![License](https://img.shields.io/badge/license-BSL-blue.svg)](http://www.boost.org/LICENSE_1_0.txt)
[![Build Status](https://travis-ci.org/tcbrindle/NanoRange.svg?branch=master)](https://travis-ci.org/tcbrindle/NanoRange)
[![Build status](https://ci.appveyor.com/api/projects/status/6vciaaskslg34pux/branch/master?svg=true)](https://ci.appveyor.com/project/tcbrindle/nanorange/branch/master) [![download](https://img.shields.io/badge/latest-download-blue.svg)](https://github.com/tcbrindle/NanoRange/raw/master/single_include/nanorange.hpp) 
[![Try it on godbolt online](https://img.shields.io/badge/on-godbolt-blue.svg)](https://godbolt.org/g/Dbi77j) 


# NanoRange #

NanoRange is a C++14 implementation of the C++20 Ranges proposals (formerly the
Ranges TS). It provides SFINAE-based implementations of all the proposed Concepts,
and constrained and range-based versions the algorithms in the `<algorithm>`
standard library header.

It is intended for users who want range-based goodness in their C++, but don't
want to (or can't) use the full-blown
 [Range-V3][Range-V3]. It also aims to provide
 an easy upgrade path to standard ranges when they arrive.
 
NanoRange is compatible with all three major C++ compilers, including the
latest version of Microsoft Visual C++.

There is currently no reference documentation for NanoRange itself, but
the Ranges TS on which it is based is partially documented
[on cppreference](http://en.cppreference.com/w/cpp/experimental/ranges).

## Usage ##

The easiest way to use NanoRange is to simply download the [latest, automatically-generated 
single-header version](https://github.com/tcbrindle/NanoRange/raw/master/single_include/nanorange.hpp)
and include it in your own sources like any other header. This is currently
the recommended way to use the library.

Alternatively, you can clone this repository and use the individual headers in
the `include/nanorange` directory. This may give a slight boost to compile times,
although there doesn't seem to be [too much difference](https://github.com/tcbrindle/NanoRange/wiki/Compile-times)
at present. (In any case, the single-header version is similar to what you'll
get when you `#include <algorithm>` in C++20).

Finally, if you use [vcpkg], you can install the latest version of NanoRange from
master using

```
vcpkg install nanorange --head
```

## Compatibility ##

NanoRange requires a conforming C++14 compiler, and is
[tested](https://travis-ci.org/tcbrindle/nanorange) with GCC 5.4 and Clang 3.8
and newer. Older versions may work in some cases, but this is not guaranteed.

In addition, NanoRange works with MSVC 2017 version 15.7. Note that
the `/permissive-` switch is required for correct two-phase lookup.

## What it provides ##

### Concepts ###

NanoRange provides all of the concepts from the ranges papers
in the form of `constexpr bool` variable templates. You can use these to
constrain your own templates via `std::enable_if`, or in `if constexpr` 
statements in C++17. For example

```cpp
template <typename Rng>
void foo(Rng&& rng) {
    if constexpr (nano::RandomAccessRange<Rng>) {
         // do something
    } else if constexpr (nano::BidirectionalRange<Rng>>) {
        // do something else
    } else if constexpr (nano::ForwardRange<Rng>) {
        // do a third thing
    }
}
```

### Iterator adaptors ###

NanoRange provides implementations of most of the iterator adaptors from
[P0896], specifically:

 * `common_iterator`
 * `counted_iterator`
 * `reverse_iterator`
 * `front_insert_iterator`
 * `back_insert_iterator`
 * `insert_iterator`
 * `istream_iterator`
 * `ostream_iterator`
 * `istreambuf_iterator`
 * `ostreambuf_iterator`

In addition, NanoRange provides an implementation of `subrange` from
[P0789]. This can be used to turn an iterator/sentinel pair into in range,
or as a `span`-like view of a subset of another range.

### Algorithms ###

#### Range-based overloads ####

NanoRange provides range-based overloads of all the algorithms in `<algorithm>`.
This means that you can finally say

```cpp
std::vector<int> vec{5, 4, 3, 2, 1};
nano::sort(vec);
```

and it will Just Work.

#### Constraint checking ####

In the existing STL, the algorithm calls are *unconstrained*. This means that
if you pass an argument which doesn't meet the requirements of the function,
the compiler will go ahead and try to instantiate the template anyway,
usually resulting in pages of error messages with template backtraces for
which C++ is infamous.

For example, the following program has an error: a `std::list` iterator
is not random-access, and so doesn't meet the requirements of `std::sort()`:

```cpp
int main()
{
    std::list<int> list{5, 4, 3, 2, 1};
    std::sort(list.begin(), list.end());
}
```

Compiling this two-line example with Clang 6.0 results in over [350 lines of error messages](https://gist.github.com/tcbrindle/13c23fc5c1a46db12665ee509bf8265f)!

Calling `nano::sort()` instead of `std::sort()` on the other hand means that
constraints are checked before the template is instantated. This time, the
entire error output is:

```
example.cpp:9:5: error: no matching function for call to object of type 'const nano::ranges::detail::sort_fn'
    nano::sort(list.begin(), list.end());
    ^~~~~~~~~~
include/nanorange/algorithm/stl/sort.hpp:26:5: note: candidate template ignored: requirement 'RandomAccessIterator<std::__1::__list_iterator<int, void *> >' was not satisfied [with I = std::__1::__list_iterator<int, void *>, Comp = nano::ranges::less<void>]
    operator()(I first, I last, Comp comp = Comp{}) const
    ^
include/nanorange/algorithm/stl/sort.hpp:37:5: note: candidate template ignored: substitution failure [with Rng = std::__1::__list_iterator<int, void *>, Comp = std::__1::__list_iterator<int, void *>]: no matching function for call to object of type 'const nano::ranges::detail::begin_::fn'
    operator()(Rng&& rng, Comp comp = Comp{}) const
    ^
1 error generated.
```

which makes it clear that the first overload was rejected because `list::iterator`
doesn't meet the requirements of `RandomAccessIterator`, and the second overload
was rejected because `list::iterator` is not a range (you can't call `nano::begin()`
on it). Much better.

#### Function objects ####

The algorithms in NanoRange are implemented as non-template function objects with
templated function call operators. This means that you can pass them around
without needing to specify template arguments or wrap them in lambdas.
For example:

```cpp
std::vector<std::vector<int>> vec_of_vecs = ...

nano::for_each(vec_of_vecs, nano::sort); // sorts each inner vector
```

#### Projections ####

The fully reimplemented algorithms (see below) offer support for *projections*.
A projection is a unary callable which modifies (*"projects"*) the view of the data
that the algorithm sees. Because projections are routed through (an implementation
of) `std::invoke()`, it's possible to use pointers to member functions and pointers
to member data as projections. For example:

```cpp
struct S {
    int i;
    float f;
};

std::vector<S> vec = ...

auto iter = nano::find(vec, 10, &S::i);
// iter points to the first member of vec for which i == 10
```

#### `constexpr` support ####

In C++20, most of the existing algorithms in `<algorithm>` will be marked
`constexpr`, and so will be usable at compile time. While the ranges
proposals do not currently feature `constexpr` support, it seems inevitable
that this will be added at some point. As an extension therefore, all of the
fully reimplemented algorithms in NanoRange (with the exception of those
which can potentially allocate) are `constexpr`.

(Note that `constexpr` support has not been well tested for all algorithms.
You may run into bugs, particularly on MSVC, which tends to bail out if
a `constexpr` function gets too complicated. More extensive testing is 
planned for a future version.)

#### Algorithms status ####

Most of the STL algorithms have been fully reimplemented in NanoRange and
provide all of the improvements from the ranges papers, including differing
iterator/sentinel types and support for projections.

A few of the more complex algorithms have not yet been fully reimplemented, 
and instead function as constrained wrappers around a call to the existing
STL implementation from your `<algorithm>` header.

Because they call into the existing STL, these
versions have additional constraints above what the Ranges papers require:
in particular, the iterator and sentinel types must be the same (or for the
range-based overloads, the range must model `CommonRange`). Iterators must also
be STL-compatible, that is, a specialisation of `std::iterator_traits` must exist
which provides all five required typedefs. In addition, projections are not
supported, and the return types match those of the original STL versions.

The file [algorithms.md](./algorithms.md) lists which algorithms have been fully reimplemented,
and which are STL wrappers. The long-term goal is to move all the algorithms
into the former category.

## What's missing ##

NanoRange is a new library, and certain features haven't been implemented yet.

In particular, NanoRange doesn't yet provide any of the Views from [P0789][P0789].
These will be added as the library evolves.

There is a [TODO list](TODO.md) which is gradually being migrated
to Github issues.

## Differences from Range-V3 ##

One inevitable question is *"why should I use this, when Range-V3 exists?"*.
The simple answer is that if you *can* use Range-V3, then go ahead and do so.
You won't regret it.

Having said that, a couple of NanoRange selling points might be:

 * It's very much smaller. Range-V3 provides a huge array of views and actions,
   not all of which are (or will be) proposed for standardisation. By contrast,
   while it's nowhere near as "nano" as it was when it started out, this library
   aims to provide only those features that will (or are likely to) end up in
   C++20.
   
 * NanoRange works with the latest MSVC, while Range-V3 currently doesn't. (There
   is an old fork of Range-V3 that is MSVC compatible, but it's buggy and
   mostly unmaintained.)

 * NanoRange currently tracks the C++20 Ranges proposals more closely than
   Range-V3. This means that if you use NanoRange today, there should be fewer
   changes you'll need to make to your code to upgrade to standard ranges when
   they arrive.
 
 * As an extension, NanoRange's reimplemented algorithms (see above) are all
   `constexpr`. This is not currently the case for Range-V3.

 * There is still a lot to do and probably a lot of bugs to fix, so you might have
   more fun hacking on NanoRange ;-)

## Ranges papers ##

NanoRange wholly or partially implements the following C++20 proposal papers:

 * [P0898R2][P0898] *Standard library concepts*
 * [P0896R1][P0896] *Merging the Ranges TS*
 * [P1037R0][P1037] *Deep Integration of the Ranges TS*
 * [P0970R1][P0970] *Better, safer range access customization points*
 * [P0789R3][P0789] *Range adaptors and utilities* (only `subrange` so far)

## Stability ##

NanoRange aims to track the various C++20 ranges proposals, and will be updated
as new papers are published. As such, there are no API stability guarantees
at this time.

## Licence ##

NanoRange is provided under the Boost licence. See LICENSE_1_0.txt for details.

## Acknowledgements ##

Many thanks to the following:

 * Eric Niebler and Casey Carter for the Ranges TS, Range-V3
   and CMCSTL2. You guys are awesome.
   
 * Orson Peters for [pdqsort]

 * Phil Nash for the fantastic Catch testing framework

 * The editors of [cppreference.com](https://cppreference.com) for painstakingly
   detailing the existing requirements of standard library algorithms, and
   more generally for maintaining the C++ programmer's bible.

<!-- References -->

[Range-V3]: https://github.com/ericniebler/range-v3/
[P0898]: http://www.open-std.org/jtc1/sc22/wg21/docs/papers/2018/p0898r2.pdf
[P0896]: http://www.open-std.org/jtc1/sc22/wg21/docs/papers/2018/p0896r1.pdf
[P0970]: http://www.open-std.org/jtc1/sc22/wg21/docs/papers/2018/p0970r1.pdf
[P0789]: http://www.open-std.org/jtc1/sc22/wg21/docs/papers/2018/p0789r3.pdf
[P1037]: http://www.open-std.org/jtc1/sc22/wg21/docs/papers/2018/p1037r0.pdf
[pdqsort]: https://github.com/orlp/pdqsort
[vcpkg]: https://github.com/Microsoft/vcpkg