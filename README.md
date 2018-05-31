
[![Standard](https://img.shields.io/badge/c%2B%2B-14/17/20-blue.svg)](https://en.wikipedia.org/wiki/C%2B%2B#Standardization)
[![License](https://img.shields.io/badge/license-BSL-blue.svg)](http://www.boost.org/LICENSE_1_0.txt)
[![Build Status](https://travis-ci.org/tcbrindle/NanoRange.svg?branch=master)](https://travis-ci.org/tcbrindle/nanorange)
[![Build status](https://ci.appveyor.com/api/projects/status/6vciaaskslg34pux/branch/master?svg=true)](https://ci.appveyor.com/project/tcbrindle/nanorange/branch/master) [![download](https://img.shields.io/badge/latest-download-blue.svg)](https://github.com/tcbrindle/NanoRange/raw/master/single_include/nanorange.hpp) 
[![Try it on godbolt online](https://img.shields.io/badge/on-godbolt-blue.svg)](https://godbolt.org/g/Dbi77j) 


# NanoRange #

NanoRange is a new C++14 implementation of the C++20 Ranges proposals (formerly the
Ranges TS). It provides SFINAE-based implementations of the Concepts from the TS,
and constrained and range-based wrappers for the algorithms in the `<algorithm>`
standard library header.

It is intended for users who want range-based goodness in their C++, but don't
want to (or can't) use the full-blown
 [Range-V3][Range-V3]. It also aims to provide
 an easy upgrade path to standard ranges when they arrive.
 
NanoRange is compatible with all three major C++ compilers, including the
latest version of Microsoft Visual C++.

## Usage ##

The easiest way to use NanoRange is to simply download the [latest, automatically-
generated single-header version](https://github.com/tcbrindle/NanoRange/raw/master/single_include/nanorange.hpp)
and include it in your own sources like any other header. This is currently
the recommended way to use the library.

Alternatively, you can clone this repository and use the individual headers in
the `include/nanorange` directory. This may give a slight boost to compile times,
although there doesn't seem to be [too much difference](https://github.com/tcbrindle/NanoRange/wiki/Compile-times)
at present. (In any case, the single-header version is similar to what you'll
get when you `#include <algorithm>` in C++20).

## Compatibility ##

NanoRange requires a conforming C++14 compiler. It has been
[tested](https://travis-ci.org/tcbrindle/nanorange) with GCC 5.4 and Clang 3.8.
Older versions may work in some cases, but this is not guaranteed.

In addition, thanks to the excellent work Microsoft have done in improving their
standards compliance, NanoRange works with MSVC 2017 version 15.7. Note that
the `/permissive-` switch is required for correct two-phase lookup.

## What it provides ##

### Concepts ###

NanoRange provides all of the concepts from [P0898][P0898]
in the form of `constexpr bool` variable templates. You can use these to
constrain your own templates via `std::enable_if`, or in `constexpr if` 
statements in C++17. For example

```cpp
template <typename Range>
void foo(Range&& rng) {
    if constexpr (nano::RandomAccessRange<Rng>) {
         // do something
    } else if constexpr (nano::BidirectionalIterator<Rng>>) {
        // do something else
    } else if constexpr (nano::ForwardRange<Rng>) {
        // do a third thing
    }
}
```

### Iterator adaptors ###

NanoRange provides implementations of some of the iterator adaptors from
[P0896][P0896],
specifically:

 * `common_iterator`
 * `counted_iterator`
 * `front_insert_iterator`
 * `back_insert_iterator`
 * `insert_iterator`
 * `ostream_iterator`
 * `ostreambuf_iterator`
 
The remaining adaptors (namely `reverse_iterator`, `move_iterator`,
`istream_iterator` and `istreambuf_iterator`) have not yet been implemented,
but the existing STL versions can be used with NanoRange algorithms.

In addition, NanoRange provides an implementation of `subrange` from
[P0789][P0789].
This can be used to turn an iterator/sentinel pair into in range, or as a
`span`-like view of a subset of another range.

### Algorithms ###

NanoRange implements constrained version of all the algorithms from [P0896][P0896],
including range-based overloads. This means that you can finally say

```cpp
std::vector<int> vec{5, 4, 3, 2, 1};
nano::sort(vec);
```

and it will Just Work.

#### Algorithms status ####

Around half of the algorithms have been fully reimplemented in NanoRange and
provide all of the improvements from the ranges paper, including differing
iterator/sentinel types and support for projections. As an extension, all of
the reimplemented algorithms are marked `constexpr` for use at compile time (
but note that `constexpr` support has not been well tested: if you run into
problems please file a bug).

Most of the more complex algorithms have not yet been reimplemented, 
and instead are provided as constrained wrappers around a call to the existing
STL implementation from your `<algorithm>` header. This is the case for
`nano::sort()`, for example. Because they call into the existing STL, these
versions have additional constraints above what the Ranges papers require:
in particular, the iterator and sentinel types must be the same (or for the
range-based overloads, the range must model `CommonRange`). Iterators must also
be STL-compatible, that is, a specialisation of `std::iterator_traits` must exist
which provides all five required typedefs. In addition, projections are not
supported, and the return types match those of the original STL versions.

The file [algorithms.md](../blob/master/algorithms.md) lists which algorithms have been fully reimplemented,
and which are STL wrappers. The long-term goal is to move all the algorithms
into the former category.

## What's missing ##

NanoRange is a new library, and certain features haven't been implemented yet.

In particular, NanoRange doesn't yet provide any of the Views from [P0789][P0789].
These will be added as the library evolves.

There is a [TODO list](../blob/master/TODO.md) which is gradually being migrated
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
 
 * As an extension, NanoRange's reimplemented algorithms (see above) are all
   `constexpr`. This is not currently the case for Range-V3.

 * There is still a lot to do and probably a lot of bugs to fix, so you might have
   more fun hacking on NanoRange ;-)

## Ranges papers ##

NanoRange wholly or partially implements the following C++20 proposal papers:

 * [P0898R2][P0898] *Standard library concepts*
 * [P0896R1][P0896] *Merging the Ranges TS*
 * [P0970R1][P0970] *Better, safer range access customization points*
 * [P0789R3][P0789] *Range adaptors and utilities* (only `subrange` so far)

[Range-V3]: https://github.com/ericniebler/range-v3/
[P0898]: http://www.open-std.org/jtc1/sc22/wg21/docs/papers/2018/p0898r2.pdf
[P0896]: http://www.open-std.org/jtc1/sc22/wg21/docs/papers/2018/p0896r1.pdf
[P0970]: http://www.open-std.org/jtc1/sc22/wg21/docs/papers/2018/p0970r1.pdf
[P0789]: http://www.open-std.org/jtc1/sc22/wg21/docs/papers/2018/p0789r3.pdf

## Stability ##

NanoRange aims to track the various C++20 ranges proposals, and will be updated
as new revisions are published. As such, there are no API stability guarantees
at this time.

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
