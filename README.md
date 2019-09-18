
[![Standard](https://img.shields.io/badge/c%2B%2B-17/20-blue.svg)](https://en.wikipedia.org/wiki/C%2B%2B#Standardization)
[![License](https://img.shields.io/badge/license-BSL-blue.svg)](http://www.boost.org/LICENSE_1_0.txt)
[![Build Status](https://travis-ci.org/tcbrindle/NanoRange.svg?branch=master)](https://travis-ci.org/tcbrindle/NanoRange)
[![Build status](https://ci.appveyor.com/api/projects/status/6vciaaskslg34pux/branch/master?svg=true)](https://ci.appveyor.com/project/tcbrindle/nanorange/branch/master) [![download](https://img.shields.io/badge/latest-download-blue.svg)](https://github.com/tcbrindle/NanoRange/raw/master/single_include/nanorange.hpp) 
[![Try it on godbolt online](https://img.shields.io/badge/on-godbolt-blue.svg)](https://godbolt.org/g/Dbi77j) 


# NanoRange #

NanoRange is a C++17 implementation of the C++20 Ranges proposals (formerly the
Ranges TS). It provides SFINAE-based implementations of all the proposed Concepts,
and constrained and range-based versions the algorithms in the `<algorithm>`
standard library header.

It is intended for users who want range-based goodness in their C++, but don't
want to (or can't) use the full-blown
 [Range-V3][Range-V3]. It also aims to provide
 an easy upgrade path to standard ranges when they arrive.
 
NanoRange is compatible with all three major C++ compilers, including the
latest version of Microsoft Visual C++.

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

NanoRange requires a conforming C++17 compiler, and is
[tested](https://travis-ci.org/tcbrindle/nanorange) with GCC 7 and Clang 4.0
and newer. Older versions may work in some cases, but this is not guaranteed.

In addition, NanoRange works with MSVC 2017 version 15.9. Note that
the `/permissive-` switch is required for correct two-phase lookup.

## Documentation ##

There is currently no reference documentation for NanoRange itself, but
the Ranges TS on which it is based is partially documented
[on cppreference](http://en.cppreference.com/w/cpp/experimental/ranges).

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

The One Ranges proposal [P0896] adds two new iterator adaptors to the
standard library, namely `common_iterator` and `counted_iterator`,
which are both implemented in NanoRange.

Additionally, [P0896] modifies the existing iterator adaptors for
compatibility with the new concepts. NanoRange therefore provides
drop-in replacements for these, specifically:

 * `reverse_iterator`
 * `front_insert_iterator`
 * `back_insert_iterator`
 * `insert_iterator`
 * `istream_iterator`
 * `ostream_iterator`
 * `istreambuf_iterator`
 * `ostreambuf_iterator`

Lastly, NanoRange provides an implementation of `subrange` from
[P0896]. This can be used to turn an iterator/sentinel pair into in range,
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

In [P0896], almost all the algorithms are marked  as `constexpr` (the exceptions
being those which can potentially allocate temporary storage). NanoRange fully supports
this, meaning the vast majority of algorithms can be called at compile-time. For example

```cpp
auto sort_copy = [](auto rng) {
    nano::sort(rng);
    return rng;
};

int main()
{
    constexpr std::array a{5, 4, 3, 2, 1};
    constexpr auto b = sort_copy(a);

    static_assert(nano::is_sorted(b));
}
```

## Ranges papers ##

The Ranges proposals have been consolidated into two main papers:

 * [P0898R3][P0898] *Standard library concepts*
 * [P0896R2][P0896] *The One Ranges Proposal*
 
 NanoRange fully implements the first, and implements most of the second (except
 for Views).

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
[P0898]: http://www.open-std.org/jtc1/sc22/wg21/docs/papers/2018/p0898r3.pdf
[P0896]: http://www.open-std.org/jtc1/sc22/wg21/docs/papers/2018/p0896r2.pdf
[pdqsort]: https://github.com/orlp/pdqsort
[vcpkg]: https://github.com/Microsoft/vcpkg
