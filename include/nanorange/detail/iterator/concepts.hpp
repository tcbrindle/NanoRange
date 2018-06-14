// nanorange/detail/iterator/concepts.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_DETAIL_ITERATOR_CONCEPTS_HPP_INCLUDED
#define NANORANGE_DETAIL_ITERATOR_CONCEPTS_HPP_INCLUDED

#include <nanorange/detail/concepts/object.hpp>
#include <nanorange/detail/iterator/associated_types.hpp>
#include <nanorange/detail/iterator/traits.hpp>

NANO_BEGIN_NAMESPACE

// [range.iterators.readable]
namespace detail {

struct Readable_req {
    template <typename In>
    auto requires_()
        -> decltype(std::declval<value_type_t<In>>(),
                               std::declval<reference_t<In>>(),
                               std::declval<rvalue_reference_t<In>>());
};

template <typename>
auto Readable_fn(long) -> std::false_type;

template <typename In>
auto Readable_fn(int) -> std::enable_if_t<
     requires_<Readable_req, In> &&
     CommonReference<reference_t<In>&&, value_type_t<In>&> &&
     CommonReference<reference_t<In>&&, rvalue_reference_t<In>&&> &&
     CommonReference<rvalue_reference_t<In>&&, const value_type_t<In>&>,
             std::true_type>;

} // namespace detail

template <typename In>
NANO_CONCEPT Readable = decltype(detail::Readable_fn<In>(0))::value;

// [range.iterators.writable]
namespace detail {

struct Writable_req {
    template <typename Out, typename T>
    auto requires_(Out&& o, T&& t) -> decltype(valid_expr(
        *o = std::forward<T>(t), *std::forward<Out>(o) = std::forward<T>(t),
        const_cast<const reference_t<Out>&&>(*o) = std::forward<T>(t),
        const_cast<const reference_t<Out>&&>(*std::forward<Out>(o)) =
            std::forward<T>(t)));
};

} // namespace detail

template <typename Out, typename T>
NANO_CONCEPT Writable = detail::requires_<detail::Writable_req, Out, T>;

// [range.iterators.weaklyincrementable]

namespace detail {

template <typename T, typename Deduced>
auto same_lv(Deduced&) -> std::enable_if_t<Same<T, Deduced>, int>;

template <typename T, typename Deduced>
auto same_rv(Deduced &&) -> std::enable_if_t<Same<T, Deduced>, int>;

struct WeaklyIncrementable_req {
    template <typename I>
    auto requires_(I i) -> decltype(
        std::declval<difference_type_t<I>>(),
        requires_expr<SignedIntegral<difference_type_t<I>>>{},
        same_lv<I>(++i), i++);
};

} // namespace detail

template <typename I>
NANO_CONCEPT WeaklyIncrementable =
    Semiregular<I>&& detail::requires_<detail::WeaklyIncrementable_req, I>;

// [range.iterators.incrementable]

namespace detail {

struct Incrementable_req {
    template <typename I>
    auto requires_(I i) -> decltype(valid_expr(same_rv<I>(i++)));
};

} // namespace detail

template <typename I>
NANO_CONCEPT Incrementable = Regular<I>&& WeaklyIncrementable<I>&&
    detail::requires_<detail::Incrementable_req, I>;

// [range.iterators.iterator]

namespace detail {

struct Iterator_req {
    template <typename I>
    auto requires_(I i) -> decltype(not_void(*i));
};

} // namespace detail

template <typename I>
NANO_CONCEPT Iterator =
    detail::requires_<detail::Iterator_req, I> && WeaklyIncrementable<I>;

// [range.iterators.sentinel]

template <typename S, typename I>
NANO_CONCEPT Sentinel =
    Semiregular<S>&& Iterator<I>&& detail::WeaklyEqualityComparableWith<S, I>;

// [range.iterators.sizedsentinel]

template <typename S, typename I>
constexpr bool disable_sized_sentinel = false;

namespace detail {

struct SizedSentinel_req {
    template <typename S, typename I>
    auto requires_(const S& s, const I& i)
        -> decltype(valid_expr(same_rv<difference_type_t<I>>(s - i),
                               same_rv<difference_type_t<I>>(i - s)));
};

} // namespace detail

template <typename S, typename I>
NANO_CONCEPT SizedSentinel =
    Sentinel<S, I> &&
    !disable_sized_sentinel<std::remove_cv_t<S>, std::remove_cv_t<I>> &&
    detail::requires_<detail::SizedSentinel_req, S, I>;

// [range.iterators.input]

namespace detail {

template <typename>
auto InputIterator_fn(long) -> std::false_type;

template <typename I>
auto InputIterator_fn(int) -> std::enable_if_t<
    Iterator<I> && Readable<I> &&
    exists_v<iterator_category_t, I> &&
    DerivedFrom<iterator_category_t<I>, input_iterator_tag>,
            std::true_type>;


}

template <typename I>
NANO_CONCEPT InputIterator = decltype(detail::InputIterator_fn<I>(0))::value;

// [ranges.iterator.output]

namespace detail {

struct OutputIterator_req {
    template <typename I, typename T>
    auto requires_(I i, T&& t)
        -> decltype(valid_expr(*i++ = std::forward<T>(t)));
};

} // namespace detail

template <typename I, typename T>
NANO_CONCEPT OutputIterator = Iterator<I>&& Writable<I, T>&&
    detail::requires_<detail::OutputIterator_req, I, T>;

// [ranges.iterators.forward]

namespace detail {

template <typename>
auto ForwardIterator_fn(long) -> std::false_type;

template <typename I>
auto ForwardIterator_fn(int) -> std::enable_if_t<
        InputIterator<I> &&
        DerivedFrom<iterator_category_t<I>, forward_iterator_tag> &&
        Incrementable<I> &&
        Sentinel<I, I>,
                std::true_type>;

}

template <typename I>
NANO_CONCEPT ForwardIterator = decltype(detail::ForwardIterator_fn<I>(0))::value;

// [ranges.iterators.bidirectional]

namespace detail {

struct BidirectionalIterator_req {
    template <typename I>
    auto requires_(I i)
        -> decltype(valid_expr(same_lv<I>(--i), same_rv<I>(i--)));
};

template <typename>
auto BidirectionalIterator_fn(long) -> std::false_type;

template <typename I>
auto BidirectionalIterator_fn(int) -> std::enable_if_t<
        ForwardIterator<I> &&
        DerivedFrom<iterator_category_t<I>, bidirectional_iterator_tag> &&
        requires_<BidirectionalIterator_req, I>,
                std::true_type>;

} // namespace detail

template <typename I>
NANO_CONCEPT BidirectionalIterator =
    decltype(detail::BidirectionalIterator_fn<I>(0))::value;

// [ranges.iterators.random.access]

namespace detail {

struct RandomAccessIterator_req {
    template <typename I>
    auto requires_(I i, const I j, const difference_type_t<I> n) -> decltype(
        valid_expr(same_lv<I>(i += n), same_rv<I>(j + n),
                   n + j, // same_rv<I>(n + j) -- FIXME: MSVC doesn't like this
                          // with I = int*, find out why
                   same_lv<I>(i -= n), same_rv<I>(j - n), j[n],
                   requires_expr<Same<decltype(j[n]), reference_t<I>>>{}));
};

template <typename>
auto RandomAccessIterator_fn(long) -> std::false_type;

template <typename I>
auto RandomAccessIterator_fn(int) -> std::enable_if_t<
     BidirectionalIterator<I> &&
     DerivedFrom<iterator_category_t<I>, random_access_iterator_tag> &&
     StrictTotallyOrdered<I> &&
     SizedSentinel<I, I> &&
     requires_<RandomAccessIterator_req, I>,
             std::true_type>;

} // namespace detail

template <typename I>
NANO_CONCEPT RandomAccessIterator = 
        decltype(detail::RandomAccessIterator_fn<I>(0))::value;


// Extension: used for constraining iterators for existing STL algos

namespace detail {

template <typename I,
          typename T = std::iterator_traits<I>,
          typename = typename T::value_type,
          typename = typename T::difference_type,
          typename = typename T::reference,
          typename = typename T::pointer,
          typename = typename T::iterator_category>
using legacy_iterator_traits_t = void;

template <typename I>
NANO_CONCEPT Cpp98Iterator = exists_v<legacy_iterator_traits_t, I>;

}

NANO_END_NAMESPACE

#endif
