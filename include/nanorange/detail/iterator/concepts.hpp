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
        -> decltype(valid_expr(std::declval<value_type_t<In>>(),
                               std::declval<reference_t<In>>(),
                               std::declval<rvalue_reference_t<In>>()));
};

} // namespace detail

template <typename In>
NANO_CONCEPT Readable =
    detail::requires_<detail::Readable_req, In>&& CommonReference<
        detail::checked_reference_t<In>&&, detail::checked_value_type_t<In>&>&&
        CommonReference<detail::checked_reference_t<In>&&,
                        detail::checked_rvalue_ref_t<In>&>&&
            CommonReference<detail::checked_rvalue_ref_t<In>&&,
                            const detail::checked_value_type_t<In>&>;

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
        valid_expr(std::declval<difference_type_t<I>>(),
                   requires_expr<SignedIntegral<difference_type_t<I>>>{},
                   same_lv<I>(++i), i++));
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
    auto requires_(I i) -> decltype(valid_expr(not_void(*i)));
};

} // namespace detail

template <typename I>
NANO_CONCEPT Iterator =
    detail::requires_<detail::Iterator_req, I>&& WeaklyIncrementable<I>;

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

template <typename I>
NANO_CONCEPT InputIterator =
    Iterator<I>&& Readable<I>&& detail::exists_v<iterator_category_t, I>&&
        DerivedFrom<detail::checked_iterator_category_t<I>, input_iterator_tag>;

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

template <typename I>
NANO_CONCEPT ForwardIterator = InputIterator<I>&&
    DerivedFrom<detail::checked_iterator_category_t<I>, forward_iterator_tag>&&
        Incrementable<I>&& Sentinel<I, I>;

// [ranges.iterators.bidirectional]

namespace detail {

struct BidirectionalIterator_req {
    template <typename I>
    auto requires_(I i)
        -> decltype(valid_expr(same_lv<I>(--i), same_rv<I>(i--)));
};

} // namespace detail

template <typename I>
NANO_CONCEPT BidirectionalIterator =
    ForwardIterator<I>&& DerivedFrom<detail::checked_iterator_category_t<I>,
                                     bidirectional_iterator_tag>&&
        detail::requires_<detail::BidirectionalIterator_req, I>;

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

} // namespace detail

template <typename I>
NANO_CONCEPT RandomAccessIterator = BidirectionalIterator<I>&& DerivedFrom<
    detail::checked_iterator_category_t<I>, random_access_iterator_tag>&&
    StrictTotallyOrdered<I>&& SizedSentinel<I, I>&&
        detail::requires_<detail::RandomAccessIterator_req, I>;


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
NANO_CONCEPT Cpp98Iterator =
        Iterator<I> && Sentinel<I, I> && exists_v<legacy_iterator_traits_t, I>;

}

NANO_END_NAMESPACE

#endif
