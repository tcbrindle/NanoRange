// nanorange/memory/uninitialized_copy.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_MEMORY_UNINITIALIZED_COPY_HPP_INCLUDED
#define NANORANGE_MEMORY_UNINITIALIZED_COPY_HPP_INCLUDED

#include <nanorange/algorithm/copy.hpp>
#include <nanorange/memory/destroy.hpp>

NANO_BEGIN_NAMESPACE

template <typename I, typename O>
using uninitialized_copy_result = copy_result<I, O>;

namespace detail {

struct uninitialized_copy_fn {
private:
    friend struct uninitialized_copy_n_fn;

    template <typename I, typename S, typename O, typename S2>
    static uninitialized_copy_result<I, O>
    impl4(I ifirst, S ilast, O ofirst, S2 olast)
    {
        O oit = ofirst;
        try {
            for (; ifirst != ilast && oit != olast; ++ifirst, (void) ++oit) {
                ::new(detail::voidify(*oit))
                        std::remove_reference_t<iter_reference_t<O>>(*ifirst);
            }
            return {std::move(ifirst), std::move(oit)};
        } catch (...) {
            nano::destroy(ofirst, ++oit);
            throw;
        }
    }

    template <typename I, typename S, typename O>
    static uninitialized_copy_result<I, O>
    impl3(I ifirst, S ilast, O ofirst)
    {
        O oit = ofirst;
        try {
            for (; ifirst != ilast; ++ifirst, (void) ++oit) {
                ::new(const_cast<void*>(static_cast<const volatile void*>(std::addressof(
                        *oit))))
                        std::remove_reference_t<iter_reference_t<O>>(*ifirst);
            }
            return {std::move(ifirst), std::move(oit)};
        } catch (...) {
            nano::destroy(ofirst, ++oit);
            throw;
        }
    }

public:
    // Four-legged
    template <typename I, typename S, typename O, typename S2>
    std::enable_if_t<
        input_iterator<I> && sentinel_for<S, I> &&
            no_throw_forward_iterator<O> &&
            no_throw_sentinel<S2, O> &&
        constructible_from<iter_value_t<O>, iter_reference_t<I>>,
        uninitialized_copy_result<I, O>>
    operator()(I ifirst, S ilast, O ofirst, S2 olast) const
    {
        return uninitialized_copy_fn::impl4(
                std::move(ifirst), std::move(ilast),
                std::move(ofirst), std::move(olast));
    }

    // Two ranges
    template <typename IRng, typename ORng>
    std::enable_if_t<
        input_range<IRng> && no_throw_forward_range<ORng> &&
        constructible_from<iter_value_t<iterator_t<ORng>>, iter_reference_t<iterator_t<IRng>>>,
        uninitialized_copy_result<safe_iterator_t<IRng>, safe_iterator_t<ORng>>>
    operator()(IRng&& irng, ORng&& orng) const
    {
        return uninitialized_copy_fn::impl4(
                nano::begin(irng), nano::end(irng),
                nano::begin(orng), nano::end(orng));
    }

    // Three-legged
    template <typename I, typename S, typename O>
    NANO_DEPRECATED
    std::enable_if_t<
        input_iterator<I> && sentinel_for<S, I> &&
            no_throw_forward_iterator<O> &&
        constructible_from<iter_value_t<O>, iter_reference_t<I>>,
        uninitialized_copy_result<I, O>>
    operator()(I ifirst, S ilast, O ofirst) const
    {
        return uninitialized_copy_fn::impl3(std::move(ifirst), std::move(ilast),
                                            std::move(ofirst));
    }

    // Range and a half
    template <typename IRng, typename O>
    NANO_DEPRECATED
    std::enable_if_t<
        input_range<IRng> && no_throw_forward_iterator<std::decay_t<O>> &&
        !no_throw_forward_range<O> &&
        constructible_from<iter_value_t<std::decay_t<O>>, iter_reference_t<iterator_t<IRng>>>,
        uninitialized_copy_result<safe_iterator_t<IRng>, std::decay_t<O>>>
    operator()(IRng&& irng, O&& ofirst) const
    {
        return uninitialized_copy_fn::impl3(
                nano::begin(irng), nano::end(irng), std::forward<O>(ofirst));
    }
};

}

NANO_INLINE_VAR(detail::uninitialized_copy_fn, uninitialized_copy)

template <typename I, typename O>
using uninitialized_copy_n_result = uninitialized_copy_result<I, O>;

namespace detail {

struct uninitialized_copy_n_fn {
    template <typename I, typename O, typename S>
    std::enable_if_t<
        input_iterator<I> && no_throw_forward_iterator<O> &&
            no_throw_sentinel<S, O> &&
        constructible_from<iter_value_t<O>, iter_reference_t<I>>,
        uninitialized_copy_n_result<I, O>>
    operator()(I ifirst, iter_difference_t<I> n, O ofirst, S olast) const
    {
        auto t = uninitialized_copy_fn::impl4(
                    make_counted_iterator(std::move(ifirst), n),
                    default_sentinel, std::move(ofirst), std::move(olast));
        return {std::move(t).in.base(), std::move(t).out};
    }

    template <typename I, typename O>
    NANO_DEPRECATED
    std::enable_if_t<
        input_iterator<I> && no_throw_forward_iterator<O> &&
        constructible_from<iter_value_t<O>, iter_reference_t<I>>,
        uninitialized_copy_n_result<I, O>>
    operator()(I ifirst, iter_difference_t<I> n, O ofirst) const
    {
        auto t = uninitialized_copy_fn::impl3(
                make_counted_iterator(std::move(ifirst), n),
                default_sentinel, std::move(ofirst));
        return {std::move(t).in.base(), std::move(t).out};
    }

};

}

NANO_INLINE_VAR(detail::uninitialized_copy_n_fn, uninitialized_copy_n)

NANO_END_NAMESPACE

#endif
