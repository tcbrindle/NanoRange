// nanorange/algorithm/sample.hpp
//
//===----------------------------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#ifndef NANORANGE_ALGORITHM_SAMPLE_HPP_INCLUDED
#define NANORANGE_ALGORITHM_SAMPLE_HPP_INCLUDED

#include <nanorange/ranges.hpp>
#include <nanorange/random.hpp>
#include <nanorange/algorithm/min.hpp>

#include <random>

NANO_BEGIN_NAMESPACE

namespace detail {

struct sample_fn {
private:
    template <typename I, typename S, typename O, typename Gen>
    static O impl_fwd(I first, S last, O out, iter_difference_t<I> n, Gen& g)
    {
        using diff_t = iter_difference_t<I>;
        using distr_t = std::uniform_int_distribution<diff_t>;
        using param_t = typename distr_t::param_type;

        distr_t D;

        auto unsampled_size = nano::distance(first, last);

        for (n = (nano::min)(n, unsampled_size); n != 0; ++first) {
            if (D(g, param_t(0, --unsampled_size)) < n) {
                *out++ = *first;
                --n;
            }
        }

        return out;
    }
    template <typename I, typename S, typename O, typename Gen>
    static O impl_ra(I first, S last, O out, iter_difference_t<I> n, Gen& g) {
        using diff_t = iter_difference_t<I>;
        using distr_t = std::uniform_int_distribution<diff_t>;
        using param_t = typename distr_t::param_type;

        distr_t D;
        diff_t k = 0;

        for(; first != last && k < n; ++first, (void) ++k) {
            out[k] = *first;
        }

        diff_t size = k;
        for (; first != last; ++first, (void) ++k) {
            diff_t r = distr_t(0, k)(g);
            if (D(g, param_t(0, k)) < size) {
                out[r] = *first;
            }
        }

        return out + (nano::min)(n, k);
    }

    template <typename I, typename S, typename O, typename Gen>
    static O impl(I first, S last, O out, iter_difference_t<I> n, Gen& g)
    {
        if constexpr (nano::forward_iterator<I>) {
            return impl_fwd(std::move(first), std::move(last), std::move(out), n, g);
	} else {
            return impl_ra(std::move(first), std::move(last), std::move(out), n, g);
	}
    }

public:
    template <typename I, typename S, typename O, typename Gen>
    std::enable_if_t<
        input_iterator<I> &&
        sentinel_for<S, I> &&
        weakly_incrementable<O> &&
        (forward_iterator<I> || random_access_iterator<O>) &&
        indirectly_copyable<I, O> &&
        uniform_random_bit_generator<std::remove_reference_t<Gen>>,
        O>
    operator()(I first, S last, O out, iter_difference_t<I> n, Gen&& gen) const
    {
        return sample_fn::impl(std::move(first), std::move(last),
                               std::move(out), std::move(n),
                               std::forward<Gen>(gen));
    }

    template <typename Rng, typename O, typename Gen>
    std::enable_if_t<
        input_range<Rng> &&
        weakly_incrementable<O> &&
        (forward_range<Rng> || random_access_iterator<O> ) &&
        indirectly_copyable<iterator_t<Rng>, O> &&
        uniform_random_bit_generator<std::remove_reference_t<Gen>>,
        O>
    operator()(Rng&& rng, O out, range_difference_t<Rng> n, Gen&& gen) const
    {
        return sample_fn::impl(nano::begin(rng), nano::end(rng),
                               std::move(out), std::move(n), std::forward<Gen>(gen));
    }
};

}

NANO_INLINE_VAR(detail::sample_fn, sample)

NANO_END_NAMESPACE

#endif
