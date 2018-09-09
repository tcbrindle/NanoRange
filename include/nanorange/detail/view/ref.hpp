// nanorange/detail/view/ref.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_DETAIL_VIEW_REF_HPP_INCLUDED
#define NANORANGE_DETAIL_VIEW_REF_HPP_INCLUDED

#include <nanorange/view/interface.hpp>

NANO_BEGIN_NAMESPACE

namespace detail {

template <typename Rng>
class ref_view : public view_interface<ref_view<Rng>> {

    static_assert(Range<Rng> && std::is_object<Rng>::value && !View<Rng>, "");

    Rng* rng_ = nullptr;

public:
    constexpr ref_view() noexcept = default;

    constexpr ref_view(Rng& rng) noexcept
        : rng_(std::addressof(rng))
    {}

    constexpr Rng& base() const { return *rng_; }

    constexpr iterator_t<Rng> begin() const
        noexcept(noexcept(ranges::begin(*rng_)))
    {
        return ranges::begin(*rng_);
    }

    constexpr sentinel_t<Rng> end() const
        noexcept(noexcept(ranges::end(*rng_)))
    {
        return ranges::end(*rng_);
    }

    constexpr auto empty() const
        noexcept(noexcept(ranges::empty(*rng_)))
        -> decltype(static_cast<bool>(ranges::empty(*rng_)))
    {
        return ranges::empty(*rng_);
    }

    template <typename R = Rng, std::enable_if_t<SizedRange<R>, int> = 0>
    constexpr auto size() const
        noexcept(noexcept(ranges::size(*rng_)))
    {
        return ranges::size(*rng_);
    }

    template <typename R = Rng, std::enable_if_t<ContiguousRange<R>, int> = 0>
    constexpr auto data() const
        noexcept(noexcept(ranges::data(*rng_)))
    {
        return ranges::data(*rng_);
    }

    friend constexpr iterator_t<Rng> begin(ref_view&& r)
        noexcept(noexcept(r.begin()))
    {
        return r.begin();
    }

    friend constexpr sentinel_t<Rng> end(ref_view&& r)
        noexcept(noexcept(r.end()))
    {
        return r.end();
    }
};

template <typename Rng>
constexpr std::enable_if_t<
    Range<Rng> &&
    std::is_object<Rng>::value &&
    !View<Rng>,
    ref_view<Rng>>
make_ref_view(Rng& rng) noexcept
{
    return ref_view<Rng>{rng};
}

} // namespace detail

NANO_END_NAMESPACE

#endif