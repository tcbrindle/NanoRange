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

template <typename R>
class ref_view : public view_interface<ref_view<R>> {

    static_assert(Range<R> && std::is_object<R>::value, "");

    R* r_ = nullptr;

    struct constructor_req {
        static void FUN(R&);
        static void FUN(R&&) = delete;

        template <typename T>
        auto requires_() -> decltype(FUN(std::declval<T>()));
    };

public:
    constexpr ref_view() noexcept = default;

    template <typename T, std::enable_if_t<
            detail::NotSameAs<T, ref_view> &&
            ConvertibleTo<T, R&> &&
            detail::requires_<constructor_req, T>, int> = 0>
    constexpr ref_view(T&& t)
        : r_(std::addressof(static_cast<R&>(std::forward<T>(t))))
    {}

    constexpr R& base() const { return *r_; }

    constexpr iterator_t<R> begin() const { return ranges::begin(*r_); }

    constexpr sentinel_t<R> end() const { return ranges::end(*r_); }

    constexpr auto empty() const
        -> decltype(static_cast<bool>(ranges::empty(*r_)))
    {
        return ranges::empty(*r_);
    }

    template <typename RR = R, std::enable_if_t<SizedRange<RR>, int> = 0>
    constexpr auto size() const
    {
        return ranges::size(*r_);
    }

    template <typename RR = R, std::enable_if_t<ContiguousRange<RR>, int> = 0>
    constexpr auto data() const
    {
        return ranges::data(*r_);
    }

    friend constexpr iterator_t<R> begin(ref_view r) { return r.begin(); }

    friend constexpr sentinel_t<R> end(ref_view r) { return r.end(); }
};

template <typename Rng>
constexpr std::enable_if_t<
    Range<Rng> &&
    std::is_object<Rng>::value,
    ref_view<Rng>>
make_ref_view(Rng& rng)
{
    return ref_view<Rng>{rng};
}

template <typename T>
void make_ref_view(const T&&) = delete;

} // namespace detail

NANO_END_NAMESPACE

#endif