// nanorange/detail/views/ref.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_VIEWS_REF_HPP_INCLUDED
#define NANORANGE_VIEWS_REF_HPP_INCLUDED

#include <nanorange/views/interface.hpp>

NANO_BEGIN_NAMESPACE

namespace ref_view_ {

template <typename R>
class ref_view : public view_interface<ref_view<R>> {

    static_assert(range<R> && std::is_object<R>::value, "");

    R* r_ = nullptr;

    struct constructor_req {
        static void FUN(R&);
        static void FUN(R&&) = delete;

        template <typename T>
        auto requires_() -> decltype(FUN(std::declval<T>()));
    };

public:
    constexpr ref_view() noexcept = default;

    template <typename T,
              std::enable_if_t<detail::not_same_as<T, ref_view>, int> = 0,
              std::enable_if_t<detail::requires_<constructor_req, T>, int> = 0,
              std::enable_if_t<convertible_to<T, R&>, int> = 0>
    constexpr ref_view(T&& t)
        : r_(std::addressof(static_cast<R&>(std::forward<T>(t))))
    {}

    constexpr R& base() const { return *r_; }

    constexpr iterator_t<R> begin() const { return ranges::begin(*r_); }

    constexpr sentinel_t<R> end() const { return ranges::end(*r_); }

    template <typename RR = R,
              typename = decltype(ranges::empty(std::declval<RR&>()))>
    constexpr bool empty() const
    {
        return ranges::empty(*r_);
    }

    template <typename RR = R, std::enable_if_t<sized_range<RR>, int> = 0>
    constexpr auto size() const
    {
        return ranges::size(*r_);
    }

    template <typename RR = R, std::enable_if_t<contiguous_range<RR>, int> = 0>
    constexpr auto data() const
    {
        return ranges::data(*r_);
    }

    friend constexpr iterator_t<R> begin(ref_view r) { return r.begin(); }

    friend constexpr sentinel_t<R> end(ref_view r) { return r.end(); }
};

template <typename R, std::enable_if_t<range<R> && std::is_object_v<R>, int> = 0>
ref_view(R&) -> ref_view<R>;

} // namespace ref_view_

using ref_view_::ref_view;

NANO_END_NAMESPACE

#endif