// nanorange/views/single.hpp
//
// Copyright (c) 2019 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_VIEWS_SINGLE_HPP_INCLUDED
#define NANORANGE_VIEWS_SINGLE_HPP_INCLUDED

#include <nanorange/detail/views/semiregular_box.hpp>
#include <nanorange/views/interface.hpp>

NANO_BEGIN_NAMESPACE

template <typename T>
struct single_view : view_interface<single_view<T>> {
    static_assert(copy_constructible<T>);
    static_assert(std::is_object<T>::value);

    single_view() = default;

    constexpr explicit single_view(const T& t)
        : value_(t)
    {}

    constexpr explicit single_view(T&& t)
        : value_(std::move(t))
    {}

    template <typename... Args,
              std::enable_if_t<constructible_from<T, Args...>, int> = 0>
    constexpr single_view(std::in_place_t, Args&&... args)
        : value_{std::in_place, std::forward<Args>(args)...}
    {}

    constexpr T* begin() noexcept { return data(); }
    constexpr const T* begin() const noexcept { return data(); }

    constexpr T* end() noexcept { return data() + 1; }
    constexpr const T* end() const noexcept { return data() + 1; }

    static constexpr std::size_t size() { return 1; }

    constexpr T* data() noexcept { return value_.operator->(); }
    constexpr const T* data() const noexcept { return value_.operator->(); }

private:
    detail::semiregular_box<T> value_;
};

namespace views {

namespace detail {

struct single_view_fn {
    template <typename T>
    constexpr auto operator()(T&& t) const
        noexcept(noexcept(single_view{std::forward<T>(t)}))
        -> decltype(single_view{std::forward<T>(t)})
    {
        return single_view{std::forward<T>(t)};
    }
};

}

NANO_INLINE_VAR(detail::single_view_fn, single)

}


NANO_END_NAMESPACE

#endif