// nanorange/view/empty.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_VIEW_EMPTY_HPP_INCLUDED
#define NANORANGE_VIEW_EMPTY_HPP_INCLUDED

#include <nanorange/view/interface.hpp>

NANO_BEGIN_NAMESPACE

namespace empty_view_ {

template <typename T>
class empty_view : view_interface<empty_view<T>> {
    static_assert(std::is_object<T>::value, "");

public:
    static constexpr T* begin() noexcept { return nullptr; }
    static constexpr T* end() noexcept { return nullptr; }
    static constexpr std::ptrdiff_t size() noexcept { return 0; }
    static constexpr T* data() noexcept { return nullptr; }

    static constexpr bool empty() noexcept { return true; }

    friend constexpr T* begin(const empty_view&&) noexcept { return nullptr; }
    friend constexpr T* end(const empty_view&&) noexcept { return nullptr; }
};

}

using empty_view_::empty_view;

namespace view {

#ifdef NANO_HAVE_INLINE_VARS
template <typename T, typename = std::enable_if_t<std::is_object<T>::value>>
inline constexpr empty_view<T> empty{};
#else
template <typename T, typename = std::enable_if_t<std::is_object<T>::value>>
constexpr const auto& empty = nano::detail::static_const_<empty_view<T>>::value;
#endif // NANO_HAVE_INLINE_VARS

}


NANO_END_NAMESPACE

#endif