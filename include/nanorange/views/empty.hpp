// nanorange/views/empty.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_VIEWS_EMPTY_HPP_INCLUDED
#define NANORANGE_VIEWS_EMPTY_HPP_INCLUDED

#include <nanorange/views/interface.hpp>

NANO_BEGIN_NAMESPACE

namespace empty_view_ {

template <typename T>
class empty_view : public view_interface<empty_view<T>> {
    static_assert(std::is_object<T>::value);

public:
    static constexpr T* begin() noexcept { return nullptr; }
    static constexpr T* end() noexcept { return nullptr; }
    static constexpr std::size_t size() noexcept { return 0; }
    static constexpr T* data() noexcept { return nullptr; }
    static constexpr bool empty() noexcept { return true; }
};

}

using empty_view_::empty_view;

template <typename T>
inline constexpr bool enable_borrowed_range<empty_view<T>> = true;

namespace views {

template <typename T, typename = std::enable_if_t<std::is_object<T>::value>>
inline constexpr empty_view<T> empty{};

}


NANO_END_NAMESPACE

#endif
