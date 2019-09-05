// nanorange/detail/views/semiregular_box.hpp
//
// Copyright (c) 2019 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_DETAIL_VIEW_SEMIREGULAR_BOX_HPP_INCLUDED
#define NANORANGE_DETAIL_VIEW_SEMIREGULAR_BOX_HPP_INCLUDED

#include <nanorange/detail/concepts/core.hpp>

#include <optional>

NANO_BEGIN_NAMESPACE

namespace detail {

template <typename T>
struct semiregular_box : std::optional<T>
{
    static_assert(copy_constructible<T>);
    static_assert(std::is_object_v<T>);

private:
    std::optional<T>& base() { return *this; }
    std::optional<T> const& base() const { return *this; }

public:
    template <typename U = T,
              std::enable_if_t<default_constructible<U>, int> = 0>
    constexpr semiregular_box()
        noexcept(std::is_nothrow_default_constructible_v<T>)
        : semiregular_box{std::in_place}
    {}

    template <typename U = T,
              std::enable_if_t<!default_constructible<U>, int> = 0>
    constexpr semiregular_box() {}

    // All other constructors get forwarded to optional -- but don't hijack
    // copy/move construct
    template <typename Arg0, typename... Args,
              std::enable_if_t<
                  constructible_from<std::optional<T>, Arg0, Args...> &&
                  !same_as<remove_cvref_t<Arg0>, semiregular_box>, int> = 0>
    constexpr semiregular_box(Arg0&& arg0, Args&&... args)
        : std::optional<T>{std::forward<Arg0>(arg0), std::forward<Args>(args)...}
    {}

    constexpr semiregular_box(const semiregular_box&) = default;
    constexpr semiregular_box(semiregular_box&&) = default;

    semiregular_box& operator=(const semiregular_box& other)
    {
        if constexpr (assignable_from<T&, const T&>) {
            base() = other.base();
        } else {
            if (other) {
                this->emplace(*other);
            } else {
                this->reset();
            }
        }

        return *this;
    }

    semiregular_box& operator=(semiregular_box&& other) noexcept
    {
        if constexpr (assignable_from<T&, T>) {
            base() = std::move(other.base());
        } else {
            if (other) {
                this->emplace(std::move(*other));
            } else {
                this->reset();
            }
        }

        return *this;
    }
};

} // namespace detail

NANO_END_NAMESPACE

#endif
