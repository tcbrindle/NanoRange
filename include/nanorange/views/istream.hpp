// nanorange/views/istream.hpp
//
// Copyright (c) 2019 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_VIEWS_ISTREAM_HPP_INCLUDED
#define NANORANGE_VIEWS_ISTREAM_HPP_INCLUDED

#include <nanorange/detail/iterator/traits.hpp>
#include <nanorange/iterator/default_sentinel.hpp>
#include <nanorange/views/interface.hpp>

#include <iosfwd>

NANO_BEGIN_NAMESPACE

namespace detail {

struct StreamExtractable_req {
    template <typename Val, typename CharT, typename Traits>
    auto requires_(std::basic_istream<CharT, Traits>& is, Val& t)
        -> decltype(is >> t);
};

template <typename Val, typename CharT, typename Traits>
NANO_CONCEPT StreamExtractable =
    requires_<StreamExtractable_req, Val, CharT, Traits>;

} // namespace detail

template <typename Val, typename CharT, typename Traits = std::char_traits<CharT>>
struct basic_istream_view : view_interface<basic_istream_view<Val, CharT, Traits>> {

    static_assert(movable<Val>);
    static_assert(default_constructible<Val>);
    static_assert(detail::StreamExtractable<Val, CharT, Traits>);

    basic_istream_view() = default;

    constexpr explicit basic_istream_view(std::basic_istream<CharT, Traits>& stream)
        : stream_(std::addressof(stream))
    {}

    constexpr auto begin()
    {
        if (stream_) {
            *stream_ >> object_;
        }
        return iterator{*this};
    }

    constexpr default_sentinel_t end() const noexcept
    {
        return default_sentinel;
    }

private:
    struct iterator {
        using iterator_category = input_iterator_tag;
        using difference_type = std::ptrdiff_t;
        using value_type = Val;

        iterator() = default;

        constexpr explicit iterator(basic_istream_view& parent) noexcept
            : parent_(std::addressof(parent))
        {}

        // Disable move-only iterator until views support them properly
#if 0
        iterator(const iterator&) = delete;
        iterator(iterator&&) = default;

        iterator& operator=(const iterator&) = delete;
        iterator& operator=(iterator&&) = default;
#endif
        iterator& operator++()
        {
            *parent_->stream_ >> parent_->object_;
            return *this;
        }

        void operator++(int)
        {
            ++*this;
        }

        Val& operator*() const { return parent_->object_; }

        friend bool operator==(const iterator& x, default_sentinel_t)
        {
            return x.done();
        }

        friend bool operator==(default_sentinel_t s, const iterator& x)
        {
            return x == s;
        }

        friend bool operator!=(const iterator& x, default_sentinel_t s)
        {
            return !(x == s);
        }

        friend bool operator!=(default_sentinel_t s, const iterator& x)
        {
            return !(x == s);
        }

    private:
        [[nodiscard]] bool done() const
        {
            return parent_ == nullptr
                  || parent_->stream_ == nullptr
                  || !*parent_->stream_;
        }

        basic_istream_view* parent_{};
    };

    std::basic_istream<CharT, Traits>* stream_{};
    Val object_ = Val();
};

template <typename Val, typename CharT, typename Traits>
auto istream_view(std::basic_istream<CharT, Traits>& s)
    -> basic_istream_view<Val, CharT, Traits>
{
    return basic_istream_view<Val, CharT, Traits>{s};
}

NANO_END_NAMESPACE

#endif
