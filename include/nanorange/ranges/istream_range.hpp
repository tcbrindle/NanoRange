// nanorange/ranges/istream_range.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_RANGES_ISTREAM_RANGE_HPP_INCLUDED
#define NANORANGE_RANGES_ISTREAM_RANGE_HPP_INCLUDED

#include <nanorange/detail/iterator/traits.hpp>
#include <nanorange/iterator/default_sentinel.hpp>

#include <iosfwd>

NANO_BEGIN_NAMESPACE

template <typename T, typename CharT = char, typename Traits = std::char_traits<CharT>,
          typename Distance = std::ptrdiff_t>
class istream_range {
public:

    class iterator {
    public:
        using iterator_category = ranges::input_iterator_tag;
        using difference_type = Distance;
        using value_type = T;
        using reference = const T&;
        using pointer = const T*;
        using char_type = CharT;
        using traits = Traits;

        constexpr iterator() = default;

        explicit iterator(istream_range& rng)
            : rng_(std::addressof(rng))
        {}

        reference operator*() const { return rng_->value_; }

        pointer operator->() const { return std::addressof(rng_->value_); }

        iterator& operator++()
        {
            rng_->next();
            return *this;
        }

        iterator operator++(int)
        {
            auto tmp = *this;
            this->operator++();
            return tmp;
        }

        friend bool operator==(const iterator& lhs, const iterator& rhs)
        {
            return lhs.rng_ == rhs.rng_;
        }

        friend bool operator!=(const iterator& lhs, const iterator& rhs)
        {
            return !(lhs == rhs);
        }

        friend bool operator==(const iterator& lhs, default_sentinel_t)
        {
            return lhs.done();
        }

        friend bool operator!=(const iterator& lhs, default_sentinel_t rhs)
        {
            return !(lhs == rhs);
        }

        friend bool operator==(default_sentinel_t, const iterator& rhs)
        {
            return rhs.done();
        }

        friend bool operator!=(default_sentinel_t lhs, const iterator& rhs)
        {
            return !(lhs == rhs);
        }

    private:
        bool done() const
        {
            return rng_ == nullptr || rng_->in_stream_ == nullptr;
        }

        istream_range* rng_ = nullptr;
    };

    using sentinel = default_sentinel_t;
    using istream_type = std::basic_istream<CharT, Traits>;

    constexpr istream_range() = default;

    explicit istream_range(istream_type& s)
        : in_stream_(std::addressof(s))
    {
        next();
    }

    iterator begin() { return iterator{*this}; }

    sentinel end() { return {}; }

private:
    void next()
    {
        if (!(*in_stream_ >> value_)) {
            in_stream_ = nullptr;
        }
    }

    istream_type* in_stream_ = nullptr;
    T value_{};
};

NANO_END_NAMESPACE

#endif
