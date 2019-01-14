// nanorange/iterator/istream_iterator.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_ITERATOR_ISTREAM_ITERATOR_HPP_INCLUDED
#define NANORANGE_ITERATOR_ISTREAM_ITERATOR_HPP_INCLUDED

#include <nanorange/iterator/default_sentinel.hpp>
#include <nanorange/detail/iterator/traits.hpp>

#include <iosfwd>

NANO_BEGIN_NAMESPACE

template <typename T, typename CharT = char, typename Traits = std::char_traits<CharT>,
          typename Distance = std::ptrdiff_t>
class istream_iterator {
public:
    using iterator_category = input_iterator_tag;
    using difference_type = Distance;
    using value_type = T;
    using reference = const T&;
    using pointer = const T*;
    using char_type = CharT;
    using traits_type = Traits;
    using istream_type = std::basic_istream<CharT, Traits>;

    constexpr istream_iterator() = default;

    constexpr istream_iterator(default_sentinel_t) {}

    istream_iterator(istream_type& s)
        : in_stream_(std::addressof(s))
    {
        s >> value_;
    }

    istream_iterator(const istream_iterator& x) = default;

    ~istream_iterator() = default;

    const T& operator*() const { return value_; }

    const T* operator->() const { return std::addressof(value_); }

    istream_iterator& operator++()
    {
        *in_stream_ >> value_;
        if (in_stream_->fail()) {
            in_stream_ = nullptr;
        }
        return *this;
    }

    istream_iterator operator++(int)
    {
        istream_iterator tmp = *this;
        this->operator++();
        return tmp;
    }

    friend bool operator==(const istream_iterator& x, const istream_iterator& y)
    {
        return x.in_stream_ == y.in_stream_;
    }

    friend bool operator==(default_sentinel_t, const istream_iterator y)
    {
        return nullptr == y.in_stream_;
    }

    friend bool operator==(const istream_iterator& x, default_sentinel_t)
    {
        return x.in_stream_ == nullptr;
    }

    friend bool operator!=(const istream_iterator& x, const istream_iterator& y)
    {
        return !(x == y);
    }

    friend bool operator!=(default_sentinel_t x, const istream_iterator y)
    {
        return !(x == y);
    }

    friend bool operator!=(const istream_iterator& x, default_sentinel_t y)
    {
        return !(x == y);
    }

private:
    istream_type* in_stream_ = nullptr;
    T value_{};
};

NANO_END_NAMESPACE

#endif
