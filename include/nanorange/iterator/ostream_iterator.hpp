// nanorange/iterator/ostream_iterator.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_ITERATOR_OSTREAM_ITERATOR_HPP_INCLUDED
#define NANORANGE_ITERATOR_OSTREAM_ITERATOR_HPP_INCLUDED

#include <nanorange/detail/macros.hpp>

#include <iosfwd>
#include <iterator>
#include <memory>

NANO_BEGIN_NAMESPACE

template <typename T, typename CharT = char, typename Traits = std::char_traits<CharT>>
struct ostream_iterator
{
    using char_type = CharT;
    using traits_type = Traits;
    using ostream_type = std::basic_ostream<CharT, Traits>;
    using difference_type = std::ptrdiff_t;

    constexpr ostream_iterator() noexcept = default;

    ostream_iterator(ostream_type& os, const CharT* delim = nullptr) noexcept
            : os_(std::addressof(os)), delim_(delim)
    {}

    ostream_iterator& operator=(const T& value)
    {
        *os_ << value;
        if (delim_) {
            *os_ << delim_;
        }
        return *this;
    }

    ostream_iterator& operator*() { return *this; }
    ostream_iterator& operator++() { return *this; }
    ostream_iterator& operator++(int) { return *this; }

private:
    ostream_type* os_ = nullptr;
    const char_type* delim_ = nullptr;
};

NANO_END_NAMESPACE

namespace std {

template <typename T, typename C, typename Tr>
struct iterator_traits<::nano::ranges::ostream_iterator<T, C, Tr>> {
    using value_type = void;
    using difference_type = ptrdiff_t;
    using reference = void;
    using pointer = void;
    using iterator_category = std::output_iterator_tag;
};

}

#endif