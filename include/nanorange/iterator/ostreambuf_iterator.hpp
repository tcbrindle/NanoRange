// nanorange/iterator/ostreambuf_iterator.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_ITERATOR_OSTREAMBUF_ITERATOR_HPP_INCLUDED
#define NANORANGE_ITERATOR_OSTREAMBUF_ITERATOR_HPP_INCLUDED

#include <nanorange/detail/macros.hpp>

#include <iosfwd> // for basic_streambuf
#include <iterator>

NANO_BEGIN_NAMESPACE

template <typename CharT, typename Traits = std::char_traits<CharT>>
struct ostreambuf_iterator {

    using char_type = CharT;
    using traits = Traits;
    using difference_type = std::ptrdiff_t;
    using streambuf_type = std::basic_streambuf<CharT, Traits>;
    using ostream_type = std::basic_ostream<CharT, Traits>;

    constexpr ostreambuf_iterator() = default;

    ostreambuf_iterator(ostream_type& s) noexcept : sbuf_(s.rdbuf()) {}

    ostreambuf_iterator(streambuf_type* s) noexcept : sbuf_(s) {}

    ostreambuf_iterator& operator=(char_type c)
    {
        if (!failed()) {
            failed_ = (sbuf_->sputc(c) == traits::eof());
        }
        return *this;
    }

    ostreambuf_iterator& operator*() { return *this; }
    ostreambuf_iterator& operator++() { return *this; }
    ostreambuf_iterator& operator++(int) { return *this; }

    bool failed() const noexcept { return failed_; }

private:
    streambuf_type* sbuf_ = nullptr;
    bool failed_ = false;
};

NANO_END_NAMESPACE

namespace std {

template <typename C, typename T>
struct iterator_traits<::nano::ranges::ostreambuf_iterator<C, T>> {
    using value_type = void;
    using difference_type = ptrdiff_t;
    using reference = void;
    using pointer = void;
    using iterator_category = output_iterator_tag;
};

} // namespace std

#endif
