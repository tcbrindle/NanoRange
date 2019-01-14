// nanorange/iterator/istreambuf_iterator.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_ITERATOR_ISTREAMBUF_ITERATOR_HPP_INCLUDED
#define NANORANGE_ITERATOR_ISTREAMBUF_ITERATOR_HPP_INCLUDED

#include <nanorange/detail/iterator/traits.hpp>
#include <nanorange/iterator/default_sentinel.hpp>

#include <iosfwd>

NANO_BEGIN_NAMESPACE

template <typename CharT, typename Traits = std::char_traits<CharT>>
class istreambuf_iterator {
    class proxy {
        friend class istreambuf_iterator;
        CharT keep_;
        std::basic_streambuf<CharT, Traits>* sbuf_;

        proxy(CharT c, std::basic_streambuf<CharT, Traits>* sbuf)
            : keep_(c), sbuf_(sbuf)
        {}
    public:
        CharT operator*() const { return keep_; }
    };

public:
    using iterator_category = input_iterator_tag;
    using value_type = CharT;
    using difference_type = typename Traits::off_type;
    using reference = CharT;
    using pointer = CharT*;
    using char_type = CharT;
    using traits_type = Traits;
    using int_type = typename Traits::int_type;
    using streambuf_type = std::basic_streambuf<CharT, Traits>;
    using istream_type = std::basic_istream<CharT, Traits>;

    constexpr istreambuf_iterator() noexcept = default;

    constexpr istreambuf_iterator(default_sentinel_t) noexcept {}

    istreambuf_iterator(const istreambuf_iterator&) noexcept = default;

    ~istreambuf_iterator() = default;

    istreambuf_iterator(istream_type& s) noexcept
        : sbuf_(s.rdbuf())
    {}

    istreambuf_iterator(streambuf_type* s) noexcept
        : sbuf_(s)
    {}

    istreambuf_iterator(const proxy& p) noexcept
        : sbuf_(p.sbuf_)
    {}

    char_type operator*() const { return Traits::to_char_type(sbuf_->sgetc()); }

    istreambuf_iterator& operator++()
    {
        sbuf_->sbumpc();
        return *this;
    }

    proxy operator++(int)
    {
        return proxy(Traits::to_char_type(sbuf_->sbumpc()), sbuf_);
    }

    bool equal(const istreambuf_iterator& b) const
    {
        return  is_eof() == b.is_eof();
    }

private:
    bool is_eof() const
    {
        if (sbuf_ && sbuf_->sgetc() == Traits::eof()) {
            sbuf_ = nullptr;
            return true;
        }

        return sbuf_ == nullptr;
    }

    mutable streambuf_type* sbuf_ = nullptr;
};

template <typename CharT, typename Traits>
bool operator==(const istreambuf_iterator<CharT, Traits>& a,
                const istreambuf_iterator<CharT, Traits>& b)
{
    return a.equal(b);
}

template <typename CharT, typename Traits>
bool operator==(default_sentinel_t,
                const istreambuf_iterator<CharT, Traits>& b)
{
    return istreambuf_iterator<CharT, Traits>{}.equal(b);
}

template <typename CharT, typename Traits>
bool operator==(const istreambuf_iterator<CharT, Traits>& a,
                default_sentinel_t)
{
    return a.equal(istreambuf_iterator<CharT, Traits>{});
}

template <typename CharT, typename Traits>
bool operator!=(const istreambuf_iterator<CharT, Traits>& a,
                const istreambuf_iterator<CharT, Traits>& b)
{
    return !(a == b);
}

template <typename CharT, typename Traits>
bool operator!=(default_sentinel_t a,
                const istreambuf_iterator<CharT, Traits>& b)
{
    return !(a == b);
}

template <typename CharT, typename Traits>
bool operator!=(const istreambuf_iterator<CharT, Traits>& a,
                default_sentinel_t b)
{
    return !(a == b);
}

NANO_END_NAMESPACE

#endif
