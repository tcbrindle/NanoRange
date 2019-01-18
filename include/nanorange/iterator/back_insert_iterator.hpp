// nanorange/iterator/back_insert_iterator.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_ITERATOR_BACK_INSERT_ITERATOR_HPP_INCLUDED
#define NANORANGE_ITERATOR_BACK_INSERT_ITERATOR_HPP_INCLUDED

#include <nanorange/detail/ranges/concepts.hpp>
#include <iterator>

NANO_BEGIN_NAMESPACE

template <typename Container>
struct back_insert_iterator {
    using container_type = Container;
    using difference_type = std::ptrdiff_t;

    constexpr back_insert_iterator() = default;

    explicit back_insert_iterator(Container& x) : cont_(std::addressof(x)) {}

    back_insert_iterator& operator=(const iter_value_t<iterator_t<Container>>& value)
    {
        cont_->push_back(value);
        return *this;
    }

    back_insert_iterator& operator=(iter_value_t<iterator_t<Container>>&& value)
    {
        cont_->push_back(std::move(value));
        return *this;
    }

    back_insert_iterator& operator*() { return *this; }
    back_insert_iterator& operator++() { return *this; }
    back_insert_iterator& operator++(int) { return *this; }

private:
    container_type* cont_ = nullptr;
};

template <typename Container>
back_insert_iterator<Container> back_inserter(Container& x)
{
    return back_insert_iterator<Container>(x);
}

NANO_END_NAMESPACE

namespace std {

template <typename Cont>
struct iterator_traits<::nano::back_insert_iterator<Cont>> {
    using value_type = void;
    using difference_type = ptrdiff_t;
    using reference = void;
    using pointer = void;
    using iterator_category = std::output_iterator_tag;
};

} // namespace std

#endif
