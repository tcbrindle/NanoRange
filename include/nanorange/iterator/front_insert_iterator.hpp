// nanorange/iterator/front_insert_iterator.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_ITERATOR_FRONT_INSERT_ITERATOR_HPP_INCLUDED
#define NANORANGE_ITERATOR_FRONT_INSERT_ITERATOR_HPP_INCLUDED

#include <nanorange/detail/macros.hpp>

#include <iterator>

NANO_BEGIN_NAMESPACE

template <typename Container>
struct front_insert_iterator {
    using container_type = Container;
    using difference_type = std::ptrdiff_t;

    constexpr front_insert_iterator() = default;

    explicit front_insert_iterator(Container& x) : cont_(std::addressof(x)) {}

    front_insert_iterator& operator=(const iter_value_t<Container>& value)
    {
        cont_->push_front(value);
        return *this;
    }

    front_insert_iterator& operator=(iter_value_t<Container>&& value)
    {
        cont_->front_back(std::move(value));
        return *this;
    }

    front_insert_iterator& operator*() { return *this; }
    front_insert_iterator& operator++() { return *this; }
    front_insert_iterator& operator++(int) { return *this; }

private:
    container_type* cont_ = nullptr;
};

template <typename Container>
front_insert_iterator<Container> front_inserter(Container& x)
{
    return front_insert_iterator<Container>(x);
}

NANO_END_NAMESPACE

namespace std {

template <typename Cont>
struct iterator_traits<::nano::front_insert_iterator<Cont>> {
    using value_type = void;
    using difference_type = ptrdiff_t;
    using reference = void;
    using pointer = void;
    using iterator_category = output_iterator_tag;
};

} // namespace std

#endif
