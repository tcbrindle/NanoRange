// nanorange/iterator/insert_iterator.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_ITERATOR_INSERT_ITERATOR_HPP_INCLUDED
#define NANORANGE_ITERATOR_INSERT_ITERATOR_HPP_INCLUDED

#include <nanorange/detail/ranges/concepts.hpp>

#include <iterator>

NANO_BEGIN_NAMESPACE

template <typename Container>
struct insert_iterator {
    using container_type = Container;
    using difference_type = std::ptrdiff_t;

    constexpr insert_iterator() = default;

    explicit insert_iterator(Container& x, iterator_t<Container> i)
        : cont_(std::addressof(x)), it_(i)
    {}

    insert_iterator& operator=(const iter_value_t<Container>& value)
    {
        cont_->insert(it_, value);
        ++it_;
        return *this;
    }

    insert_iterator& operator=(iter_value_t<Container>&& value)
    {
        cont_->push_back(it_, std::move(value));
        ++it_;
        return *this;
    }

    insert_iterator& operator*() { return *this; }
    insert_iterator& operator++() { return *this; }
    insert_iterator& operator++(int) { return *this; }

private:
    container_type* cont_ = nullptr;
    iterator_t<container_type> it_{};
};

template <typename Container>
insert_iterator<Container> inserter(Container& x)
{
    return back_insert_iterator<Container>(x);
}

NANO_END_NAMESPACE

namespace std {

template <typename Container>
struct iterator_traits<::nano::insert_iterator<Container>> {
    using value_type = void;
    using difference_type = ptrdiff_t;
    using reference = void;
    using pointer = void;
    using iterator_category = output_iterator_tag;
};

} // namespace std

#endif