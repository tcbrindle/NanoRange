
#pragma once

// constexpr std::array implementation for testing

#include <nanorange/algorithm/equal.hpp>
#include <nanorange/algorithm/fill.hpp>

template <typename T, std::size_t N>
struct carray {
    using value_type = T;
    using size_type = std::size_t;
    using difference_type = std::ptrdiff_t;
    using reference = value_type&;
    using const_reference = const value_type&;
    using pointer = value_type*;
    using const_pointer = const value_type*;
    using iterator = pointer;
    using const_iterator = const_pointer;

    constexpr reference operator[](size_type idx) { return arr_[idx]; }
    constexpr const_reference operator[](size_type idx) const { return arr_[idx]; }

    constexpr reference front() { return arr_[0]; }
    constexpr const_reference front() const { return arr_[0]; }
    constexpr reference back() { return arr_[N - 1]; }
    constexpr const_reference back() const { return arr_[N - 1]; }

    constexpr pointer data() { return arr_; }
    constexpr const_pointer data() const { return arr_; }

    constexpr iterator begin() { return data(); }
    constexpr const_iterator begin() const { return data(); }
    constexpr const_iterator cbegin() const { return begin(); }

    constexpr iterator end() { return data() + N; }
    constexpr const_iterator end() const { return data() + N; }
    constexpr const_iterator cend() const { return end(); }

    static constexpr bool empty() { return N == 0; }
    static constexpr size_type size() { return N; }

    constexpr void fill(const T& value)
    {
        nano::fill(*this, value);
    }

    constexpr void swap(carray& other)
    {
        nano::swap(arr_, other.arr_);
    }

    friend constexpr bool operator==(const carray& lhs, const carray& rhs)
    {
        return nano::equal(lhs, rhs);
    }

    friend constexpr bool operator!=(const carray& lhs, const carray& rhs)
    {
        return !(lhs == rhs);
    }

    T arr_[N];
};