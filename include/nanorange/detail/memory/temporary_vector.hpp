
// nanorange/detail/memory/temporary_vector.hpp
//
// Copyright (c) 2019 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_DETAIL_MEMORY_TEMPORARY_VECTOR_HPP_INCLUDED
#define NANORANGE_DETAIL_MEMORY_TEMPORARY_VECTOR_HPP_INCLUDED

#include <nanorange/memory/destroy.hpp>

#include <cassert>
#include <memory>
#include <new>

NANO_BEGIN_NAMESPACE

namespace detail {

template <typename T>
struct temporary_vector {
private:
    struct deleter {
        void operator()(T* ptr) const { ::operator delete[](ptr); }
    };

public:
    temporary_vector() = default;

    explicit temporary_vector(std::size_t capacity)
        : start_(static_cast<T*>(::operator new[](capacity * sizeof(T), std::nothrow))),
          end_cap_(start_ ? start_.get() + capacity : nullptr)
    {}

    temporary_vector(temporary_vector&& other) noexcept
        : start_(std::move(other.start_)),
          end_(other.end_),
          end_cap_(other.end_cap_)
    {
        other.end_ = nullptr;
        other.end_cap_ = nullptr;
    }

    temporary_vector& operator=(temporary_vector&& other) noexcept
    {
        nano::swap(start_, other.start_);
        nano::swap(end_, other.end_);
        nano::swap(end_cap_, other.end_cap_);
        return *this;
    }

    ~temporary_vector()
    {
        nano::destroy(begin(), end());
    }

    std::size_t size() const { return end_ - start_.get(); }
    std::size_t capacity() const { return end_cap_ - start_.get(); }
    [[nodiscard]] bool empty() const { return size() == 0; }

    void push_back(const T& elem) { emplace_back(elem); }
    void push_back(T&& elem) { emplace_back(std::move(elem)); }

    template <typename... Args>
    void emplace_back(Args&&... args)
    {
        assert(end_ < end_cap_);
        ::new (end_) T(std::forward<Args>(args)...);
        ++end_;
    }

    T* begin() { return start_.get(); }
    const T* begin() const { return start_.get(); }
    T* end() { return end_; }
    const T* end() const { return end_; }

    void clear()
    {
        nano::destroy(begin(), end());
        end_ = start_.get();
    }

private:
    std::unique_ptr<T, deleter> start_{nullptr};
    T* end_ = start_.get();
    T* end_cap_ = nullptr;
};

} // namespace detail

NANO_END_NAMESPACE

#endif
