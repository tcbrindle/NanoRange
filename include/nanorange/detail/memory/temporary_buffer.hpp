
// nanorange/algorithm/move.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_DETAIL_MEMORY_TEMPORARY_BUFFER_HPP_INCLUDED
#define NANORANGE_DETAIL_MEMORY_TEMPORARY_BUFFER_HPP_INCLUDED

#include <nanorange/detail/macros.hpp>

#include <cassert>
#include <memory>
#include <vector>

NANO_BEGIN_NAMESPACE

namespace detail {

template <typename T>
struct temporary_buffer {
private:
    struct deleter {
        void operator()(T* ptr) const { ::operator delete[](ptr); }
    };

public:
    temporary_buffer() = default;

    explicit temporary_buffer(std::ptrdiff_t desired_size)
            : data_(static_cast<T*>(::operator new[](desired_size * sizeof(T), std::nothrow))),
              max_(data_ ? pos_ + desired_size : nullptr)
    {}

    std::size_t available() const { return max_ - pos_; }

    std::size_t max_size() const { return max_ - data_.get(); }

    T* allocate(std::size_t n)
    {
        assert(n <= available());
        if (n > available()) {
            return nullptr; //???
        }

        T* p = pos_;
        pos_ += n;
        return p;
    }

    void deallocate(T* p, std::size_t n)
    {
        if (pos_ == p + n) { // this was the most recent allocation
            pos_ = p;
        }
    }

    explicit operator bool() const { return bool(data_); }

private:
    std::unique_ptr<T, deleter> data_{nullptr};
    T* pos_ = data_.get();
    T* max_ = nullptr;
};

template <typename T>
struct buffer_allocator {
    using value_type = T;

    explicit buffer_allocator(temporary_buffer<T>& buf) : buf_(buf) {}

    T* allocate(std::size_t n) { return buf_.allocate(n); }

    void deallocate(T* ptr, std::size_t n) noexcept
    {
        return buf_.deallocate(ptr, n);
    }

    std::size_t max_size() const { return buf_.max_size(); }

    friend bool operator==(const buffer_allocator& lhs, const buffer_allocator& rhs)
    {
        return std::addressof(lhs.buf_) == std::addressof(rhs.buf_);
    }

    friend bool operator!=(const buffer_allocator& lhs, const buffer_allocator& rhs)
    {
        return !(lhs == rhs);
    }

private:
    temporary_buffer<T>& buf_;
};

template <typename T>
auto make_temporary_vector(temporary_buffer<T>& buf)
{
    return std::vector<T, buffer_allocator<T>>(buffer_allocator<T>(buf));
}

} // namespace detail

NANO_END_NAMESPACE

#endif
