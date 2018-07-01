// cmcstl2 - A concept-enabled C++ standard library
//
//  Copyright Casey Carter 2016
//  Copyright Christopher Di Bella 2016
//
//  Use, modification and distribution is subject to the
//  Boost Software License, Version 1.0. (See accompanying
//  file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt)
//
// Project home: https://github.com/caseycarter/cmcstl2
//
#ifndef COMMON_HPP
#define COMMON_HPP

#include <array>
#include <cstdint>
#include <string>

template <typename T>
class raw_buffer {
public:
	using value_type = T;

	~raw_buffer() {
		if (data_) {
			std::allocator<T>{}.deallocate(data_, size_);
		}
	}

	explicit raw_buffer(const std::ptrdiff_t size)
		: data_{std::allocator<T>{}.allocate(size)}
		, size_{size}
	{}

	raw_buffer(raw_buffer&& that) noexcept
	: data_{that.data_}
	, size_{that.size_}
	{
		that.data_ = nullptr;
		that.size_ = 0;
	}

	raw_buffer& operator=(raw_buffer&& that) & noexcept {
		nano::swap(data_, that.data_);
		nano::swap(size_, that.size_);
		return *this;
	}

	T* begin() noexcept {
		return data_;
	}

	const T* begin() const noexcept {
		return data_;
	}

	auto cbegin() const noexcept {
		return begin();
	}

	T* end() noexcept {
		return data_ + size_;
	}

	const T* end() const noexcept {
		return data_ + size_;
	}

	auto cend() const noexcept {
		return end();
	}

	T* data() noexcept {
		return data_;
	}
	const T* data() const noexcept {
		return data_;
	}

	std::ptrdiff_t size() const noexcept {
		return size_;
	}
private:
	T* data_;
	std::ptrdiff_t size_;
};

template <typename T>
auto make_buffer(const std::size_t size) {
	return raw_buffer<T>{static_cast<std::ptrdiff_t>(size)};
}

class Book {
public:
	bool operator==(const Book& b) const noexcept
	{
		return
			isbn_ == b.isbn_ &&
			price_ == b.price_ &&
			title_ == b.title_ &&
			author_ == b.author_;
	}

	bool operator!=(const Book& b) const noexcept
	{
		return !(*this == b);
	}

	bool empty() const noexcept
	{
		return title_.empty() && author_.empty();
	}
private:
	std::int64_t isbn_{1248163264128-256};
	double price_{16.64};
	std::string title_{"The Lord of the Rings: The Ring Goes South"};
	std::string author_{"J.R.R Tolkien"};
};

template <typename T>
using Array = std::array<T, 8>;

#endif // COMMON_HPP
