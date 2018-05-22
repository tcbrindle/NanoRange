// cmcstl2 - A concept-enabled C++ standard library
//
//  Copyright Casey Carter 2017
//
//  Use, modification and distribution is subject to the
//  Boost Software License, Version 1.0. (See accompanying
//  file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt)
//
// Project home: https://github.com/caseycarter/cmcstl2
//
#include <stl2/detail/iterator/operations.hpp>
#include <stl2/view/iota.hpp>
#include <stl2/view/take_exactly.hpp>
#include <stl2/detail/iterator/basic_iterator.hpp>

namespace ranges = std::experimental::ranges;

namespace {
    template <class T, std::size_t N, bool Bidi = true>
    struct unsized_range {
        T array_[N];

        struct cursor {
            const T* ptr_;

            constexpr const T& read() const noexcept { return *ptr_; }
            constexpr void next() noexcept { ++ptr_; }
            constexpr void prev() noexcept requires Bidi { --ptr_; }
            constexpr bool equal(cursor that) const noexcept { return ptr_ == that.ptr_; }
        };

        using iterator = ranges::basic_iterator<cursor>;
        constexpr iterator begin() const noexcept { return iterator{cursor{array_ + 0}}; }
        constexpr iterator end() const noexcept { return iterator{cursor{array_ + N}}; }
    };

    constexpr bool test_advance() {
        {
            constexpr int rng[] = {0,1,2,3};
            auto pos = ranges::begin(rng);
            // advance(i, n)
            ranges::advance(pos, 1);
            if (pos != rng + 1) return false;
            ranges::advance(pos, -1);
            if (pos != rng + 0) return false;
            ranges::advance(pos, 3);
            if (pos != rng + 3) return false;
            // advance(i, s)
            ranges::advance(pos, ranges::end(rng));
            if (pos != rng + 4) return false;
            // advance(i, n, s)
            pos = ranges::begin(rng);
            if (ranges::advance(pos, 2, ranges::end(rng)) != 0) return false;
            if (pos != rng + 2) return false;
            if (ranges::advance(pos, 42, ranges::end(rng)) != 40) return false;
            if (pos != rng + 4) return false;
            // advance(i, n, i)
            if (ranges::advance(pos, -2, ranges::begin(rng)) != 0) return false;
            if (pos != rng + 2) return false;
            if (ranges::advance(pos, -42, ranges::begin(rng)) != -40) return false;
            if (pos != rng + 0) return false;
        }

        {
            auto rng = ranges::ext::take_exactly_view<ranges::ext::iota_view<int>>{{}, 42};
            static_assert(ranges::models::RandomAccessRange<decltype(rng)>);
            auto pos = ranges::begin(rng);
            // advance(i, n)
            ranges::advance(pos, 1);
            if (*pos != 1) return false;
            ranges::advance(pos, -1);
            if (pos != ranges::begin(rng)) return false;
            ranges::advance(pos, 3);
            if (*pos != 3) return false;
            // advance(i, s)
            ranges::advance(pos, ranges::end(rng));
            if (pos != ranges::begin(rng) + 42) return false;
            // advance(i, n, s)
            pos = ranges::begin(rng);
            if (ranges::advance(pos, 2, ranges::end(rng)) != 0) return false;
            if (pos != ranges::begin(rng) + 2) return false;
            if (ranges::advance(pos, 42, ranges::end(rng)) != 2) return false;
            if (pos != ranges::begin(rng) + 42) return false;
            // advance(i, n, i)
            if (ranges::advance(pos, -2, ranges::begin(rng)) != 0) return false;
            if (pos != ranges::begin(rng) + 40) return false;
            if (ranges::advance(pos, -42, ranges::begin(rng)) != -2) return false;
            if (pos != ranges::begin(rng) + 0) return false;
        }

        {
            constexpr unsized_range<int, 4> rng = {0,1,2,3};
            auto pos = ranges::begin(rng);
            // advance(i, n)
            ranges::advance(pos, 1);
            if (*pos != 1) return false;
            ranges::advance(pos, -1);
            if (pos != ranges::begin(rng)) return false;
            ranges::advance(pos, 3);
            if (*pos != 3) return false;
            // advance(i, s)
            auto saved = pos;
            ranges::advance(pos, ranges::end(rng));
            if (pos != ++saved) return false;
            // advance(i, n, s)
            pos = ranges::begin(rng);
            if (ranges::advance(pos, 2, ranges::end(rng)) != 0) return false;
            if (*pos != 2) return false;
            if (ranges::advance(pos, 42, ranges::end(rng)) != 40) return false;
            if (pos != saved) return false;
            // advance(i, n, i)
            if (ranges::advance(pos, -2, ranges::begin(rng)) != 0) return false;
            if (*pos != 2) return false;
            if (ranges::advance(pos, -42, ranges::begin(rng)) != -40) return false;
            if (pos != ranges::begin(rng)) return false;
        }

        {
            constexpr unsized_range<int, 4, false> rng = {0,1,2,3};
            auto pos = ranges::begin(rng);
            // advance(i, n)
            ranges::advance(pos, 1);
            if (*pos != 1) return false;
            pos = ranges::begin(rng);
            ranges::advance(pos, 3);
            if (*pos != 3) return false;
            // advance(i, s)
            auto saved = pos;
            ranges::advance(pos, ranges::end(rng));
            if (pos != ++saved) return false;
            // advance(i, n, s)
            pos = ranges::begin(rng);
            if (ranges::advance(pos, 2, ranges::end(rng)) != 0) return false;
            if (*pos != 2) return false;
            if (ranges::advance(pos, 42, ranges::end(rng)) != 40) return false;
            if (pos != saved) return false;
        }

        return true;
    }
    static_assert(test_advance());

    constexpr bool test_next() {
        {
            constexpr int rng[] = {0,1,2,3};
            // next(i, n)
            auto pos = ranges::next(ranges::begin(rng), 1);
            if (pos != rng + 1) return false;
            pos = ranges::next(pos, -1);
            if (pos != rng + 0) return false;
            pos = ranges::next(pos, 3);
            if (pos != rng + 3) return false;
            // next(i, s)
            pos = ranges::next(pos, ranges::end(rng));
            if (pos != rng + 4) return false;
            // next(i, n, s)
            pos = ranges::next(ranges::begin(rng), 2, ranges::end(rng));
            if (pos != rng + 2) return false;
            pos = ranges::next(pos, 42, ranges::end(rng));
            if (pos != rng + 4) return false;
            // next(i, n, i)
            pos = ranges::next(pos, -2, ranges::begin(rng));
            if (pos != rng + 2) return false;
            pos = ranges::next(pos, -42, ranges::begin(rng));
            if (pos != rng + 0) return false;
        }

        {
            auto rng = ranges::ext::take_exactly_view<ranges::ext::iota_view<int>>{{}, 42};
            static_assert(ranges::models::RandomAccessRange<decltype(rng)>);
            // next(i, n)
            auto pos = ranges::next(ranges::begin(rng), 1);
            if (*pos != 1) return false;
            pos = ranges::next(pos, -1);
            if (pos != ranges::begin(rng)) return false;
            pos = ranges::next(pos, 3);
            if (*pos != 3) return false;
            // next(i, s)
            pos = ranges::next(pos, ranges::end(rng));
            if (pos != ranges::begin(rng) + 42) return false;
            // next(i, n, s)
            pos = ranges::next(ranges::begin(rng), 2, ranges::end(rng));
            if (pos != ranges::begin(rng) + 2) return false;
            pos = ranges::next(pos, 42, ranges::end(rng));
            if (pos != ranges::begin(rng) + 42) return false;
            // next(i, n, i)
            pos = ranges::next(pos, -2, ranges::begin(rng));
            if (pos != ranges::begin(rng) + 40) return false;
            pos = ranges::next(pos, -42, ranges::begin(rng));
            if (pos != ranges::begin(rng) + 0) return false;
        }

        {
            constexpr unsized_range<int, 4> rng = {0,1,2,3};
            // next(i, n)
            auto pos = ranges::next(ranges::begin(rng), 1);
            if (*pos != 1) return false;
            pos = ranges::next(pos, -1);
            if (pos != ranges::begin(rng)) return false;
            pos = ranges::next(pos, 3);
            if (*pos != 3) return false;
            // next(i, s)
            auto saved = pos;
            pos = ranges::next(pos, ranges::end(rng));
            if (pos != ++saved) return false;
            // next(i, n, s)
            pos = ranges::begin(rng);
            pos = ranges::next(pos, 2, ranges::end(rng));
            if (*pos != 2) return false;
            pos = ranges::next(pos, 42, ranges::end(rng));
            if (pos != saved) return false;
            // next(i, n, i)
            pos = ranges::next(pos, -2, ranges::begin(rng));
            if (*pos != 2) return false;
            pos = ranges::next(pos, -42, ranges::begin(rng));
            if (pos != ranges::begin(rng)) return false;
        }

        {
            constexpr unsized_range<int, 4, false> rng = {0,1,2,3};
            // next(i, n)
            auto pos = ranges::next(ranges::begin(rng), 1);
            if (*pos != 1) return false;
            pos = ranges::begin(rng);
            pos = ranges::next(pos, 3);
            if (*pos != 3) return false;
            // next(i, s)
            auto saved = pos;
            pos = ranges::next(pos, ranges::end(rng));
            if (pos != ++saved) return false;
            // next(i, n, s)
            pos = ranges::begin(rng);
            pos = ranges::next(pos, 2, ranges::end(rng));
            if (*pos != 2) return false;
            pos = ranges::next(pos, 42, ranges::end(rng));
            if (pos != saved) return false;
        }

        return true;
    }
    static_assert(test_next());

    constexpr bool test_prev() {
        {
            constexpr int rng[] = {0,1,2,3};
            // prev(i, n)
            auto pos = ranges::prev(ranges::end(rng), 1);
            if (pos != rng + 3) return false;
            pos = ranges::prev(pos, -1);
            if (pos != rng + 4) return false;
            // prev(i, n, i)
            pos = ranges::prev(pos, 2, ranges::begin(rng));
            if (pos != rng + 2) return false;
            pos = ranges::prev(pos, 42, ranges::begin(rng));
            if (pos != rng + 0) return false;
        }

        {
            auto rng = ranges::ext::take_exactly_view<ranges::ext::iota_view<int>>{{}, 42};
            static_assert(ranges::models::RandomAccessRange<decltype(rng)>);
            // prev(i, n)
            auto pos = ranges::prev(ranges::next(ranges::begin(rng), ranges::end(rng)), 1);
            if (pos != ranges::begin(rng) + 41) return false;
            pos = ranges::prev(pos, -1);
            if (pos != ranges::begin(rng) + 42) return false;
            // prev(i, n, i)
            pos = ranges::prev(pos, 2, ranges::begin(rng));
            if (pos != ranges::begin(rng) + 40) return false;
            pos = ranges::prev(pos, 42, ranges::begin(rng));
            if (pos != ranges::begin(rng) + 0) return false;
        }

        {
            constexpr unsized_range<int, 4> rng = {0,1,2,3};
            // prev(i, n)
            auto pos = ranges::prev(ranges::end(rng), 1);
            if (*pos != 3) return false;
            pos = ranges::prev(pos, -1);
            if (pos != ranges::end(rng)) return false;
            // prev(i, n, i)
            pos = ranges::prev(ranges::end(rng), 2, ranges::begin(rng));
            if (*pos != 2) return false;
            pos = ranges::prev(pos, 42, ranges::begin(rng));
            if (pos != ranges::begin(rng)) return false;
        }

        return true;
    }
    static_assert(test_prev());

    constexpr bool test_distance() {
        {
            constexpr int rng[] = {0,1,2,3};
            if (ranges::distance(rng) != 4) return false;
            if (ranges::distance(ranges::begin(rng), ranges::end(rng)) != 4) return false;
            auto bound = std::make_pair(std::ptrdiff_t{4}, ranges::end(rng));
            if (ranges::ext::enumerate(rng) != bound) return false;
            if (ranges::ext::enumerate(ranges::begin(rng), ranges::end(rng)) != bound) return false;
        }

        {
            auto rng = ranges::ext::take_exactly_view<ranges::ext::iota_view<int>>{{}, 42};
            if (ranges::distance(rng) != 42) return false;
            if (ranges::distance(ranges::begin(rng), ranges::end(rng)) != 42) return false;
            {
                auto result = ranges::ext::enumerate(rng);
                if (result.count() != 42) return false;
                if (result.end() != ranges::default_sentinel{}) return false;
            }
            {
                auto result = ranges::ext::enumerate(ranges::begin(rng), ranges::end(rng));
                if (result.count() != 42) return false;
                if (result.end() != ranges::default_sentinel{}) return false;
            }
        }

        {
            constexpr unsized_range<int, 4> rng = {0,1,2,3};
            if (ranges::distance(rng) != 4) return false;
            if (ranges::distance(ranges::begin(rng), ranges::end(rng)) != 4) return false;
            auto bound = std::make_pair(std::ptrdiff_t{4}, ranges::end(rng));
            if (ranges::ext::enumerate(rng) != bound) return false;
            if (ranges::ext::enumerate(ranges::begin(rng), ranges::end(rng)) != bound) return false;
        }

        {
            constexpr unsized_range<int, 4, false> rng = {0,1,2,3};
            if (ranges::distance(rng) != 4) return false;
            if (ranges::distance(ranges::begin(rng), ranges::end(rng)) != 4) return false;
            auto bound = std::make_pair(std::ptrdiff_t{4}, ranges::end(rng));
            if (ranges::ext::enumerate(rng) != bound) return false;
            if (ranges::ext::enumerate(ranges::begin(rng), ranges::end(rng)) != bound) return false;
        }

        return true;
    }
    static_assert(test_distance());
} // unnamed namespace

int main() {}
