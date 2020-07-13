
#include "../catch.hpp"

#include <nanorange/algorithm.hpp>
#include <nanorange/iterator/back_insert_iterator.hpp>
#include <nanorange/iterator/ostream_iterator.hpp>

#include <array>
#include <complex>
#include <functional>
#include <iostream>
#include <list>
#include <random>
#include <vector>

namespace rng = nano;

TEST_CASE("alg.basic.copy")
{
    const std::vector<int> src{1, 2, 3, 4, 5, 6, 7, 8, 9};
    std::vector<int> dest(src.size());

    SECTION("with iterators")
    {
        auto res = rng::copy(src.begin(), src.end(), dest.begin());
        REQUIRE(res.in == src.end());
        REQUIRE(res.out == dest.end());
        REQUIRE(src == dest);
    }

    SECTION("with range")
    {
        const auto res = rng::copy(src, dest.begin());
        REQUIRE(res.in == src.end());
        REQUIRE(res.out == dest.end());
        REQUIRE(src == dest);
    }
}

TEST_CASE("alg.basic.copy_if")
{
    const std::vector<int> src{1, 2, 2, 2, 2, 2, 3, 4, 5, 6};
    std::vector<int> dest;
    const auto pred = [] (int i) { return i == 2; };

    SECTION("with iterators") {
        rng::copy_if(src.begin(), src.end(), rng::back_inserter(dest), pred);
        REQUIRE(dest == (std::vector<int>{2, 2, 2, 2, 2}));
    }

    SECTION("with range") {
        rng::copy_if(src, rng::back_inserter(dest), pred);
        REQUIRE(dest == (std::vector<int>{2, 2, 2, 2, 2,}));
    }
}

TEST_CASE("alg.basic.copy_n")
{
    const std::vector<int> src{1, 2, 3, 4, 5, 6, 7, 8, 9};
    std::vector<int> dest;

    rng::copy_n(src.begin(), 5, rng::back_inserter(dest));
    REQUIRE(dest == (std::vector<int>{1, 2, 3, 4, 5}));
}

TEST_CASE("alg.basic.copy_backward")
{
    // Let's use a list for a change
    const std::list<int> src{1, 2, 3, 4, 5};
    std::vector<int> dest(src.size());

    SECTION("with iterators")
    {
        const auto res = rng::copy_backward(src.begin(), src.end(), dest.end());
        REQUIRE(res.out == dest.begin());
        REQUIRE(rng::equal(src, dest));
    }

    SECTION("with ranges")
    {
        const auto res = rng::copy_backward(src, dest.end());
        REQUIRE(res.out == dest.begin());
        REQUIRE(rng::equal(src, dest));
    }
}

namespace {

struct mark_move {
    mark_move() = default;

    mark_move(mark_move &&other) noexcept
        : moved_from(other.moved_from),
          moved_into(true) {
        other.moved_from = true;
    }

    mark_move &operator=(mark_move &&other) noexcept {
        moved_from = other.moved_from;
        moved_into = true;
        other.moved_from = true;
        return *this;
    }

    bool moved_from = false;
    bool moved_into = false;
};

}

TEST_CASE("alg.basic.move")
{
    std::vector<mark_move> src(5);
    std::vector<mark_move> dest;

    SECTION("with iterators") {
        rng::move(src.begin(), src.end(), rng::back_inserter(dest));
        REQUIRE(dest.size() == src.size());
        REQUIRE(rng::all_of(src, [](const auto& m) { return m.moved_from == true; }));
        REQUIRE(rng::all_of(dest, [] (const auto& m) { return m.moved_into == true; }));
    }

    SECTION("with range") {
        dest.resize(src.size());
        rng::move(src, dest.begin());
        REQUIRE(dest.size() == src.size());
        REQUIRE(rng::all_of(src, [](const auto& m) { return m.moved_from == true; }));
        REQUIRE(rng::all_of(dest, [] (const auto& m) { return m.moved_into == true; }));
    }
}

TEST_CASE("alg.basic.move_backward")
{
    std::vector<mark_move> src(5);
    std::vector<mark_move> dest(5);

    SECTION("with iterators") {
        const auto it = rng::move_backward(src.begin(), src.end(), dest.end());
        REQUIRE(it.out == dest.begin());
        REQUIRE(rng::all_of(src, [](const auto& m) { return m.moved_from == true; }));
        REQUIRE(rng::all_of(dest, [] (const auto& m) { return m.moved_into == true; }));
    }

    SECTION("with range") {
        const auto it = rng::move_backward(src, dest.end());
        REQUIRE(it.out == dest.begin());
        REQUIRE(rng::all_of(src, [](const auto& m) { return m.moved_from == true; }));
        REQUIRE(rng::all_of(dest, [] (const auto& m) { return m.moved_into == true; }));
    }
}

TEST_CASE("alg.basic.fill")
{
    std::vector<int> vec(5);

    SECTION("with iterators") {
        rng::fill(vec.begin(), vec.end(), 2);
        REQUIRE(rng::all_of(vec, [](int i) { return i == 2; }));
    }

    SECTION("with range") {
        rng::fill(vec, 2);
        REQUIRE(rng::all_of(vec, [](int i) { return i == 2; }));
    }
}

TEST_CASE("alg.basic.fill_n")
{
    std::vector<int> vec(5);
    rng::fill_n(vec.begin(), 3, 2);
    REQUIRE(vec == (std::vector<int>{2, 2, 2, 0, 0}));
}

TEST_CASE("alg.basic.transform (unary)")
{
    constexpr std::array<int, 5> src{{1, 2, 3, 4, 5}};
    std::vector<int> dest(src.size());
    const auto square = [] (int i) { return i * i; };

    SECTION("with iterators") {
        const auto it = rng::transform(src.begin(), src.end(), dest.begin(), square);
        REQUIRE(it.in == src.end());
        REQUIRE(it.out == dest.end());
        REQUIRE(dest == (std::vector<int>{1, 4, 9, 16, 25}));
    }

    SECTION("with range") {
        const auto it = rng::transform(src, dest.begin(), square);
        REQUIRE(it.in == src.end());
        REQUIRE(it.out == dest.end());
        REQUIRE(dest == (std::vector<int>{1, 4, 9, 16, 25}));
    }
}

TEST_CASE("alg.basic.transform (binary)")
{
    // Let's do something interesting
    using namespace std::complex_literals;
    const std::vector<float> real{1.0, 0.0, -1.0};
    const std::vector<float> imag{-1.0, 0.0, 1.0};
    std::vector<std::complex<double>> cmplx(3);

    const auto make_complex = [] (auto r, auto i) { return std::complex<float>{r, i}; };

    SECTION("with iterators") {
        const auto res = rng::transform(real.begin(), real.end(),imag.begin(),
                                       cmplx.begin(), make_complex);
        REQUIRE(res.in1 == real.end());
        REQUIRE(res.in2 == imag.end());
        REQUIRE(res.out == cmplx.end());
        REQUIRE(cmplx[0] == std::complex<double>(1.0, -1.0));
        REQUIRE(cmplx[1] == std::complex<double>(0.0 , 0.0));
        REQUIRE(cmplx[2] == std::complex<double>(-1.0, 1.0));
    }

    SECTION("with ranges") {
        const auto res = rng::transform(real, imag, cmplx.begin(), make_complex);
        REQUIRE(res.in1 == real.end());
        REQUIRE(res.in2 == imag.end());
        REQUIRE(res.out == cmplx.end());
        REQUIRE(cmplx[0] == std::complex<double>(1.0, -1.0));
        REQUIRE(cmplx[1] == std::complex<double>(0.0 , 0.0));
        REQUIRE(cmplx[2] == std::complex<double>(-1.0, 1.0));
    }
}

TEST_CASE("alg.basic.generate")
{
    std::vector<int> vec(5);
    int count = 0;
    const auto generator = [&count] { ++count; return count * count; };

    SECTION("with iterators") {
        rng::generate(vec.begin(), vec.end(), generator);
        REQUIRE(vec == (std::vector<int>{1, 4, 9, 16, 25}));
    }

    SECTION("with range") {
        rng::generate(vec, generator);
        REQUIRE(vec == (std::vector<int>{1, 4, 9, 16, 25}));
    }
}

TEST_CASE("alg.basic.generate_n")
{
    std::vector<int> vec(5);
    int count = 0;
    const auto generator = [&count] { ++count; return count * count; };

    rng::generate_n(vec.begin() + 1, 4, generator);
    REQUIRE(vec == (std::vector<int>{0, 1, 4, 9, 16}));
}

TEST_CASE("alg.basic.remove")
{
    std::vector<int> vec{1, 2, 3, 4, 5};

    SECTION("with iterators") {
        const auto it = rng::remove(vec.begin(), vec.end(), 2);
        REQUIRE(it == vec.end() - 1);
        constexpr int arr[] = {1, 3, 4, 5};
        REQUIRE(std::equal(vec.begin(), it, std::begin(arr), std::end(arr)));
    }

    SECTION("with range") {
        const auto it = rng::remove(vec, 2);
        REQUIRE(it == vec.end() - 1);
        constexpr int arr[] = {1, 3, 4, 5};
        REQUIRE(std::equal(vec.begin(), it, std::begin(arr), std::end(arr)));
    }
}

constexpr bool less_than_four(int i) { return i < 4; }

TEST_CASE("alg.basic.remove_if")
{
    std::vector<int> vec{1, 2, 3, 4, 5};

    SECTION("with iterators")
    {
        const auto it = rng::remove_if(vec.begin(), vec.end(), less_than_four);
        REQUIRE(it == vec.begin() + 2);
        REQUIRE(vec[0] == 4);
        REQUIRE(vec[1] == 5);
    }

    SECTION("with range")
    {
        const auto it = rng::remove_if(vec, less_than_four);
        REQUIRE(it == vec.begin() + 2);
        REQUIRE(vec[0] == 4);
        REQUIRE(vec[1] == 5);
    }
}

TEST_CASE("alg.basic.remove_copy")
{
    // Something a bit different this time
    const std::string src = "Hello World";
    std::ostringstream ss;

    SECTION("with iterators")
    {
        rng::remove_copy(std::begin(src), std::end(src),
                         rng::ostream_iterator<char>(ss), 'l');
        REQUIRE(ss.str() == "Heo Word");
    }

    SECTION("with ranges")
    {
        rng::remove_copy(src, rng::ostream_iterator<char>(ss), 'l');
        REQUIRE(ss.str() == "Heo Word");
    }
}

TEST_CASE("alg.basic.remove_copy_if")
{
    const std::string src = "Hello World";
    std::ostringstream ss;

    SECTION("with iterators")
    {
        rng::remove_copy_if(std::begin(src), std::end(src),
                            rng::ostream_iterator<char>(ss), ::isspace);
        REQUIRE(ss.str() == "HelloWorld");
    }

    SECTION("with ranges")
    {
        rng::remove_copy_if(src, rng::ostream_iterator<char>(ss), ::isspace);
        REQUIRE(ss.str() == "HelloWorld");
    }
}

TEST_CASE("alg.basic.replace")
{
    std::vector<int> vec{1, 2, 3};

    SECTION("with iterators")
    {
        rng::replace(vec.begin(), vec.end(), 2, 99);
        REQUIRE(vec[0] == 1);
        REQUIRE(vec[1] == 99);
        REQUIRE(vec[2] == 3);
    }

    SECTION("with iterators")
    {
        rng::replace(vec, 2, 99);
        REQUIRE(vec[0] == 1);
        REQUIRE(vec[1] == 99);
        REQUIRE(vec[2] == 3);
    }
}

TEST_CASE("alg.basic.replace_if")
{
    std::vector<int> vec{-2, -1, 0, 1, 2};
    const auto is_negative = [] (int i) { return i < 0; };

    SECTION("with iterators")
    {
        rng::replace_if(vec.begin(), vec.end(), is_negative, 99);
        REQUIRE(vec == (std::vector<int>{99, 99, 0, 1, 2}));
    }

    SECTION("with range")
    {
        rng::replace_if(vec, is_negative, 99);
        REQUIRE(vec == (std::vector<int>{99, 99, 0, 1, 2}));
    }
}

namespace {
namespace inner {

template<typename I>
struct iterator_range {
    I first{}, last{};
};

template<typename I>
I begin(iterator_range<I> rng) { return rng.first; }

template<typename I>
I end(iterator_range<I> rng) { return rng.last; }

}

template<typename I>
auto make_range(I first, I last) {
    return inner::iterator_range<I>{std::move(first), std::move(last)};
}

}

TEST_CASE("alg.basic.replace_copy")
{
    std::istringstream iss{"1 2 3 4 5"};
    std::ostringstream oss;

    SECTION("with iterators")
    {
        rng::replace_copy(std::istream_iterator<int>{iss}, std::istream_iterator<int>{},
                          rng::ostream_iterator<int>{oss}, 2, 99);
        REQUIRE(oss.str() == "199345");
    }

    SECTION("with range")
    {
        rng::replace_copy(make_range(std::istream_iterator<int>{iss}, std::istream_iterator<int>{}),
                          rng::ostream_iterator<int>{oss}, 2, 99);
        REQUIRE(oss.str() == "199345");
    }
}

TEST_CASE("alg.basic.replace_copy_if")
{
    std::istringstream iss{"-2 -1 0 1 2"};
    std::ostringstream oss;
    const auto is_negative = [](int i) { return i < 0; };

    SECTION("with iterators")
    {
        rng::replace_copy_if(std::istream_iterator<int>{iss}, std::istream_iterator<int>{},
                             rng::ostream_iterator<int>{oss}, is_negative, 0);
        REQUIRE(oss.str() == "00012");
    }

    SECTION("with range")
    {
        rng::replace_copy_if(make_range(std::istream_iterator<int>{iss}, std::istream_iterator<int>{}),
                          rng::ostream_iterator<int>{oss}, is_negative, 0);
        REQUIRE(oss.str() == "00012");
    }
}

TEST_CASE("alg.basic.swap_ranges")
{
    std::vector<int> vec{1, 2, 3};
    std::array<int, 3> arr{{4, 5, 6}};

    SECTION("with iterators, 3-legged")
    {
        const auto p = rng::swap_ranges(vec.begin(), vec.end(), arr.begin());
        REQUIRE(p.in1 == vec.end());
        REQUIRE(p.in2 == arr.end());
        REQUIRE(vec == (std::vector<int>{4, 5, 6}));
        REQUIRE(arr == (std::array<int, 3>{{1, 2, 3}}));
    }

    SECTION("with iterators, 4-legged")
    {
        const auto p = rng::swap_ranges(vec.begin(), vec.end(), arr.begin(), arr.end());
        REQUIRE(p.in1 == vec.end());
        REQUIRE(p.in2 == arr.end());
        REQUIRE(vec == (std::vector<int>{4, 5, 6}));
        REQUIRE(arr == (std::array<int, 3>{{1, 2, 3}}));
    }

    SECTION("with ranges")
    {
        const auto p = rng::swap_ranges(vec, arr);
        REQUIRE(p.in1 == vec.end());
        REQUIRE(p.in2 == arr.end());
        REQUIRE(vec == (std::vector<int>{4, 5, 6}));
        REQUIRE(arr == (std::array<int, 3>{{1, 2, 3}}));
    }
}

TEST_CASE("alg.basic.reverse")
{
    std::list<int> list{1, 2, 3, 4, 5};

    SECTION("with iterators") {
        const auto it = rng::reverse(list.begin(), list.end());
        REQUIRE(list == (std::list<int>{5, 4, 3, 2, 1}));
        REQUIRE(it == list.end());
    }

    SECTION("with range") {
        const auto it = rng::reverse(list);
        REQUIRE(list == (std::list<int>{5, 4, 3, 2, 1}));
        REQUIRE(it == list.end());
    }
}

TEST_CASE("alg.basic.reverse_copy")
{
    const std::list<int> list{1, 2, 3, 4, 5};
    std::ostringstream oss;

    SECTION("with iterators") {
        rng::reverse_copy(list.begin(), list.end(),
                          rng::ostream_iterator<int>{oss});
        REQUIRE(oss.str() == "54321");
    }

    SECTION("with range") {
        rng::reverse_copy(list, rng::ostream_iterator<int>{oss});
        REQUIRE(oss.str() == "54321");
    }
}

TEST_CASE("alg.basic.rotate")
{
    std::vector<int> vec{1, 2, 3, 4, 5};

    SECTION("with iterators") {
        const std::vector<int> result{2, 3, 4, 5, 1};
        (void)rng::rotate(vec.begin(), vec.begin() + 1, vec.end());
        REQUIRE(vec == result);
    }

    SECTION("with range") {
        const std::vector<int> result{2, 3, 4, 5, 1};
        (void)rng::rotate(vec, vec.begin() + 1);
        REQUIRE(vec == result);
    }
}

TEST_CASE("alg.basic.rotate_copy")
{
    const std::vector<int> src{23451};
    std::ostringstream dest;

    SECTION("with iterators") {
        rng::rotate_copy(src.begin(), src.begin() + 1, src.end(),
                         rng::ostream_iterator<int>(dest));
        REQUIRE(dest.str() == "23451");
    }

    SECTION("with range") {
        rng::rotate_copy(src, src.begin() + 1, rng::ostream_iterator<int>(dest));
        REQUIRE(dest.str() == "23451");
    }
}

TEST_CASE("alg.basic.sample")
{
    const std::vector<int> orig{1, 2, 3, 4, 5};
    std::vector<int> vec(orig);
    std::mt19937 rng{std::random_device{}()};
    std::vector<int> out;

    SECTION("with iterators") {
	out.clear();
        rng::sample(vec.begin(), vec.end(), nano::back_inserter(out), 2, rng);
        REQUIRE(out.size() == 2);
    }

    SECTION("with range") {
	out.clear();
        rng::sample(vec, nano::back_inserter(out), 2, rng);
        REQUIRE(out.size() == 2);
    }
}

TEST_CASE("alg.basic.shuffle")
{
    const std::vector<int> orig{1, 2, 3, 4, 5};
    std::vector<int> vec(orig);
    std::mt19937 rng{std::random_device{}()};

    SECTION("with iterators") {
        rng::shuffle(vec.begin(), vec.end(), rng);
        REQUIRE(std::is_permutation(vec.begin(), vec.end(), orig.begin(), orig.end()));
    }

    SECTION("with range") {
        rng::shuffle(vec, rng);
        REQUIRE(std::is_permutation(vec.begin(), vec.end(), orig.begin(), orig.end()));
    }
}

TEST_CASE("alg.basic.unique")
{
    std::vector<int> vec{1, 2, 2, 3, 3, 3, 4, 4};

    SECTION("with iterators") {
        const auto sub = rng::unique(vec.begin(), vec.end());
        REQUIRE(sub.end() == vec.end());
        REQUIRE(sub.begin() == vec.begin() + 4);
        REQUIRE(vec[0] == 1);
        REQUIRE(vec[1] == 2);
        REQUIRE(vec[2] == 3);
        REQUIRE(vec[3] == 4);
    }

    SECTION("with range") {
        const auto sub = rng::unique(vec);
        REQUIRE(sub.end() == vec.end());
        REQUIRE(sub.begin() == vec.begin() + 4);
        REQUIRE(vec[0] == 1);
        REQUIRE(vec[1] == 2);
        REQUIRE(vec[2] == 3);
        REQUIRE(vec[3] == 4);
    }
}

TEST_CASE("alg.basic.unique (with predicate)")
{
    std::vector<int> vec{1, 2, 2, 3, 3, 3, 4, 4};

    SECTION("with iterators") {
        const auto sub = rng::unique(vec.begin(), vec.end(), std::equal_to<>{});
        REQUIRE(sub.end() == vec.end());
        REQUIRE(sub.begin() == vec.begin() + 4);
        REQUIRE(vec[0] == 1);
        REQUIRE(vec[1] == 2);
        REQUIRE(vec[2] == 3);
        REQUIRE(vec[3] == 4);
    }

    SECTION("with range") {
        const auto sub = rng::unique(vec, std::equal_to<>{});
        REQUIRE(sub.end() == vec.end());
        REQUIRE(sub.begin() == vec.begin() + 4);
        REQUIRE(vec[0] == 1);
        REQUIRE(vec[1] == 2);
        REQUIRE(vec[2] == 3);
        REQUIRE(vec[3] == 4);
    }
}

TEST_CASE("alg.basic.unique_copy")
{
    std::istringstream src{"1 1 1 1 2 2 2 2 3 3 3 3 4 4 4 4 5 5 5 5"};
    std::ostringstream dest;

    SECTION("with iterators") {
        rng::unique_copy(std::istream_iterator<int>{src}, std::istream_iterator<int>{},
                         rng::ostream_iterator<int>{dest, " "});
        REQUIRE(dest.str() == "1 2 3 4 5 ");
    }

    SECTION("with range") {
        rng::unique_copy(make_range(std::istream_iterator<int>{src}, std::istream_iterator<int>{}),
                         rng::ostream_iterator<int>{dest, " "});
        REQUIRE(dest.str() == "1 2 3 4 5 ");
    }
}

TEST_CASE("alg.basic.unique_copy (with predicate)")
{
    std::istringstream src{"1 1 1 1 2 2 2 2 3 3 3 3 4 4 4 4 5 5 5 5"};
    std::ostringstream dest;

    SECTION("with iterators") {
        rng::unique_copy(std::istream_iterator<int>{src}, std::istream_iterator<int>{},
                         rng::ostream_iterator<int>{dest, " "}, std::equal_to<>{});
        REQUIRE(dest.str() == "1 2 3 4 5 ");
    }

    SECTION("with range") {
        rng::unique_copy(make_range(std::istream_iterator<int>{src}, std::istream_iterator<int>{}),
                         rng::ostream_iterator<int>{dest, " "}, std::equal_to<>{});
        REQUIRE(dest.str() == "1 2 3 4 5 ");
    }
}
