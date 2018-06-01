
#pragma once

#include <nanorange/view/subrange.hpp>

#include "catch.hpp"
#include "test_iterators.hpp"

template <typename T>
T& as_lvalue(T&& t)
{
    return t;
}

template <typename Rng, typename Rng2>
void check_equal_(Rng && actual, Rng2&& expected)
{
    auto begin0 = nano::begin(actual);
    auto end0 = nano::end(actual);
    auto begin1 = nano::begin(expected);
    auto end1 = nano::end(expected);
    for(; begin0 != end0 && begin1 != end1; ++begin0, ++begin1)
        CHECK(*begin0 == *begin1);
    CHECK(begin0 == end0);
    CHECK(begin1 == end1);
}

template <typename Val, typename Rng>
void check_equal(Rng && actual, std::initializer_list<Val> expected)
{
    check_equal_(actual, expected);
}

template <typename Rng, typename Rng2>
void check_equal(Rng && actual, Rng2&& expected)
{
    check_equal_(actual, expected);
}

namespace nano {
inline namespace ranges {
inline namespace ext {

template <typename I, typename S>
struct iterator_range {
    I first;
    S last;

    I begin() const { return first; }
    S end() const { return last; }
};

template <typename I, typename S>
iterator_range<I, S> make_range(I i, S s)
{
    return {std::move(i), std::move(s)};
}

}
}
}

template <typename T>
struct checker
{
private:
    std::function<void(std::function<void(T)>)> algo_;
public:
    explicit checker(std::function<void(std::function<void(T)>)> algo)
            : algo_(std::move(algo)) {}

    void check(std::function<void(T)> const & check) const {
        algo_(check);
    }
};

template <bool B, typename T>
typename std::conditional<B, T, T const &>::type rvalue_if(T const &t) {
    return t;
}

template <typename Algo, bool RvalueOK = false>
struct test_range_algo_1
{
private:
    Algo algo_;
public:
    explicit test_range_algo_1(Algo algo)
            : algo_(algo)
    {}
    template <typename I, typename... Rest>
    auto operator()(I begin, I end, Rest &&... rest) const ->
    checker<decltype(algo_(begin, end, rest...))>
    {
        using R = decltype(algo_(begin, end, rest...));
        return checker<R>{[=](std::function<void(R)> const & check)
                          {
                              using S = typename sentinel_type<I>::type;
                              check(algo_(begin, end, rest...));
                              check(algo_(begin, S{base(end)}, rest...));
                              check(algo_(::rvalue_if<RvalueOK>(nano::make_subrange(begin, end)), rest...));
                              check(algo_(::rvalue_if<RvalueOK>(nano::make_subrange(begin, S{base(end)})), rest...));
                          }};
    }
};

template <bool RvalueOK = false, typename Algo>
test_range_algo_1<Algo, RvalueOK> make_testable_1(Algo algo)
{
    return test_range_algo_1<Algo, RvalueOK>{algo};
}

template <typename Algo, bool RvalueOK1 = false, bool RvalueOK2 = false>
struct test_range_algo_2
{
private:
    Algo algo_;
public:
    explicit test_range_algo_2(Algo algo)
            : algo_(algo)
    {}
    template <typename I1, typename I2, typename... Rest>
    auto operator()(I1 begin1, I1 end1, I2 begin2, I2 end2, Rest &&... rest) const ->
    checker<decltype(algo_(begin1, end1, begin2, end2, rest...))>
    {
        using R = decltype(algo_(begin1, end1, begin2, end2, rest...));
        return checker<R>{[=](std::function<void(R)> const & check)
                          {
                              using S1 = typename sentinel_type<I1>::type;
                              using S2 = typename sentinel_type<I2>::type;
                              check(algo_(begin1, end1, begin2, end2, rest...));
                              check(algo_(begin1, S1{base(end1)}, begin2, S2{base(end2)}, rest...));
                              check(algo_(::rvalue_if<RvalueOK1>(nano::make_subrange(begin1, end1)),
                                          ::rvalue_if<RvalueOK2>(nano::make_subrange(begin2, end2)),
                                          rest...));
                              check(algo_(::rvalue_if<RvalueOK1>(nano::make_subrange(begin1, S1{base(end1)})),
                                          ::rvalue_if<RvalueOK2>(nano::make_subrange(begin2, S2{base(end2)})),
                                          rest...));
                          }};
    }
};

template <bool RvalueOK1 = false, bool RvalueOK2 = false, typename Algo>
test_range_algo_2<Algo, RvalueOK1, RvalueOK2> make_testable_2(Algo algo)
{
    return test_range_algo_2<Algo, RvalueOK1, RvalueOK2>{algo};
}