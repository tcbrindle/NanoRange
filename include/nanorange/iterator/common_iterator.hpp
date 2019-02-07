// nanorange/iterator/common_iterator.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_ITERATOR_COMMON_ITERATOR_HPP_INCLUDED
#define NANORANGE_ITERATOR_COMMON_ITERATOR_HPP_INCLUDED

#include <nanorange/detail/variant.hpp>
#include <nanorange/iterator/concepts.hpp>

NANO_BEGIN_NAMESPACE

// [range.iterators.common]

namespace common_iterator_ {

struct access {
    template <typename CI>
    static constexpr decltype(auto) get_variant(CI&& ci)
    {
        return std::forward<CI>(ci).v_;
    }
};

template <typename I, typename S>
class common_iterator {
    static_assert(Iterator<I>, "");
    static_assert(Sentinel<S, I>, "");
    static_assert(!Same<I, S>, "");

    template <typename II, typename SS>
    friend class common_iterator;

    friend struct common_iterator_::access;

    class op_arrow_proxy {
        iter_value_t<I> keep_;

        constexpr op_arrow_proxy(iter_reference_t<I>&& x) : keep_(std::move(x)) {}

    public:
        constexpr const iter_value_t<I>* operator->() const
        {
            return std::addressof(keep_);
        }
    };

    template <typename II>
    using op_arrow_t = decltype(std::declval<const II&>().operator->());

    template <typename II>
    static constexpr auto do_op_arrow(const II& i, detail::priority_tag<2>)
        -> std::enable_if_t<
            std::is_pointer<II>::value || detail::exists_v<op_arrow_t, II>, I>
    {
        return i;
    }

    template <typename II>
    static constexpr auto do_op_arrow(const II& i, detail::priority_tag<1>)
        -> std::enable_if_t<std::is_reference<iter_reference_t<const II>>::value,
                            std::add_pointer_t<iter_reference_t<const II>>>
    {
        auto&& tmp = *i;
        return std::addressof(tmp);
    }

    template <typename II>
    static constexpr auto do_op_arrow(const II& i, detail::priority_tag<0>)
        -> op_arrow_proxy
    {
        return {i};
    }

    template <typename II, typename SS>
    struct copy_visitor {
        constexpr auto operator()(const II& iter) const
        {
            return detail::variant<I, S>(detail::in_place_index<0>, iter);
        }

        constexpr auto operator()(const SS& sent) const
        {
            return detail::variant<I, S>(detail::in_place_index<1>, sent);
        }
    };

    template <typename I2, typename S2>
    struct equality_visitor_base {
        // Both sentinels
        constexpr auto operator()(const S&, const S2&) const
        {
            return true;
        }

        template <typename T1, typename T2>
        constexpr auto operator()(const T1& t1, const T2& t2) const
            -> decltype(t1 == t2)
        {
            return t1 == t2;
        }
    };

    template <typename I2, typename S2, bool B = EqualityComparableWith<I, I2>>
    struct equality_visitor : equality_visitor_base<I2, S2> {};

    template <typename I2, typename S2>
    struct equality_visitor<I2, S2, false> : equality_visitor_base<I2, S2> {
        using equality_visitor_base<I2, S2>::operator();
        constexpr bool operator()(const I&, const I2&) const { return true; }
    };

    template <typename I2, typename S2>
    struct distance_visitor {
        constexpr iter_difference_t<I2> operator()(const S&, const S2&) const
        {
            return 0;
        }

        template <typename T1, typename T2>
        constexpr iter_difference_t<I2> operator()(const T1& t1, const T2& t2) const
        {
            return t1 - t2;
        }
    };

public:
    using difference_type = iter_difference_t<I>;

    constexpr common_iterator() = default;

    constexpr common_iterator(I i)
        : v_(detail::in_place_type<I>, std::move(i))
    {}

    constexpr common_iterator(S s)
        : v_(detail::in_place_type<S>, std::move(s))
    {}

    template <
        typename II, typename SS,
        std::enable_if_t<ConvertibleTo<II, I> && ConvertibleTo<SS, S>, int> = 0>
    constexpr common_iterator(const common_iterator<II, SS>& other)
        : v_(detail::visit(copy_visitor<II, SS>{}, other.v_))
    {}

    template <typename II, typename SS>
    constexpr std::enable_if_t<ConvertibleTo<II, I> && ConvertibleTo<SS, S>,
                               common_iterator&>
    operator=(const common_iterator<II, SS>& other)
    {
        v_ = detail::visit(copy_visitor<II, SS>{}, other.v_);
        return *this;
    }

    constexpr decltype(auto) operator*() { return *iter(); }

    template <typename II = I,
              std::enable_if_t<detail::Dereferenceable<const I>, int> = 0>
    constexpr decltype(auto) operator*() const
    {
        return *iter();
    }

    template <typename II = I>
    constexpr auto operator-> () const
        -> decltype(common_iterator::do_op_arrow(std::declval<const II&>(),
                                                 detail::priority_tag<2>{}))
    {
        return do_op_arrow(iter(), detail::priority_tag<2>{});
    }

    constexpr common_iterator& operator++()
    {
        ++iter();
        return *this;
    }

    template <typename II = I, std::enable_if_t<!ForwardIterator<II>, int> = 0>
    constexpr  decltype(auto) operator++(int)
    {
        return iter()++;
    }

    template <typename II = I, std::enable_if_t<ForwardIterator<II>, int> = 0>
    constexpr common_iterator operator++(int)
    {
        common_iterator tmp = *this;
        ++iter();
        return tmp;
    }

    template <typename I2, typename S2>
    friend constexpr auto operator==(const common_iterator& x,
                                     const common_iterator<I2, S2>& y)
        -> std::enable_if_t<Sentinel<S2, I> && Sentinel<S, I2> &&
                            !EqualityComparableWith<I, I2>, bool>
    {
        return detail::visit(equality_visitor<I2, S2>{}, x.v_, access::get_variant(y));
    }

    template <typename I2, typename S2>
    friend constexpr auto operator==(const common_iterator& x,
                                     const common_iterator<I2, S2>& y)
        -> std::enable_if_t<Sentinel<S2, I> && Sentinel<S, I2> &&
                            EqualityComparableWith<I, I2>, bool>
    {
        return detail::visit(equality_visitor<I2, S2>{}, x.v_, access::get_variant(y));
    }

    template <typename I2, typename S2>
    friend constexpr auto operator!=(const common_iterator& x,
                                     const common_iterator<I2, S2>& y)
        -> std::enable_if_t<Sentinel<S2, I> && Sentinel<S, I2>, bool>
    {
        return !(x == y);
    }

    template <typename I2, typename S2>
    friend constexpr auto operator-(const common_iterator& x,
                                    const common_iterator<I2, S2>& y)
        -> std::enable_if_t<SizedSentinel<I, I2> && SizedSentinel<S, I2> &&
                            SizedSentinel<S, I2>, iter_difference_t<I2>>
    {
        return detail::visit(distance_visitor<I2, S2>{}, x.v_, access::get_variant(y));
    }

    friend constexpr iter_rvalue_reference_t<I> iter_move(const common_iterator& i)
    {
        return ranges::iter_move(i.iter());
    }

    template <typename I2, typename S2>
    friend constexpr std::enable_if_t<IndirectlySwappable<I2, I>>
    iter_swap(const common_iterator& x, const common_iterator<I2, S2>& y)
    {
        return ranges::iter_swap(x.iter(), y.iter());
    }

private:
    constexpr I& iter() { return detail::unsafe_get<I>(v_); }
    constexpr const I& iter() const { return detail::unsafe_get<I>(v_); }

    detail::variant<I, S> v_;
};

} // namespace common_iterator_

using common_iterator_::common_iterator;

template <typename I, typename S>
struct readable_traits<common_iterator<I, S>> {
    using value_type = iter_value_t<I>;
};

template <typename I, typename S>
struct incrementable_traits<common_iterator<I, S>> {
    using difference_type = iter_difference_t<I>;
};

template <typename I, typename S>
struct iterator_category<common_iterator<I, S>>
    : std::conditional<ForwardIterator<I>, forward_iterator_tag,
                       input_iterator_tag> {
};

NANO_END_NAMESPACE

namespace std {

template <typename I, typename S>
struct iterator_traits<::nano::common_iterator<I, S>> {
    using difference_type =
        ::nano::iter_difference_t<::nano::common_iterator<I, S>>;
    using value_type = ::nano::iter_value_t<::nano::common_iterator<I, S>>;
    using pointer =
        std::add_pointer_t<::nano::iter_reference_t<::nano::common_iterator<I, S>>>;
    using reference = ::nano::iter_reference_t<::nano::common_iterator<I, S>>;
    using iterator_category =
        std::conditional_t<::nano::ForwardIterator<I>,
                           std::forward_iterator_tag,
                           std::input_iterator_tag>;
};

} // namespace std

#endif
