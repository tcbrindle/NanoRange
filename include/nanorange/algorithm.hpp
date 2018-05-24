// nanorange/concepts.hpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef NANORANGE_ALGORITHM_HPP_INCLUDED
#define NANORANGE_ALGORITHM_HPP_INCLUDED

// Algorithms reimplemented in Nanorange
#include <nanorange/algorithm/adjacent_find.hpp>
#include <nanorange/algorithm/all_of.hpp>
#include <nanorange/algorithm/any_of.hpp>
#include <nanorange/algorithm/copy.hpp>
#include <nanorange/algorithm/count.hpp>
#include <nanorange/algorithm/equal.hpp>
#include <nanorange/algorithm/find.hpp>
#include <nanorange/algorithm/find_first_of.hpp>
#include <nanorange/algorithm/for_each.hpp>
#include <nanorange/algorithm/mismatch.hpp>
#include <nanorange/algorithm/move.hpp>
#include <nanorange/algorithm/none_of.hpp>
#include <nanorange/algorithm/replace.hpp>
#include <nanorange/algorithm/search.hpp>
#include <nanorange/algorithm/search_n.hpp>
#include <nanorange/algorithm/swap_ranges.hpp>
#include <nanorange/algorithm/transform.hpp>

// Algorithms which reuse the STL implementation
#include <nanorange/detail/algorithm/stl/find_end.hpp>

#endif
