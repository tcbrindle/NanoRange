#include <nanorange.hpp>

namespace ranges = nano::ranges;

struct foo;
static_assert(ranges::Same<std::ptrdiff_t,ranges::difference_type_t<foo*>>, "");
static_assert(ranges::Same<foo*,ranges::common_type_t<foo*, foo*>>, "");
struct foo {};

// FIXME: Contiguous iterators
//static_assert(ranges::ContiguousIterator<foo*>);
static_assert(ranges::RandomAccessIterator<foo*>, "");
