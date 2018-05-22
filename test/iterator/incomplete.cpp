#include <stl2/iterator.hpp>
#include <stl2/type_traits.hpp>

namespace ranges = __stl2;

struct foo;
static_assert(ranges::models::Same<std::ptrdiff_t,ranges::difference_type_t<foo*>>);
static_assert(ranges::models::Same<foo*,ranges::common_type_t<foo*, foo*>>);
struct foo {};

static_assert(ranges::models::ContiguousIterator<foo*>);

int main() {}
