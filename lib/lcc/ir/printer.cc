#include <lcc/ir/printer.hh>

#include <lcc/ir/ir.hh>

#include <unordered_map>

namespace lcc {
std::unordered_map<Value*, usz> ValuePrinter::ids{};
}
