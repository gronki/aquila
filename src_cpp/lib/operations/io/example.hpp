#include "../../interpreter/interpreter.hpp"
#include "../../values/frame.hpp"

namespace aquila::ops
{

// struct ExampleOp : public Operation
// {
//     // std::unique_ptr<Value> call(const std::vector<const Value *> &) const {}
//     BIND_ARGS(&ExampleOp::run);
//     std::unique_ptr<Value> run(const String &param) const {}
//     std::optional<ArgManifest> arg_manifest() const { return std::nullopt; }
//     std::string name() const { return "op"; }
// };

// REGISTER(ExampleOp);

} // namespace aquila::ops