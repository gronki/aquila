#pragma once

#include <functional>
#include <memory>
#include <string>

#include "bind_args.hpp"
#include "value.hpp"

namespace aquila::interpreter
{

template <typename T>
inline bool is_type(const Value &val)
{
    return dynamic_cast<T *>(val) != nullptr;
}

inline bool default_check(const Value &val)
{
    return true;
}

using CheckFunction = std::function<bool(const Value &)>;

struct ArgManifest
{
    std::string name;
    std::unique_ptr<Value> defval;
    CheckFunction check = default_check;
};

class Operation
{
public:
    // virtual std::vector<ArgManifest> arg_manifest() = 0;
    virtual std::unique_ptr<Value> call(const std::vector<Value *> &) = 0;
    virtual ~Operation() = default;
};

} // namespace aquila::interpreter