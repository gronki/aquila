#pragma once

#include <functional>
#include <memory>
#include <optional>
#include <stdexcept>
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

using CheckFunction = bool (*)(const Value &);

struct ArgSpec
{
    std::string name, help = "";

    // this might not be the prettiest way, but makes it easy
    // to build manifests with initializer lists and ensures
    // that user does not give anything crazy as defaults.
    std::optional<Int> default_int = std::nullopt;
    std::optional<Real> default_real = std::nullopt;
    std::optional<String> default_str = std::nullopt;

    bool has_default() const;
    std::unique_ptr<Value> build_default() const;
    // CheckFunction check = default_check;
};

class Operation
{
public:
    virtual std::optional<std::vector<ArgSpec>> arg_manifest() const { return std::nullopt; }
    virtual std::unique_ptr<Value> call(const std::vector<const Value *> &) const = 0;
    virtual ~Operation() = default;
};

#define BIND_ARGS(proc)                                                                \
    std::unique_ptr<Value> call(const std::vector<const Value *> &args) const override \
    {                                                                                  \
        return bind_args(this, (proc), args);                                          \
    }

std::vector<const Value *> build_ptrs(
    const std::vector<std::unique_ptr<Value>> &given_args);

struct ArgMatch
{
    bool matched = false;
    size_t pos;
    std::unique_ptr<Value> deftgt = nullptr;
};

std::vector<ArgMatch> match_arguments(
    const std::vector<ArgSpec> &manifest, const std::vector<std::string> &given_keys);

std::vector<const Value *> build_ptrs_from_match(
    const std::vector<std::unique_ptr<Value>> &given_args,
    const std::vector<ArgMatch> &match);

} // namespace aquila::interpreter