#pragma once

#include <map>

#include "value.hpp"

namespace aquila::interpreter
{

class Namespace
{
    std::map<std::string, ValuePtr> vault;
    const Namespace *global = nullptr;

public:
    Namespace() {}
    Namespace(const Namespace *global) : global(global) {}

    ValuePtr push(const std::string &name, ValuePtr v);
    ValuePtr get(const std::string &name) const;
    bool contains(const std::string &name) const;
    void merge(Namespace other);
};

} // namespace aquila::interpreter
