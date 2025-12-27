#pragma once

#include <map>

#include "value.hpp"

namespace aquila::interpreter
{

class Namespace
{
    std::map<String, std::unique_ptr<Value>> vault;
    const Namespace *global = nullptr;

public:
    Namespace() {}
    Namespace(const Namespace &global) : global(&global) {}

    void push(const String &name, std::unique_ptr<Value> v);
    const Value &get(const String &name) const;
    bool contains(const String &name) const;
};

} // namespace aquila::interpreter