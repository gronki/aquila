#include "namespace.hpp"

namespace aquila::interpreter
{

void Namespace::push(const std::string &name, std::unique_ptr<Value> v)
{
    vault.insert_or_assign(name, std::move(v));
}

const Value &Namespace::get(const std::string &name) const
{
    auto it = vault.find(name);
    if (it != vault.end())
        return *it->second;
    if (global && global->contains(name))
        return global->get(name);
    throw std::runtime_error(std::string("No reference to ") + name + " found");
}

bool Namespace::contains(const std::string &name) const
{
    auto it = vault.find(name);
    if (it != vault.end())
        return true;
    if (global)
        return global->contains(name);
    return false;
}

} // namespace aquila::interpreter