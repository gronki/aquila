#include "namespace.hpp"

namespace aquila::interpreter
{

const Value *Namespace::push(const std::string &name, std::unique_ptr<Value> v)
{
    auto [it, replaced] = vault.insert_or_assign(name, std::move(v));
    return it->second.get();
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

void Namespace::merge(Namespace other)
{
    for (auto &it : other.vault)
    {
        vault.insert_or_assign(it.first, std::move(it.second));
    }
}

} // namespace aquila::interpreter
