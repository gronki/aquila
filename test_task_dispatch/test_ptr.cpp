#include <iostream>
#include <value.hpp>

using namespace aquila;
using namespace aquila::interpreter;

Ptr<Value> double1(Ptr<StrValue> in)
{
    if (!in)
        return {};
    return make_unique<StrValue>(in->value + " " + in->value);
}

void test1()
{
    auto ptr1 = Ptr<StrValue>::make("test");
    Ptr<Value> ptr2 = double1(ptr1.own());
    std::cout << (bool)ptr1 << (bool)ptr2 << *ptr2 << std::endl;
}
Ptr<Value> larger(Ptr<RealValue> v1, Ptr<RealValue> v2)
{
    if (!v1 || !v2)
        return {};
    if (v1->value > v2->value)
        return v1;
    return v2;
}

void test2()
{
    auto ptr1 = Ptr<RealValue>::make(1.0);
    auto ptr2 = Ptr<RealValue>::make(2.0);
    auto ptr3 = larger(std::move(ptr1), ptr2.get());
    std::cout << ptr3.is_owned() << std::endl;
    std::cout << *ptr3 << std::endl;
}

int main(int argc, char **argv)
{
    test1();
    test2();
}
