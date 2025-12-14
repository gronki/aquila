#include <memory>
#include <sstream>
#include <string>
#include <tuple>

#include "../src_cpp/lib/interpreter/bind_args.hpp"
#include "../src_cpp/lib/interpreter/operation.hpp"
#include "../src_cpp/lib/interpreter/value.hpp"

using namespace aquila::interpreter;

class TestOperation : public Operation
{
public:
    BIND_ARGS(TestOperation::run);

    std::unique_ptr<Value> run(const RealValue &a, const IntValue &b, const StrValue &s)
    {
        std::stringstream ss;
        ss << "I got an float " << a << ", int " << b << " and string " << s << ".";
        return std::make_unique<StrValue>(ss.str());
    }
};

class EmptyOperation : public Operation
{
public:
    BIND_ARGS(EmptyOperation::run);

    std::unique_ptr<Value> run() { return std::make_unique<StrValue>("noooo"); }
};

int main()
{
    RealValue r{1.0};
    IntValue i{2};
    StrValue s{"three"};

    std::vector<const Value *> inputs{&r, &i, &s};

    TestOperation addop;
    auto result = addop.call(inputs);
    std::cout << dynamic_cast<StrValue &>(*result).value << std::endl;

    EmptyOperation emptyop;
    std::cout << dynamic_cast<StrValue &>(*emptyop.call({})).value << std::endl;
}