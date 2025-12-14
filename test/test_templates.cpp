#include <memory>
#include <string>
#include <tuple>

#include "../src_cpp/lib/interpreter/bind_args.hpp"
#include "../src_cpp/lib/interpreter/operation.hpp"
#include "../src_cpp/lib/interpreter/value.hpp"

using namespace aquila::interpreter;

class AddOperation : public Operation
{
public:
    std::unique_ptr<Value> call(const std::vector<Value *> &args) override
    {
        // the whole deal was about: how to bind it automatically?
        // return run(dynamic_cast<RealValue &>(*args[0]), dynamic_cast<IntValue&>(*args[1]));
        return bind_args(this, &AddOperation::run, args);
    }

    std::unique_ptr<Value> run(const RealValue &a, const IntValue &b)
    {
        return std::make_unique<RealValue>(a.value + b.value);
    }
};

int main()
{
    RealValue r1{1};
    IntValue i2{2};
    AddOperation addop;
    // std::cout << dynamic_cast<RealValue &>(*addop.run(r1, r2)).value << std::endl;
    std::cout << dynamic_cast<RealValue &>(*addop.call({&r1, &i2})).value << std::endl;
}