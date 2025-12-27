#include <memory>
#include <sstream>
#include <string>
#include <tuple>

#include "../src_cpp/lib/interpreter/bind_args.hpp"
#include "../src_cpp/lib/interpreter/operation.hpp"
#include "../src_cpp/lib/interpreter/value.hpp"

#include "testmacros.hpp"

using namespace aquila;
using namespace aquila::interpreter;

class TestOperation : public Operation
{
public:
    BIND_ARGS(&TestOperation::run);

    std::unique_ptr<Value> run(const RealValue &a, const IntValue &b, const StrValue &s) const
    {
        std::stringstream ss;
        ss << "I got an float " << a << ", int " << b << " and string " << s << ".";
        return std::make_unique<StrValue>(ss.str());
    }
    std::string name() const override { return "test"; }
};

class EmptyOperation : public Operation
{
public:
    BIND_ARGS(&EmptyOperation::run);
    std::unique_ptr<Value> run() const { return std::make_unique<StrValue>("noooo"); }
    std::string name() const override { return "test"; }
};

// class VariaOperation : public Operation
// {
// public:
//     std::unique_ptr<Value> call(const std::vector<const Value *> &args) override
//     {
//         return bind_args(this, &VariaOperation::run, args);
//     }

//     std::unique_ptr<Value> run(const std::vector<const RealValue *> &a)
//     {
//         Real result = 0;
//         for (const auto &aa : a)
//         {
//             result += aa->value;
//         }
//         return std::make_unique<RealValue>(result);
//     }
// };

class DirectCastOp : public Operation
{
public:
    BIND_ARGS(&DirectCastOp::run);

    std::unique_ptr<Value> run(const Real &a, const Int &b) const
    {
        return std::make_unique<RealValue>(a * b);
    }
    std::string name() const override { return "test"; }
};

TEST(normal)
{
    RealValue r{1.0};
    IntValue i{2};
    StrValue s{"three"};

    std::vector<const Value *> inputs{&r, &i, &s};

    TestOperation addop;
    auto result = addop.call(inputs);

    std::cout << *result << std::endl;
}

TEST(direct)
{
    RealValue r{1.0};
    IntValue i{2};

    std::vector<const Value *> inputs{&r, &i};

    DirectCastOp addop;
    auto result = addop.call(inputs);

    std::cout << *result << std::endl;
}

TEST(empty)
{

    EmptyOperation emptyop;
    auto result = emptyop.call({});

    std::cout << *result << std::endl;
}

TEST(wrongtype)
{
    RealValue r{1.0};
    RealValue i{2};
    StrValue s{"three"};

    std::vector<const Value *> inputs{&r, &i, &s};

    TestOperation addop;

    EXPECT_ERROR("interpret",
        [&]()
        {
            auto result = addop.call(inputs);
            std::cout << *result << std::endl;
        });
}

TEST(toomany)
{

    RealValue r{1.0};
    IntValue i{2};
    StrValue s{"three"};
    StrValue f{"oops"};

    std::vector<const Value *> inputs{&r, &i, &s, &f};

    TestOperation addop;

    EXPECT_ERROR("length",
        [&]()
        {
            auto result = addop.call(inputs);
            std::cout << *result << std::endl;
        });
}

TEST(notenough)
{

    RealValue r{1.0};
    IntValue i{2};

    std::vector<const Value *> inputs{&r, &i};

    TestOperation addop;

    EXPECT_ERROR("length",
        [&]()
        {
            auto result = addop.call(inputs);
            std::cout << *result << std::endl;
        });
}

int main()
{
    int failed = 0;
    RUN_ALL(failed);
    return failed != 0;
}