#include "../src_cpp/lib/interpreter/ast.hpp"
#include "../src_cpp/lib/interpreter/parser.hpp"
#include "../src_cpp/lib/interpreter/tokenizer.hpp"
#include "testmacros.hpp"

using namespace aquila;
using namespace aquila::interpreter;

TEST(ident)
{
    std::unique_ptr<AstNode> root = parse(tokenize("a"));

    auto *ref_node = dynamic_cast<AstRefNode *>(root.get());
    REQUIRE_NNUL(ref_node);
    REQUIRE_EQ(ref_node->refname, "a");
}

TEST(num_lit)
{
    std::unique_ptr<AstNode> root = parse(tokenize("3.0"));

    auto *value_node = dynamic_cast<AstValueNode *>(root.get());
    REQUIRE_NNUL(value_node);

    REQUIRE_EQ(*value_node->constant, RealValue(3.0));
}

TEST(call)
{
    std::unique_ptr<AstNode> root = parse(tokenize(" ff (3.0, \"a\"   )"));

    auto *op_node = dynamic_cast<AstOpNode *>(root.get());
    REQUIRE_NNUL(op_node);
    REQUIRE_EQ(op_node->opname, "ff");
    REQUIRE_EQ(op_node->args.size(), 2);

    auto &args = op_node->args;

    auto *arg_node_1 = dynamic_cast<AstValueNode *>(args[1].arg_val.get());
    REQUIRE_NNUL(arg_node_1);
    REQUIRE_EQ(*arg_node_1->constant, RealValue(3.0));

    auto *arg_node_2 = dynamic_cast<AstValueNode *>(args[1].arg_val.get());
    REQUIRE_NNUL(arg_node_2);
    REQUIRE_EQ(*arg_node_2->constant, StrValue("a"));
}

int main(int argc, char **argv)
{
    int failed = 0;
    RUN_ALL(failed);
    std::cout << "failed tests: " << failed << std::endl;
    return failed != 0;
}