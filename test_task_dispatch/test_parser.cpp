#include "../src_cpp/interpreter/ast.hpp"
#include "../src_cpp/interpreter/characters.hpp"
#include "../src_cpp/interpreter/parser.hpp"
#include "../src_cpp/interpreter/tokenizer.hpp"
#include "testmacros.hpp"

using namespace aquila;
using namespace aquila::interpreter;

TEST(ident)
{
    Tokenizer tokenizer("a");
    LazyTokenArray token_array(std::move(tokenizer));
    std::unique_ptr<AstNode> root;
    parse(token_array, root);
    std::cout << *root << std::endl;

    auto *ref_node = dynamic_cast<AstRefNode *>(root.get());
    REQUIRE_NNUL(ref_node);
    REQUIRE_EQ(ref_node->refname, "a");
}

TEST(num_lit)
{
    Tokenizer tokenizer("3.0");
    LazyTokenArray token_array(std::move(tokenizer));
    std::unique_ptr<AstNode> root;
    parse(token_array, root);
    std::cout << *root << std::endl;

    auto *value_node = dynamic_cast<AstValueNode *>(root.get());
    REQUIRE_NNUL(value_node);

    REQUIRE_EQ(*value_node->constant, RealValue(3.0));
}

TEST(str_lit)
{

    Tokenizer tokenizer("   \"3.0\"");
    LazyTokenArray token_array(std::move(tokenizer));
    std::unique_ptr<AstNode> root;
    parse(token_array, root);
    std::cout << *root << std::endl;

    auto *value_node = dynamic_cast<AstValueNode *>(root.get());
    REQUIRE_NNUL(value_node);

    REQUIRE_EQ(*value_node->constant, StrValue("3.0"));
}

TEST(call)
{
    Tokenizer tokenizer(" ff (3.0, \"a\"  )");
    LazyTokenArray token_array(std::move(tokenizer));
    std::unique_ptr<AstNode> root;
    parse(token_array, root);
    std::cout << *root << std::endl;

    auto *op_node = dynamic_cast<AstOpNode *>(root.get());
    REQUIRE_NNUL(op_node);
    REQUIRE_EQ(op_node->opname, "ff");
    REQUIRE_EQ(op_node->args.size(), 2);

    auto &args = op_node->args;

    auto *arg_node_1 = dynamic_cast<AstValueNode *>(args[0].arg_val.get());
    REQUIRE_NNUL(arg_node_1);
    REQUIRE_EQ(*arg_node_1->constant, RealValue(3.0));

    auto *arg_node_2 = dynamic_cast<AstValueNode *>(args[1].arg_val.get());
    REQUIRE_NNUL(arg_node_2);
    REQUIRE_EQ(*arg_node_2->constant, StrValue("a"));
}

TEST(call_no_paren)
{
    Tokenizer tokenizer(" ff 3.0, \"a\"  ");
    LazyTokenArray token_array(std::move(tokenizer));
    std::unique_ptr<AstNode> root;
    parse(token_array, root);
    std::cout << *root << std::endl;

    auto *op_node = dynamic_cast<AstOpNode *>(root.get());
    REQUIRE_NNUL(op_node);
    REQUIRE_EQ(op_node->opname, "ff");
    REQUIRE_EQ(op_node->args.size(), 2);

    auto &args = op_node->args;

    auto *arg_node_1 = dynamic_cast<AstValueNode *>(args[0].arg_val.get());
    REQUIRE_NNUL(arg_node_1);
    REQUIRE_EQ(*arg_node_1->constant, RealValue(3.0));

    auto *arg_node_2 = dynamic_cast<AstValueNode *>(args[1].arg_val.get());
    REQUIRE_NNUL(arg_node_2);
    REQUIRE_EQ(*arg_node_2->constant, StrValue("a"));
}

TEST(call_no_paren_array_literal)
{
    Tokenizer tokenizer(" add [1, 2], 3 ");
    LazyTokenArray token_array(std::move(tokenizer));
    std::unique_ptr<AstNode> root;
    parse(token_array, root);
    std::cout << *root << std::endl;

    auto *op_node = dynamic_cast<AstOpNode *>(root.get());
    REQUIRE_NNUL(op_node);
    REQUIRE_EQ(op_node->opname, "add");
    REQUIRE_EQ(op_node->args.size(), 2);

    auto &args = op_node->args;

    // First argument: array(1, 2)
    auto *arg_node_1 = dynamic_cast<AstOpNode *>(args[0].arg_val.get());
    REQUIRE_NNUL(arg_node_1);
    REQUIRE_EQ(arg_node_1->opname, "array");
    REQUIRE_EQ(arg_node_1->args.size(), 2);

    auto *arr_arg_1 = dynamic_cast<AstValueNode *>(arg_node_1->args[0].arg_val.get());
    REQUIRE_NNUL(arr_arg_1);
    REQUIRE_EQ(*arr_arg_1->constant, RealValue(1.0));

    auto *arr_arg_2 = dynamic_cast<AstValueNode *>(arg_node_1->args[1].arg_val.get());
    REQUIRE_NNUL(arr_arg_2);
    REQUIRE_EQ(*arr_arg_2->constant, RealValue(2.0));

    // Second argument: 3
    auto *arg_node_2 = dynamic_cast<AstValueNode *>(args[1].arg_val.get());
    REQUIRE_NNUL(arg_node_2);
    REQUIRE_EQ(*arg_node_2->constant, RealValue(3.0));
}

TEST(call_chain_keys)
{
    Tokenizer tokenizer(std::string(" ff (3.0)") + std::string(1, CHAIN_CALL_DELIM)
        + " gg(key: \"a\"  )");
    // equivalent to: gg(ff(3.0), key: "a")

    LazyTokenArray token_array(std::move(tokenizer));
    std::unique_ptr<AstNode> root;
    parse(token_array, root);
    std::cout << *root << std::endl;

    auto *op_node = dynamic_cast<AstOpNode *>(root.get());
    REQUIRE_NNUL(op_node);
    REQUIRE_EQ(op_node->opname, "gg");
    REQUIRE_EQ(op_node->args.size(), 2);

    auto &args = op_node->args;

    REQUIRE_EQ(args[0].key, "");
    auto *arg_node_1 = dynamic_cast<AstOpNode *>(args[0].arg_val.get());
    REQUIRE_NNUL(arg_node_1);
    REQUIRE_EQ(arg_node_1->opname, "ff");
    REQUIRE_EQ(arg_node_1->args.size(), 1);

    REQUIRE_EQ(arg_node_1->args[0].key, "");
    auto *arg_node_11 = dynamic_cast<AstValueNode *>(arg_node_1->args[0].arg_val.get());
    REQUIRE_NNUL(arg_node_11);
    REQUIRE_EQ(*arg_node_11->constant, RealValue(3.0));

    REQUIRE_EQ(args[1].key, "key");
    auto *arg_node_2 = dynamic_cast<AstValueNode *>(args[1].arg_val.get());
    REQUIRE_NNUL(arg_node_2);
    REQUIRE_EQ(*arg_node_2->constant, StrValue("a"));
}

TEST(call_chain_keys_no_paren)
{
    Tokenizer tokenizer(
        std::string(" ff 3.0") + std::string(1, CHAIN_CALL_DELIM) + " gg key: \"a\"  ");
    // equivalent to: gg(ff(3.0), key: "a")

    LazyTokenArray token_array(std::move(tokenizer));
    std::unique_ptr<AstNode> root;
    parse(token_array, root);
    std::cout << *root << std::endl;

    auto *op_node = dynamic_cast<AstOpNode *>(root.get());
    REQUIRE_NNUL(op_node);
    REQUIRE_EQ(op_node->opname, "gg");
    REQUIRE_EQ(op_node->args.size(), 2);

    auto &args = op_node->args;

    REQUIRE_EQ(args[0].key, "");
    auto *arg_node_1 = dynamic_cast<AstOpNode *>(args[0].arg_val.get());
    REQUIRE_NNUL(arg_node_1);
    REQUIRE_EQ(arg_node_1->opname, "ff");
    REQUIRE_EQ(arg_node_1->args.size(), 1);

    REQUIRE_EQ(arg_node_1->args[0].key, "");
    auto *arg_node_11 = dynamic_cast<AstValueNode *>(arg_node_1->args[0].arg_val.get());
    REQUIRE_NNUL(arg_node_11);
    REQUIRE_EQ(*arg_node_11->constant, RealValue(3.0));

    REQUIRE_EQ(args[1].key, "key");
    auto *arg_node_2 = dynamic_cast<AstValueNode *>(args[1].arg_val.get());
    REQUIRE_NNUL(arg_node_2);
    REQUIRE_EQ(*arg_node_2->constant, StrValue("a"));
}

TEST(call_chain_keys_2)
{
    Tokenizer tokenizer(std::string(" t( ff (3.0) ") + std::string(1, CHAIN_CALL_DELIM)
        + "gg(key: \"a\"  ) )");
    // equivalent to: gg(ff(3.0), key: "a")

    LazyTokenArray token_array(std::move(tokenizer));
    std::unique_ptr<AstNode> root;
    parse(token_array, root);
    std::cout << *root << std::endl;

    auto *big_op_node = dynamic_cast<AstOpNode *>(root.get());
    REQUIRE_NNUL(big_op_node);
    REQUIRE_EQ(big_op_node->opname, "t");
    REQUIRE_EQ(big_op_node->args.size(), 1);

    auto &big_args = big_op_node->args;

    auto *op_node = dynamic_cast<AstOpNode *>(big_args[0].arg_val.get());
    REQUIRE_NNUL(op_node);
    REQUIRE_EQ(op_node->opname, "gg");
    REQUIRE_EQ(op_node->args.size(), 2);

    auto &args = op_node->args;

    REQUIRE_EQ(args[0].key, "");
    auto *arg_node_1 = dynamic_cast<AstOpNode *>(args[0].arg_val.get());
    REQUIRE_NNUL(arg_node_1);
    REQUIRE_EQ(arg_node_1->opname, "ff");
    REQUIRE_EQ(arg_node_1->args.size(), 1);

    REQUIRE_EQ(arg_node_1->args[0].key, "");
    auto *arg_node_11 = dynamic_cast<AstValueNode *>(arg_node_1->args[0].arg_val.get());
    REQUIRE_NNUL(arg_node_11);
    REQUIRE_EQ(*arg_node_11->constant, RealValue(3.0));

    REQUIRE_EQ(args[1].key, "key");
    auto *arg_node_2 = dynamic_cast<AstValueNode *>(args[1].arg_val.get());
    REQUIRE_NNUL(arg_node_2);
    REQUIRE_EQ(*arg_node_2->constant, StrValue("a"));
}

TEST(call_chain_keys_3)
{
    Tokenizer tokenizer(std::string(" ff (3.0) ") + std::string(1, CHAIN_CALL_DELIM)
        + " gg( ) " + std::string(1, CHAIN_CALL_DELIM) + "t()");
    // equivalent to: gg(ff(3.0), key: "a")

    LazyTokenArray token_array(std::move(tokenizer));
    std::unique_ptr<AstNode> root;
    parse(token_array, root);
    std::cout << *root << std::endl;

    auto *big_op_node = dynamic_cast<AstOpNode *>(root.get());
    REQUIRE_NNUL(big_op_node);
    REQUIRE_EQ(big_op_node->opname, "t");
    REQUIRE_EQ(big_op_node->args.size(), 1);

    auto &big_args = big_op_node->args;

    auto *op_node = dynamic_cast<AstOpNode *>(big_args[0].arg_val.get());
    REQUIRE_NNUL(op_node);
    REQUIRE_EQ(op_node->opname, "gg");
    REQUIRE_EQ(op_node->args.size(), 1);

    auto &args = op_node->args;

    REQUIRE_EQ(args[0].key, "");
    auto *arg_node_1 = dynamic_cast<AstOpNode *>(args[0].arg_val.get());
    REQUIRE_NNUL(arg_node_1);
    REQUIRE_EQ(arg_node_1->opname, "ff");
    REQUIRE_EQ(arg_node_1->args.size(), 1);

    REQUIRE_EQ(arg_node_1->args[0].key, "");
    auto *arg_node_11 = dynamic_cast<AstValueNode *>(arg_node_1->args[0].arg_val.get());
    REQUIRE_NNUL(arg_node_11);
    REQUIRE_EQ(*arg_node_11->constant, RealValue(3.0));
}

TEST(call_param)
{
    Tokenizer tokenizer(
        std::string(" ff (3.0) ") + std::string(1, CHAIN_CALL_DELIM) + " gg");
    // equivalent to: gg(ff(3.0), key: "a")

    LazyTokenArray token_array(std::move(tokenizer));
    std::unique_ptr<AstNode> root;
    parse(token_array, root);
    std::cout << *root << std::endl;

    auto *op_node = dynamic_cast<AstOpNode *>(root.get());
    REQUIRE_NNUL(op_node);
    REQUIRE_EQ(op_node->opname, "param");
    REQUIRE_EQ(op_node->args.size(), 2);

    auto &args = op_node->args;

    REQUIRE_EQ(args[0].key, "");
    auto *arg_node_1 = dynamic_cast<AstOpNode *>(args[0].arg_val.get());
    REQUIRE_NNUL(arg_node_1);
    REQUIRE_EQ(arg_node_1->opname, "ff");
    REQUIRE_EQ(arg_node_1->args.size(), 1);

    REQUIRE_EQ(arg_node_1->args[0].key, "");
    auto *arg_node_11 = dynamic_cast<AstValueNode *>(arg_node_1->args[0].arg_val.get());
    REQUIRE_NNUL(arg_node_11);
    REQUIRE_EQ(*arg_node_11->constant, RealValue(3.0));

    auto *arg_node_2 = dynamic_cast<AstValueNode *>(args[1].arg_val.get());
    REQUIRE_NNUL(arg_node_2);
    REQUIRE_EQ(*arg_node_2->constant, StrValue("gg"));
}

TEST(print)
{
    Tokenizer tokenizer(std::string("  ff (3.0) ") + std::string(1, CHAIN_CALL_DELIM)
        + " gg(key: \"a\" , \"q\"" + std::string(1, CHAIN_CALL_DELIM) + "split ) "
        + std::string(1, CHAIN_CALL_DELIM) + "t(p: q" + std::string(1, CHAIN_CALL_DELIM)
        + "prop, v: "
          "z) "
        + std::string(1, CHAIN_CALL_DELIM) + " ababa(33, xyz, 13) ");
    LazyTokenArray token_array(std::move(tokenizer));
    std::unique_ptr<AstNode> root;
    parse(token_array, root);
    std::cout << *root << std::endl;
}

int main(int argc, char **argv)
{
    int failed = 0;
    RUN_ALL(failed);
    std::cout << "failed tests: " << failed << std::endl;
    return failed != 0;
}
