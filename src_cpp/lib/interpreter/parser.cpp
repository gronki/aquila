#include "parser.hpp"
#include "characters.hpp"

namespace aquila::interpreter
{

/**
 * Definitions of static procedures in this file.
 */
static void parse_basic_expression(LazyTokenArray &tokens, std::unique_ptr<AstNode> &node);
static void parse_function_argument_list(LazyTokenArray &tokens,
                                         std::vector<AstOpNode::OpArg> &node_args);
static void parse_expression(LazyTokenArray &tokens, std::unique_ptr<AstNode> &node);

/**
 * Parse an expression (literal, reference, function call) but not including chaining.
 */
static void parse_basic_expression(LazyTokenArray &tokens, std::unique_ptr<AstNode> &node)
{
    const Token &token = tokens.cur_token();

    if (token.type == TokenType::END)
        return;

    if (token.type == TokenType::NUM_LITERAL)
    {
        auto value_node = std::make_unique<AstValueNode>();
        value_node->constant = std::make_unique<RealValue>(std::stod(token.value));
        value_node->loc = token.loc;
        node = std::move(value_node);
        tokens.next_token();
        return;
    }

    if (token.type == TokenType::STR_LITERAL)
    {
        auto value_node = std::make_unique<AstValueNode>();
        value_node->constant = std::make_unique<StrValue>(token.value);
        value_node->loc = token.loc;
        node = std::move(value_node);
        tokens.next_token();
        return;
    }

    if (token.type != TokenType::IDENT)
    {
        throw std::runtime_error(std::string("unexpected token: ") + token.str());
        return;
    }

    Token maybe_opening_brace = tokens.next_token();

    if (maybe_opening_brace != Token(TokenType::DELIM, "("))
    {
        auto ref_node = std::make_unique<AstRefNode>();
        ref_node->refname = token.value;
        ref_node->loc = token.loc;
        node = std::move(ref_node);
        return;
    }

    tokens.next_token();

    // here we know that we are parsing function argument list

    auto op_node = std::make_unique<AstOpNode>();
    op_node->opname = token.value;
    op_node->loc = token.loc;
    parse_function_argument_list(tokens, op_node->args);
    node = std::move(op_node);
}

/**
 * Parse function argument list (starting from the first token after the opening brace).
 */
static void parse_function_argument_list(LazyTokenArray &tokens,
                                         std::vector<AstOpNode::OpArg> &node_args)
{

    while (true)
    {
        Token cur_token = tokens.cur_token();

        if (cur_token == Token(TokenType::DELIM, ")"))
        {
            tokens.next_token();
            return;
        }
        else if (cur_token.type == TokenType::END)
        {
            throw std::runtime_error("unexpected end of input while parsing argument "
                                     "list");
        }

        // first we check if perhaps a keyword argument is given, such as key: val
        Token maybe_kv_sep = tokens.peek_token(1);

        AstOpNode::OpArg arg;

        if (cur_token.type == TokenType::IDENT &&
            maybe_kv_sep == Token(TokenType::DELIM, KWARG_DELIM))
        {
            tokens.next_token(2);
            arg.key = std::move(cur_token.value);
        }
        parse_expression(tokens, arg.arg_val);

        node_args.push_back(std::move(arg));

        cur_token = tokens.cur_token();

        if (cur_token == Token(TokenType::DELIM, ","))
        {
            tokens.next_token();
        }
        else if (cur_token == Token(TokenType::DELIM, ")"))
        {
            // in principle we would not need to repeat that check twice,
            // but we do so to throw a clearer error message
            tokens.next_token();
            return;
        }
        else if (cur_token.type == TokenType::END)
        {
            throw std::runtime_error("unexpected end of input while parsing argument "
                                     "list");
        }
        else
        {
            throw std::runtime_error(std::string("expected , or ), but got: ") +
                                     cur_token.value);
        }
    }

    return; // unreachable
}

/**
 * Parse the expression but take care of chaining. Calls like f(a,b).g(c,d)
 * will be reformed to g(f(a, b), c, d).
 */
static void parse_expression(LazyTokenArray &tokens, std::unique_ptr<AstNode> &node)
{
    parse_basic_expression(tokens, node);

    std::cout << "CHAIN " << tokens.cur_token() << std::endl;

    while (tokens.cur_token() == Token(TokenType::DELIM, CHAIN_CALL_DELIM))
    {

        // we might have a chained call here
        tokens.next_token();

        std::unique_ptr<AstNode> parent_node;
        parse_basic_expression(tokens, parent_node);

        if (auto *parent_call_node = dynamic_cast<AstOpNode *>(parent_node.get()))
        {
            // stadard chaining: X % f(Y) -> F(X, Y)
            AstOpNode::OpArg first_arg;
            first_arg.arg_val = std::move(node);
            parent_call_node->args.insert(parent_call_node->args.begin(),
                                          std::move(first_arg));
            node = std::move(parent_node);
        }
        else if (auto *parent_call_node = dynamic_cast<AstRefNode *>(parent_node.get()))
        {
            // param function: X % Y -> param(X, "Y")
            // used to retrieve some (static) properties from a value
            // for example, an exposure time from FITS header
            auto param_op = std::make_unique<AstOpNode>();
            param_op->opname = "param";
            param_op->args.push_back(AstOpNode::OpArg{.arg_val = std::move(node)});
            auto param_arg = std::make_unique<AstValueNode>();
            param_arg->constant = std::make_unique<StrValue>(parent_call_node->refname);
            param_op->args.push_back(AstOpNode::OpArg{.arg_val = std::move(param_arg)});
            node = std::move(param_op);
        }
        else
        {
            throw std::runtime_error("incorrect chaining");
        }
    }

    return;
}

/**
 * Main parsing function.
 */
void parse(LazyTokenArray &tokens, std::unique_ptr<AstNode> &root)
{
    parse_expression(tokens, root);
}

} // namespace aquila::interpreter