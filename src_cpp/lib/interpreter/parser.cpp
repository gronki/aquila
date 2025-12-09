#include "parser.hpp"
#include "characters.hpp"

namespace aquila::interpreter
{

/**
 * Definitions of static procedures in this file.
 */
static bool parse_expression(LazyTokenArray &tokens, std::unique_ptr<AstNode> &node);
static bool parse_function_argument_list(LazyTokenArray &tokens,
                                         std::vector<AstOpNode::OpArg> &node_args);
static bool parse_expression_with_chaining(LazyTokenArray &tokens,
                                           std::unique_ptr<AstNode> &node);

/**
 * Parse an expression (literal, reference, function call) but not including chaining.
 */
static bool parse_expression(LazyTokenArray &tokens, std::unique_ptr<AstNode> &node)
{
    const Token &token = tokens.cur_token();

    if (token.type == TokenType::END)
        return false;

    if (token.type == TokenType::NUM_LITERAL)
    {
        auto value_node = std::make_unique<AstValueNode>();
        value_node->constant = std::make_unique<RealValue>(std::stod(token.value));
        value_node->loc = token.loc;
        node = std::move(value_node);
        tokens.next_token();
        return true;
    }

    if (token.type == TokenType::STR_LITERAL)
    {
        auto value_node = std::make_unique<AstValueNode>();
        value_node->constant = std::make_unique<StrValue>(token.value);
        value_node->loc = token.loc;
        node = std::move(value_node);
        tokens.next_token();
        return true;
    }

    if (token.type != TokenType::IDENT)
    {
        throw std::runtime_error(std::string("unexpected token: ") + token.str());
        return false;
    }

    Token maybe_opening_brace = tokens.next_token();

    if (maybe_opening_brace != Token(TokenType::DELIM, "("))
    {
        auto ref_node = std::make_unique<AstRefNode>();
        ref_node->refname = token.value;
        ref_node->loc = token.loc;
        node = std::move(ref_node);
        return true;
    }

    tokens.next_token();

    // here we know that we are parsing function argument list

    auto op_node = std::make_unique<AstOpNode>();
    op_node->opname = token.value;
    op_node->loc = token.loc;
    parse_function_argument_list(tokens, op_node->args);
    node = std::move(op_node);
    return true;
}

/**
 * Parse function argument list (starting from the first token after the opening brace).
 */
static bool parse_function_argument_list(LazyTokenArray &tokens,
                                         std::vector<AstOpNode::OpArg> &node_args)
{

    while (true)
    {
        Token cur_token = tokens.cur_token();

        if (cur_token == Token(TokenType::DELIM, ")"))
        {
            tokens.next_token();
            return true;
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
            return true;
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

    return false; // unreachable
}

/**
 * Parse the expression but take care of chaining. Calls like f(a,b).g(c,d)
 * will be reformed to g(f(a, b), c, d).
 */
static bool parse_expression_with_chaining(LazyTokenArray &tokens,
                                           std::unique_ptr<AstNode> &node)
{
    std::unique_ptr<AstNode> expr_node;
    bool success = parse_expression(tokens, expr_node);
    Token cur_token = tokens.cur_token();

    if (cur_token.type == TokenType::END)
    {
        node = std::move(expr_node);
        return success;
    }

    if (cur_token == Token(TokenType::DELIM, "."))
    {

        // we might have a chained call here
        tokens.next_token();

        std::unique_ptr<AstNode> parent_node;
        parse_expression(tokens, parent_node);

        auto *parent_call_node = dynamic_cast<AstOpNode *>(parent_node.get());
        if (parent_call_node)
        {
            AstOpNode::OpArg first_arg;
            first_arg.arg_val = std::move(expr_node);
            parent_call_node->args.insert(parent_call_node->args.begin(),
                                          std::move(first_arg));
            node = std::move(parent_node);
            return true;
        }
        throw std::runtime_error("only function calls allowed in chaining");
    }

    throw std::runtime_error("expected end of the expression");
    return false;
}

/**
 * Main parsing function.
 */
bool parse(LazyTokenArray &tokens, std::unique_ptr<AstNode> &root)
{
    return parse_expression_with_chaining(tokens, root);
}

} // namespace aquila::interpreter