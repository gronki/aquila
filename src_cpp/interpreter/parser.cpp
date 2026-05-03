#include "parser.hpp"
#include "characters.hpp"
#include <stdexcept>

namespace aquila::interpreter
{

struct parse_flags_t
{
    bool allow_open_syntax = true;
    parse_flags_t set_allow_open_syntax(bool f)
    {
        auto in = *this;
        in.allow_open_syntax = f;
        return in;
    }
    bool in_open_syntax = false;
    parse_flags_t set_in_open_syntax(bool f)
    {
        auto in = *this;
        in.in_open_syntax = f;
        return in;
    }
};

/**
 * Definitions of static procedures in this file.
 */
static void parse_basic_expression(
    LazyTokenArray &, std::unique_ptr<AstNode> &, parse_flags_t flags);
static void parse_function_argument_list(
    LazyTokenArray &, char, std::vector<AstOpNode::OpArg> &, parse_flags_t flags);
static void parse_expression(
    LazyTokenArray &, std::unique_ptr<AstNode> &, parse_flags_t flags);

/**
 * Parse an expression (literal, reference, function call) but not including chaining.
 */
static void parse_basic_expression(
    LazyTokenArray &tokens, std::unique_ptr<AstNode> &node, parse_flags_t flags)
{
    Token token = tokens.cur_token();

    if (token.type == TokenType::END)
        return;

    if (token.type == TokenType::NUM_LITERAL)
    {
        node = std::make_unique<AstValueNode>(
            std::make_unique<RealValue>(std::stod(token.value)), token.loc);
        tokens.next_token();
        return;
    }

    if (token.type == TokenType::STR_LITERAL)
    {
        node = std::make_unique<AstValueNode>(
            std::make_unique<StrValue>(token.value), token.loc);
        tokens.next_token();
        return;
    }

    if (token == Token(TokenType::DELIM, '['))
    {
        tokens.next_token();
        std::vector<AstOpNode::OpArg> args;
        parse_function_argument_list(tokens, ']', args, flags.set_allow_open_syntax(false));
        node = std::make_unique<AstOpNode>("array", std::move(args), token.loc);
        return;
    }

    if (token.type != TokenType::IDENT)
    {
        throw std::runtime_error(std::string("unexpected token: ") + token.str());
        return;
    }

    Token after_ident = tokens.next_token();
    bool opening_brace_after_ident = after_ident == Token(TokenType::DELIM, "(");
    if (!opening_brace_after_ident && after_ident != Token(TokenType::DELIM, "[")
        && (after_ident.type == TokenType::DELIM || after_ident.type == TokenType::END))
    {
        node = std::make_unique<AstRefNode>(token.value, token.loc);
        return;
    }

    if (!opening_brace_after_ident && !flags.allow_open_syntax)
        throw std::runtime_error("You must use () for nested operation call");

    if (opening_brace_after_ident)
        tokens.next_token();

    // here we know that we are parsing function argument list

    auto opname = token.value;
    auto loc = token.loc;
    std::vector<AstOpNode::OpArg> args;
    parse_function_argument_list(tokens,
        opening_brace_after_ident ? ')' : ' ',
        args,
        flags.set_allow_open_syntax(false).set_in_open_syntax(!opening_brace_after_ident));
    node = std::make_unique<AstOpNode>(opname, std::move(args), loc);
}

/**
 * Parse function argument list (starting from the first token after the opening brace).
 */
static void parse_function_argument_list(LazyTokenArray &tokens,
    char closing_brace,
    std::vector<AstOpNode::OpArg> &node_args,
    parse_flags_t flags)
{

    while (true)
    {
        Token cur_token = tokens.cur_token();

        if (closing_brace == ' ')
        {
            if (cur_token == Token(TokenType::DELIM, ')')
                || cur_token == Token(TokenType::DELIM, ']')
                || cur_token == Token(TokenType::DELIM, CHAIN_CALL_DELIM)
                || cur_token.type == TokenType::END)
            {
                return;
            }
        }
        else if (cur_token == Token(TokenType::DELIM, closing_brace))
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

        if (cur_token.type == TokenType::IDENT
            && maybe_kv_sep == Token(TokenType::DELIM, KWARG_DELIM))
        {
            arg.has_key = true;
            arg.key = std::move(cur_token.value);
            cur_token = tokens.next_token(2);
        }

        bool expansion = tokens.cur_token() == Token(TokenType::DELIM, EXPAND_DELIM);
        bool contraction = tokens.cur_token() == Token(TokenType::DELIM, CONTRACT_DELIM);

        if (expansion || contraction)
            cur_token = tokens.next_token();

        if (expansion && arg.has_key)
            throw std::runtime_error("Expansion not allowed here.");

        std::unique_ptr<AstNode> arg_node;
        parse_expression(tokens, arg_node, flags);

        if (expansion || contraction)
            arg_node = std::make_unique<AstExpandNode>(std::move(arg_node),
                expansion ? AstExpandNode::Kind::EXPANSION : AstExpandNode::Kind::CONTRACTION,
                cur_token.loc);
        arg.arg_val = std::move(arg_node);
        node_args.push_back(std::move(arg));

        cur_token = tokens.cur_token();

        if (cur_token == Token(TokenType::DELIM, ","))
        {
            tokens.next_token();
        }
    }

    throw std::logic_error("unreachable");
}

/**
 * Parse the expression but take care of chaining. Calls like f(a,b).g(c,d)
 * will be reformed to g(f(a, b), c, d).
 */
static void parse_expression(
    LazyTokenArray &tokens, std::unique_ptr<AstNode> &node, parse_flags_t flags)
{
    parse_basic_expression(tokens, node, flags);

    while (tokens.cur_token() == Token(TokenType::DELIM, CHAIN_CALL_DELIM))
    {
        if (flags.in_open_syntax)
            return;

        // we might have a chained call here
        tokens.next_token();

        auto cur_token = tokens.cur_token();
        bool contract_chain = cur_token == Token(TokenType::DELIM, CONTRACT_DELIM);
        bool expand_chain = cur_token == Token(TokenType::DELIM, EXPAND_DELIM);

        if (contract_chain || expand_chain)
            tokens.next_token();

        std::unique_ptr<AstNode> parent_node;
        parse_basic_expression(tokens, parent_node, flags);

        if (auto *parent_call_node = dynamic_cast<AstOpNode *>(parent_node.get()))
        {
            // stadard chaining: X % f(Y) -> F(X, Y)
            AstOpNode::OpArg first_arg;
            first_arg.arg_val = contract_chain || expand_chain
                ? std::make_unique<AstExpandNode>(std::move(node),
                      contract_chain ? AstExpandNode::Kind::CONTRACTION
                                     : AstExpandNode::Kind::EXPANSION,
                      cur_token.loc)
                : std::move(node);
            parent_call_node->args.insert(
                parent_call_node->args.begin(), std::move(first_arg));
            node = std::move(parent_node);
        }
        else if (auto *parent_call_node = dynamic_cast<AstRefNode *>(parent_node.get()))
        {
            if (contract_chain)
                throw std::runtime_error("& not allowed here");
            // param function: X % Y -> param(X, "Y")
            // used to retrieve some (static) properties from a value
            // for example, an exposure time from FITS header
            std::vector<AstOpNode::OpArg> args(2);
            args[0].arg_val = std::move(node);
            args[1].arg_val = std::make_unique<AstValueNode>(
                std::make_unique<StrValue>(parent_call_node->refname),
                parent_call_node->loc);
            node = std::make_unique<AstOpNode>(
                "param", std::move(args), args[0].arg_val->loc);
        }
        else
        {
            throw std::runtime_error("incorrect chaining: only identifier X%Y or "
                                     "function X%F() allowed");
        }
    }

    return;
}

static void parse_assignment(LazyTokenArray &tokens, std::unique_ptr<AstNode> &node)
{
    Token maybe_lhs = tokens.peek_token(0);
    Token maybe_eq = tokens.peek_token(1);

    if (maybe_lhs.type == TokenType::END)
        return;

    if (maybe_lhs.type == TokenType::IDENT && maybe_eq == Token(TokenType::DELIM, '='))
    {
        std::unique_ptr<AstNode> rhs;
        Token first_rhs = tokens.next_token(2);
        if (first_rhs.type == TokenType::END)
        {
            throw std::runtime_error("Expression expected after =");
        }
        parse_expression(tokens, rhs, {});
        node = std::make_unique<AstAssignmentNode>(
            maybe_lhs.value, std::move(rhs), maybe_lhs.loc);
    }
    else
    {
        parse_expression(tokens, node, {});
    }

    if (tokens.cur_token().type != TokenType::END)
        throw std::runtime_error("end of line expected.");
}

/**
 * Main parsing function.
 */
void parse(LazyTokenArray &tokens, std::unique_ptr<AstNode> &root)
{
    parse_assignment(tokens, root);
}

} // namespace aquila::interpreter
