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
    bool arg_idents_only = false;
    parse_flags_t set_arg_idents_only(bool f)
    {
        auto in = *this;
        in.arg_idents_only = f;
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
        node = std::make_unique<AstOpNode>("seq", std::move(args), token.loc);
        return;
    }

    if (token.type != TokenType::IDENT)
    {
        throw std::runtime_error(std::string("unexpected token: ") + token.str());
        return;
    }

    Token after_ident = tokens.next_token();
    bool opening_brace_after_ident = after_ident == Token(TokenType::DELIM, "(");
    if (!opening_brace_after_ident
        && after_ident != Token(TokenType::DELIM, "[")
        /* && after_ident != Token(TokenType::DELIM, EXPAND_DELIM) */
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
        flags.set_allow_open_syntax(false)
            .set_in_open_syntax(!opening_brace_after_ident)
            .set_arg_idents_only(opname == "as"));
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
    bool expect_end = false;
    bool expect_arg = false;
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
                if (expect_arg)
                    throw std::runtime_error(
                        std::string("Argument expected after comma (,) but got: ")
                        + cur_token.str());
                return;
            }
        }
        else if (cur_token == Token(TokenType::DELIM, closing_brace))
        {
            if (expect_arg)
                throw std::runtime_error(
                    std::string("Argument expected after comma (,) but got: ")
                    + cur_token.str());
            tokens.next_token();
            return;
        }
        else if (cur_token.type == TokenType::END)
        {
            throw std::runtime_error("unexpected end of input while parsing argument "
                                     "list");
        }

        if (expect_end)
            throw std::runtime_error(
                std::string("Missing comma (,) before argument: ") + cur_token.value);

        AstOpNode::OpArg arg;

        Token maybe_kv_sep = tokens.peek_token(1);
        /*
        if (cur_token == Token(TokenType::DELIM, EXPAND_DELIM))
        {
            if (flags.arg_idents_only)
                throw std::runtime_error(
                    "expansion not allowed in inline assignment %as...");
            arg.has_key = true;
            arg.key = cur_token.value;
            cur_token = tokens.next_token();
        } else
        */
        // we check if perhaps a keyword argument is given, such as key: val
        if (cur_token.type == TokenType::IDENT
            && maybe_kv_sep == Token(TokenType::DELIM, KWARG_DELIM))
        {
            if (flags.arg_idents_only)
                throw std::runtime_error(
                    "keywords not allowed in inline assignment %as...");
            arg.has_key = true;
            arg.key = std::move(cur_token.value);
            cur_token = tokens.next_token(2);
        }

        std::unique_ptr<AstNode> arg_node;
        parse_expression(tokens, arg_node, flags);
        if (flags.arg_idents_only && !arg_node->is_ident())
            throw std::runtime_error(
                "Only list of simple identifiers (a, b, c, ..) allowed here.");

        arg.arg_val = std::move(arg_node);
        node_args.push_back(std::move(arg));

        cur_token = tokens.cur_token();
        expect_arg = false;
        if (cur_token == Token(TokenType::DELIM, ","))
        {
            tokens.next_token();
            expect_arg = true;
        }
        else
        {
            expect_end = true;
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
        auto cur_token = tokens.next_token();
        /*
        bool expand_chain = cur_token == Token(TokenType::DELIM, EXPAND_DELIM);
        if (expand_chain)
            cur_token = tokens.next_token();
        */

        std::unique_ptr<AstNode> parent_node;
        parse_basic_expression(tokens, parent_node, flags);

        if (auto *parent_call_node = dynamic_cast<AstOpNode *>(parent_node.get()))
        {
            // stadard chaining: X % f(Y) -> F(X, Y)
            AstOpNode::OpArg first_arg;
            first_arg.arg_val = std::move(node);
            /*
            if (expand_chain)
            {
                first_arg.has_key = true;
                first_arg.key = std::string(1, EXPAND_DELIM);
            }
            */
            parent_call_node->args.insert(
                parent_call_node->args.begin(), std::move(first_arg));
            node = std::move(parent_node);
        }
        else
        {
            throw std::runtime_error("incorrect chaining: only "
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
