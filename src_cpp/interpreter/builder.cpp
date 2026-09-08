#include "builder.hpp"

namespace aquila::interpreter
{

static std::unique_ptr<ExecNode> build_op_node(
    const AstOpNode &ast_op_node, const OpDatabase &opdb)
{

    std::vector<std::unique_ptr<ExecNode>> args;
    args.reserve(ast_op_node.args.size());
    std::vector<std::string> keys;
    keys.reserve(ast_op_node.args.size());
    for (const auto &arg : ast_op_node.args)
    {
        args.push_back(build_exec_tree(arg.arg_val, opdb));
        keys.push_back(arg.key);
    }

    if (ast_op_node.opname == "as")
    {
        if (ast_op_node.args.size() < 2)
            throw std::runtime_error("As operation must have at least one identifier, "
                                     "for example f() % as x");
        std::vector<std::string> idents;
        for (std::size_t i = 1; i < ast_op_node.args.size(); i++)
        {
            auto label = ast_op_node.args[i].arg_val->get_ident();
            if (!label)
                throw std::runtime_error("Incorrect argument to \"as\".");
            idents.push_back(*label);
        }
        return std::make_unique<InlineAssignmentNode>(
            build_exec_tree(ast_op_node.args[0].arg_val, opdb), idents);
    }

    auto op_it = opdb.find(ast_op_node.opname);
    if (op_it == opdb.end())
        throw std::runtime_error(std::string("Operation not found: ") + ast_op_node.opname);
    const OpDbEntry &op_entry = op_it->second;
    auto op = op_entry.factory();

    if (!op)
        throw std::logic_error("Null operation pointer.");

    return std::make_unique<OpNode>(std::move(op), std::move(args), keys);
}

std::unique_ptr<ExecNode> build_exec_tree(
    const std::unique_ptr<AstNode> &ast, const OpDatabase &opdb)
{
    if (const auto *ast_ref_node = dynamic_cast<const AstRefNode *>(ast.get()))
    {
        if (ast_ref_node->refname == "true")
            return std::make_unique<ValueNode>(std::make_unique<BoolValue>(true));
        if (ast_ref_node->refname == "false")
            return std::make_unique<ValueNode>(std::make_unique<BoolValue>(false));
        return std::make_unique<RefNode>(ast_ref_node->refname);
    }

    if (const auto *ast_val_node = dynamic_cast<const AstValueNode *>(ast.get()))
    {
        return std::make_unique<ValueNode>(ast_val_node->constant->clone());
    }

    if (const auto *ast_op_node = dynamic_cast<const AstOpNode *>(ast.get()))
    {
        return build_op_node(*ast_op_node, opdb);
    }

    if (const auto *ast_assgn_node = dynamic_cast<const AstAssignmentNode *>(ast.get()))
    {
        return std::make_unique<AssignmentNode>(
            ast_assgn_node->lhs, build_exec_tree(ast_assgn_node->rhs, opdb));
    }

    throw std::logic_error("Unreachable");
    return nullptr;
}

} // namespace aquila::interpreter
