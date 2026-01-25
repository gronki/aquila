#include "builder.hpp"

namespace aquila::interpreter
{

static std::unique_ptr<OpNode> build_op_node(
    const AstOpNode &ast_op_node, Namespace &ns, const OpDatabase &opdb)
{

    auto op_it = opdb.find(ast_op_node.opname);
    if (op_it == opdb.end())
        throw std::runtime_error(std::string("Operation not found: ") + ast_op_node.opname);
    const OpDbEntry &op_entry = op_it->second;
    auto op = op_entry.factory();

    if (!op)
        throw std::logic_error("Null operation pointer.");

    std::vector<std::unique_ptr<ExecNode>> args;
    args.reserve(ast_op_node.args.size());
    std::vector<std::string> keys;
    keys.reserve(ast_op_node.args.size());
    for (const auto &arg : ast_op_node.args)
    {
        args.push_back(build_exec_tree(arg.arg_val, ns, opdb));
        keys.push_back(arg.key);
    }

    return std::make_unique<OpNode>(std::move(op), std::move(args), keys, ns);
}

std::unique_ptr<ExecNode> build_exec_tree(
    const std::unique_ptr<AstNode> &ast, Namespace &ns, const OpDatabase &opdb)
{
    if (const auto *ast_ref_node = dynamic_cast<const AstRefNode *>(ast.get()))
    {
        return std::make_unique<RefNode>(ast_ref_node->refname, ns);
    }

    if (const auto *ast_val_node = dynamic_cast<const AstValueNode *>(ast.get()))
    {
        return std::make_unique<ValueNode>(ast_val_node->constant->clone(), ns);
    }

    if (const auto *ast_op_node = dynamic_cast<const AstOpNode *>(ast.get()))
    {
        return build_op_node(*ast_op_node, ns, opdb);
    }

    if (const auto *ast_assgn_node = dynamic_cast<const AstAssignmentNode *>(ast.get()))
    {
        return std::make_unique<AssignmentNode>(
            ast_assgn_node->lhs, build_exec_tree(ast_assgn_node->rhs, ns, opdb), ns);
    }

    if (const auto *ast_exp_node = dynamic_cast<const AstExpandNode *>(ast.get()))
    {
        return std::make_unique<WrapperNode>(
            build_exec_tree(ast_exp_node->expandable, ns, opdb),
            ast_exp_node->kind == AstExpandNode::Kind::CONTRACTION
                ? ExecNode::Modifier::CONTRACTION
                : ExecNode::Modifier::EXPANSION,
            ns);
    }

    throw std::logic_error("Unreachable");
    return nullptr;
}

} // namespace aquila::interpreter