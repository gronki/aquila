#include "interpreter.hpp"

namespace aquila::interpreter
{

std::unique_ptr<ExecNode> build_exectree_from_str(
    const std::string &code, Namespace &ns, const OpDatabase &opdb)
{
    LazyTokenArray token_array(Tokenizer{code});
    std::unique_ptr<AstNode> root;
    parse(token_array, root);
    if (!root)
        return nullptr;
#ifndef NDEBUG
    std::cout << *root << std::endl;
#endif
    return build_exec_tree(root, ns, opdb);
}

const Value *AquilaInterpreter::exec(const std::string &command)
{
    Namespace sub_ns{&ns};
    exectree = build_exectree_from_str(command, sub_ns, global_op_db());
    if (!exectree)
        return nullptr;
    const Value *result = exectree->yield();
    ns.merge(std::move(sub_ns));
    return result;
}

} // namespace aquila::interpreter
