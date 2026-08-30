#include "interpreter.hpp"

namespace aquila::interpreter
{

std::unique_ptr<ExecNode> build_exectree_from_str(
    const std::string &code, const OpDatabase &opdb)
{
    LazyTokenArray token_array(Tokenizer{code});
    std::unique_ptr<AstNode> root;
    parse(token_array, root);
    if (!root)
        return nullptr;
#ifndef NDEBUG
    std::cout << *root << std::endl;
#endif
    return build_exec_tree(root, opdb);
}

Ptr<Value> AquilaInterpreter::exec(const std::string &command)
{
    Namespace sub_ns{&ns};
    exectree = build_exectree_from_str(command, global_op_db());
    if (!exectree)
        return nullptr;
    exectree->setup();
#ifndef NDEBUG
    std::cout << "::: " << exectree->trace({ns, &cache}) << std::endl;
#endif
    auto result = exectree->yield({ns, &cache});
    ns.merge(std::move(sub_ns));
    return result;
}

} // namespace aquila::interpreter
