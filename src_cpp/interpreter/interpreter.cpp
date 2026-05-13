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

} // namespace aquila::interpreter
