#include "interpreter.hpp"

namespace aquila::interpreter
{

std::unique_ptr<ExecNode> build_exectree_from_str(
    const std::string &code, Namespace &ns, const OpDatabase &opdb)
{
    Tokenizer tokenizer(code);
    LazyTokenArray token_array(tokenizer);
    std::unique_ptr<AstNode> root;
    parse(token_array, root);
    return build_exec_tree(root, ns, opdb);
}

} // namespace aquila::interpreter