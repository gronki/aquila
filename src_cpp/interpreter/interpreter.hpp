#pragma once

#include "builder.hpp"
#include "execution.hpp"
#include "namespace.hpp"
#include "operations/sequence_op.hpp"
#include "parser.hpp"
#include "tokenizer.hpp"

namespace aquila::interpreter
{

std::unique_ptr<ExecNode> build_exectree_from_str(
    const std::string &code, Namespace &ns, const OpDatabase &opdb);

class AquilaInterpreter
{
    Namespace ns;
    std::unique_ptr<ExecNode> exectree;

public:
    const Value *exec(const std::string &);
};

} // namespace aquila::interpreter
