#pragma once

#include "builder.hpp"
#include "cache.hpp"
#include "execution.hpp"
#include "namespace.hpp"
#include "operations/sequence_op.hpp"
#include "parser.hpp"
#include "tokenizer.hpp"

namespace aquila::interpreter
{

std::unique_ptr<ExecNode> build_exectree_from_str(
    const std::string &code, const OpDatabase &opdb);

class AquilaInterpreter
{
    Namespace ns;
    CacheSpace cache;
    std::unique_ptr<ExecNode> exectree;

public:
    ValuePtr exec(const std::string &);
};

} // namespace aquila::interpreter
