#include <laye/parser.hh>

std::unique_ptr<lcc::laye::Module> lcc::laye::Parser::Parse(Context* context, File* file)
{
    auto result = new Module;

    Parser parser{context, file};

    return std::unique_ptr<Module>(result);
}
