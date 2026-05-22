#ifndef LANGUAGE_C_AST_HH
#define LANGUAGE_C_AST_HH

#include <language_c/type.hh>

#include <lcc/stringmap.hh>

#include <fmt/base.h>
#include <fmt/format.h>

#include <unordered_map>

namespace lcc::language_c {

struct Declaration;

struct Scope {
    Scope* parent{};
    std::unordered_map<std::string_view, Declaration*> declarations{};
};

enum class NodeKind {
    Invalid,
    Group,
    Block,
    Declaration,
    IntegerLiteral,
    Return,
    Count
};

struct Node {
    NodeKind _kind{NodeKind::Invalid};

public:
    Node(NodeKind kind) : _kind(kind) {}

    auto kind() const { return _kind; }
};

struct Group : public Node {
    std::vector<Node*> _constituents;

public:
    Group(std::vector<Node*> constituents)
        : Node(NodeKind::Group), _constituents(std::move(constituents)) {}

    auto constituents() const { return _constituents; }
};

struct Block : public Node {
    std::vector<Node*> _constituents;

public:
    Block(std::vector<Node*> constituents)
        : Node(NodeKind::Block), _constituents(std::move(constituents)) {}

    auto constituents() const { return _constituents; }
};

struct IntegerLiteral : public Node {
    size_t _value;

public:
    IntegerLiteral(size_t value)
        : Node(NodeKind::IntegerLiteral), _value(value) {}

    auto value() const { return _value; }
};

struct Return : public Node {
    Node* _expression;

public:
    Return(Node* expression = nullptr)
        : Node(NodeKind::Return), _expression(expression) {}

    auto expression() const { return _expression; }
};

struct Declaration : public Node {
    Type* _type{};
    std::string _name{};
    Scope* _encapsulating_scope{};
    Node* _initialising_expression{};

public:
    Declaration(Type* type, std::string owned_name, Scope* encapsulating_scope, Node* initialising_expression)
        : Node(NodeKind::Declaration),
          _type(type),
          _name(std::string(owned_name)),
          _encapsulating_scope(encapsulating_scope),
          _initialising_expression(initialising_expression) {
        // Declare itself in the given scope
        encapsulating_scope->declarations.emplace(_name, this);
    };

    auto name() const -> std::string_view {
        return _name;
    }
    auto type() const { return _type; }
    auto scope() const { return _encapsulating_scope; }
};

struct TranslationUnit {
    StringMap<Node*> trees{};
};

} // namespace lcc::language_c

template <>
struct fmt::formatter<lcc::language_c::Node> {
    static constexpr size_t indent_width = 2;

    int depth_arg_id{-1};

    constexpr auto parse(format_parse_context& ctx) {
        auto it = ctx.begin();

        if (it != ctx.end() and *it == ':')
            ++it;

        if (it != ctx.end() and *it == '{') {
            ++it;
            if (it != ctx.end() and *it == '}') {
                depth_arg_id = ctx.next_arg_id();
                ++it;
            }
        }

        // Must return the iterator pointing to the closing '}' of the main object
        while (it != ctx.end() and *it != '}') {
            ++it;
        }
        return it;
    }

    auto format(const lcc::language_c::Node&, format_context& ctx) const
        -> format_context::iterator;
};

#endif /* LANGUAGE_C_AST_HH */
