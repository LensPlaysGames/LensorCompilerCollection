#ifndef LANGUAGE_C_AST_HH
#define LANGUAGE_C_AST_HH

#include <language_c/translation_unit.hh>
#include <language_c/type.hh>

#include <lcc/location.hh>
#include <lcc/stringmap.hh>

#include <fmt/base.h>
#include <fmt/format.h>

#include <string>
#include <string_view>
#include <unordered_map>
#include <vector>

namespace lcc::language_c {

enum struct TokenKind : unsigned int;

struct Scope {
    Scope* parent{};
    std::unordered_map<std::string_view, Declaration*> declarations{};

    Scope() {};

    // Enable Copying
    Scope(const Scope&) = default;
    Scope& operator=(const Scope&) = default;

    // Enable Moving
    Scope(Scope&& other) noexcept = default;
    Scope& operator=(Scope&& other) noexcept = default;

    // Tracked Allocation Only
    void* operator new(size_t) = delete;
    [[nodiscard]]
    void* operator new(size_t size, TranslationUnit& tu) {
        auto ptr = ::operator new(size);
        tu.allocated_scopes.push_back(static_cast<Scope*>(ptr));
        return ptr;
    };
};

enum class NodeKind {
    Invalid,
    Group,
    Block,
    NameReference,
    Declaration,
    ArrayLiteral,
    IntegerLiteral,
    Return,
    UnaryOperation,
    BinaryOperation,
    Call,
    Count
};

struct Node {
    NodeKind _kind{NodeKind::Invalid};
    Location _location{};

public:
    Node(NodeKind kind, Location location)
        : _kind(kind)
        , _location(location) {}

    // Dispatch based on derived class
    virtual ~Node() = default;

    // Disable Copying
    Node(const Node&) = delete;
    Node& operator=(const Node&) = delete;

    // Disable Moving
    Node(Node&&) = delete;
    Node& operator=(Node&&) = delete;

    // Tracked Allocation Only
    void* operator new(size_t) = delete;
    [[nodiscard]]
    void* operator new(size_t size, TranslationUnit& tu) {
        auto ptr = ::operator new(size);
        tu.allocated_nodes.push_back(static_cast<const Node*>(ptr));
        return ptr;
    };

    auto kind() const { return _kind; }
    auto location() const { return _location; }
    auto location() -> Location& { return _location; }

    // Given "  _return 69_  ", where the underscores delineate the node's
    // location, return a location like "  return 69_ _ ".
    auto get_past_location() -> Location;

    // Turn a list of nodes into a single node, or nullptr if the list was
    // empty.
    static auto MaybeToGroup(
        TranslationUnit& tu,
        std::vector<Node*> nodes
    ) -> Node*;

    auto name() const -> std::string_view;
    auto children() const -> std::vector<Node*>;
    // TODO: std::reference_wrapper???
    auto children_ref() const -> std::vector<Node**>;

#ifdef LCC_LANGTEST
    constexpr auto langtest_name() const -> std::string_view {
        return name();
    }
    constexpr auto langtest_children() const -> std::vector<Node*> {
        return children();
    }
#endif
};

struct Group : public Node {
    std::vector<Node*> _constituents;

public:
    Group(std::vector<Node*> constituents, Location location)
        : Node(NodeKind::Group, location)
        , _constituents(std::move(constituents)) {}

    auto constituents() const { return _constituents; }
};

struct Block : public Node {
    std::vector<Node*> _constituents;

public:
    Block(std::vector<Node*> constituents, Location location)
        : Node(NodeKind::Block, location)
        , _constituents(std::move(constituents)) {}

    auto constituents() const { return _constituents; }
};

struct NameReference : public Node {
    std::string _name;
    Scope* _within_scope;

public:
    NameReference(std::string name, Scope* within_scope, Location location)
        : Node(NodeKind::NameReference, location)
        , _name(std::move(name))
        , _within_scope(within_scope) {}

    auto name() const { return _name; }
    auto within_scope() const { return _within_scope; }
};

struct IntegerLiteral : public Node {
    size_t _value;

public:
    IntegerLiteral(size_t value, Location location)
        : Node(NodeKind::IntegerLiteral, location)
        , _value(value) {}

    auto value() const { return _value; }
};

struct ArrayLiteral : public Node {
    std::vector<Node*> _elements;

    // For IRGen
    Type* _element_type{nullptr};

public:
    ArrayLiteral(std::vector<Node*> elements, Location location)
        : Node(NodeKind::ArrayLiteral, location)
        , _elements(std::move(elements)) {}

    auto elements() const { return _elements; }
};

struct UnaryOperation : public Node {
    TokenKind _operator;
    Node* _operand;

    // By Sema, for IRGen
    Type* _operand_type{nullptr};

public:
    UnaryOperation(TokenKind operator_, Node* operand, Location location)
        : Node(NodeKind::UnaryOperation, location)
        , _operator(operator_)
        , _operand(operand) {}

    auto unary_operator() const { return _operator; }
    auto operand() const { return _operand; }
    auto& operand() { return _operand; }
};

struct BinaryOperation : public Node {
    TokenKind _operator;
    Node* _lhs;
    Node* _rhs;

public:
    BinaryOperation(TokenKind operator_, Node* lhs, Node* rhs, Location location)
        : Node(NodeKind::BinaryOperation, location)
        , _operator(operator_)
        , _lhs(lhs)
        , _rhs(rhs) {}

    auto binary_operator() const { return _operator; }
    auto lhs() const { return _lhs; }
    auto rhs() const { return _rhs; }
};

struct Return : public Node {
    Node* _expression;

public:
    Return(Node* expression, Location location)
        : Node(NodeKind::Return, location)
        , _expression(expression) {}

    auto expression() const { return _expression; }
};

struct Call : public Node {
    Node* _callee;
    std::vector<Node*> _arguments;

public:
    Call(Node* callee, std::vector<Node*> arguments, Location location)
        : Node(NodeKind::Call, location)
        , _callee(callee)
        , _arguments(arguments) {}

    auto callee() const { return _callee; }
    auto arguments() const { return _arguments; }
};

struct Declaration : public Node {
    Type* _type{};
    std::string _name{};
    Scope* _encapsulating_scope{};
    Node* _initialising_expression{};

public:
    Declaration(
        Type* type,
        std::string owned_name,
        Scope* encapsulating_scope,
        Node* initialising_expression,
        Location location
    )
        : Node(NodeKind::Declaration, location)
        , _type(type)
        , _name(std::string(owned_name))
        , _encapsulating_scope(encapsulating_scope)
        , _initialising_expression(initialising_expression) {
        // Declare itself in the given scope
        encapsulating_scope->declarations.emplace(_name, this);
    };

    auto name() const -> std::string_view {
        return _name;
    }
    auto type() const { return _type; }
    auto scope() const { return _encapsulating_scope; }
    auto initialising_expression() const { return _initialising_expression; }
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

    auto indent(format_context::iterator out, size_t depth) const
        -> format_context::iterator;

    auto tag(format_context::iterator out, size_t depth, std::string_view tag) const
        -> format_context::iterator;

    auto format(const lcc::language_c::Node&, format_context& ctx) const
        -> format_context::iterator;
};

#endif /* LANGUAGE_C_AST_HH */
