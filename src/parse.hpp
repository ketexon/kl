#pragma once

#include <lex.hpp>
#include <types.hpp>

#include <format>
#include <expected>
#include <memory>
#include <optional>
#include <ranges>
#include <string>
#include <vector>

namespace kl {
namespace ast {

struct BindingNode;
struct BindingDeclarationNode;
struct Node;
struct ProgramNode;
struct BlockNode;
struct StatementNode;
struct IdentifierNode;
struct TopLevelDeclarationNode;
struct TypeNode;
struct FunctionDefinitionNode;
struct FunctionDefinitionArgumentNode;

enum class NodeType {
  Binding,
  IntegerConstantExpression,
  FloatConstantExpression,
  StringConstantExpression,
  Block,
  BindingDeclaration,
  Node,
  ExpressionStatement,
  ReturnStatement,
  Program,
  FunctionCallExpression,
  Identifier,
  TopLevelDeclaration,
  FunctionDefinition,
  FunctionDefinitionArgument,

  Type,

  BinaryOperatorExpression,
  IdentifierExpression,
  UnaryOperatorExpression,
};

constexpr const char* node_type_to_string(NodeType t) {
  switch(t) {
  case NodeType::Binding: return "Binding";
  case NodeType::IntegerConstantExpression: return "IntegerConstantExpression";
  case NodeType::FloatConstantExpression: return "FloatConstantExpression";
  case NodeType::StringConstantExpression: return "StringConstantExpression";
  case NodeType::Block: return "Block";
  case NodeType::BindingDeclaration: return "BindingDeclaration";
  case NodeType::Node: return "Node";
  case NodeType::ExpressionStatement: return "ExpressionStatement";
  case NodeType::Program: return "Program";
  case NodeType::FunctionCallExpression: return "FunctionCallExpression";
  case NodeType::Identifier: return "Identifier";
  case NodeType::TopLevelDeclaration: return "TopLevelDeclaration";
  case NodeType::FunctionDefinition: return "FunctionDefinition";
  case NodeType::FunctionDefinitionArgument: return "FunctionDefinitionArgument";
  case NodeType::Type: return "Type";
  case NodeType::BinaryOperatorExpression: return "BinaryOperatorExpression";
  case NodeType::IdentifierExpression: return "IdentifierExpression";
  case NodeType::UnaryOperatorExpression: return "UnaryOperatorExpression";
  case NodeType::ReturnStatement: return "ReturnStatement";
  }
}

struct TypecheckContext {
  std::unique_ptr<types::TypeSystem> type_system;
  std::unique_ptr<types::Scope> global_scope;
  std::unique_ptr<types::Scope> function_scope;
  std::unique_ptr<types::FunctionTypeInfo> function_type_info;

  inline types::Scope* get_current_scope() const {
    if (function_scope != nullptr) {
      return function_scope.get();
    }
    else {
      return global_scope.get();
    }
  }
};

struct TypecheckError {
  enum class Type {

  };
  constexpr static const char* type_to_string(Type t) {
    switch(t) {

    }
    std::unreachable();
  }
  Type type;
  std::string message;
};

struct Node {
  Node(NodeType type);
  virtual std::string format(std::size_t indent = 0) const {
    return make_indent(indent) + "<unknown_node>";
  };

  NodeType type;

  virtual ~Node() = default;
  virtual bool operator==(const Node& other) const = 0;
  virtual void typecheck(ast::TypecheckContext&) {}

protected:
  static inline std::string make_indent(std::size_t size) {
    return std::string(4 * size, ' ');
  }
};

struct TypeNode : Node {
  TypeNode(std::string name);
  std::string format(std::size_t indent) const override;
  bool operator==(const Node& other) const override;
  void typecheck(TypecheckContext&) override;

  std::string name;
  std::unique_ptr<types::TypeInfo> type_info;
};

struct IdentifierNode : Node {
  IdentifierNode(std::string name);
  std::string format(std::size_t indent) const override;
  bool operator==(const Node& other) const override;
  void typecheck(TypecheckContext&) override;

  std::string name;
};

struct TopLevelDeclarationNode : Node {
protected:
  using Node::Node;
};

struct BindingDeclarationNode : TopLevelDeclarationNode {
  BindingDeclarationNode(std::unique_ptr<IdentifierNode> identifier,
                     std::unique_ptr<BindingNode> &&binding);
  ~BindingDeclarationNode();
  std::string format(std::size_t indent) const override;
  bool operator==(const Node& other) const override;
  void typecheck(TypecheckContext&) override;

  std::unique_ptr<IdentifierNode> identifier;
  std::unique_ptr<BindingNode> binding;
};

struct ProgramNode : Node {
  ProgramNode(std::vector<std::unique_ptr<TopLevelDeclarationNode>> &&);
  std::string format(std::size_t indent) const override;
  bool operator==(const Node& other) const override;
  void typecheck(TypecheckContext&) override;

  std::vector<std::unique_ptr<TopLevelDeclarationNode>> declarations;
};

struct BindingNode : Node {
  std::unique_ptr<types::TypeInfo> type_info;
protected:
  using Node::Node;
};

struct StatementNode : Node {
protected:
  using Node::Node;
};

struct ExpressionNode : BindingNode {
protected:
  using BindingNode::BindingNode;
};

struct IdentifierExpressionNode : ExpressionNode {
  IdentifierExpressionNode(std::unique_ptr<IdentifierNode> &&);
  std::string format(std::size_t indent) const override;
  bool operator==(const Node& other) const override;
  void typecheck(TypecheckContext&) override;

  std::unique_ptr<IdentifierNode> identifier;
};

struct ExpressionStatementNode : StatementNode {
  ExpressionStatementNode(std::unique_ptr<ExpressionNode> &&);
  std::string format(std::size_t) const override;
  bool operator==(const Node& other) const override;
  void typecheck(TypecheckContext&) override;

  std::unique_ptr<ExpressionNode> expression;
};

struct ReturnStatementNode : StatementNode {
  ReturnStatementNode(std::optional<std::unique_ptr<ExpressionNode>> &&);
  std::string format(std::size_t) const override;
  bool operator==(const Node& other) const override;
  void typecheck(TypecheckContext&) override;

  std::optional<std::unique_ptr<ExpressionNode>> expression;
};

enum class BinaryOperator {
  Add,
  Subtract,
  Multiply,
  Divide,
};

constexpr const char* binary_operator_to_string(BinaryOperator op) {
  switch(op) {
  case BinaryOperator::Add: return "+";
  case BinaryOperator::Subtract: return "-";
  case BinaryOperator::Multiply: return "*";
  case BinaryOperator::Divide: return "/";
  }
}

struct BinaryOperatorExpressionNode : ExpressionNode {
  BinaryOperatorExpressionNode(BinaryOperator, std::unique_ptr<ExpressionNode> &&,
                           std::unique_ptr<ExpressionNode> &&);
  std::string format(std::size_t) const override;
  bool operator==(const Node& other) const override;
  void typecheck(TypecheckContext&) override;

  BinaryOperator op;
  std::unique_ptr<ExpressionNode> left;
  std::unique_ptr<ExpressionNode> right;
};

enum class UnaryOperator {
  Negative,
  Not,
};

struct UnaryOperatorExpressionNode : ExpressionNode {
  UnaryOperatorExpressionNode(UnaryOperator, std::unique_ptr<ExpressionNode> &&);
  std::string format(std::size_t) const override;
  bool operator==(const Node& other) const override;
  void typecheck(TypecheckContext&) override;

  UnaryOperator op;
  std::unique_ptr<ExpressionNode> expression;
};

struct FunctionCallExpressionNode : ExpressionNode {
  FunctionCallExpressionNode(std::unique_ptr<ExpressionNode> &&,
                         std::vector<std::unique_ptr<ExpressionNode>> &&);
  std::string format(std::size_t) const override;
  bool operator==(const Node& other) const override;
  void typecheck(TypecheckContext&) override;

  std::unique_ptr<ExpressionNode> function;
  std::vector<std::unique_ptr<ExpressionNode>> arguments;
};

struct ConstantExpressionNode : ExpressionNode {
protected:
  using ExpressionNode::ExpressionNode;
};

struct FloatConstantExpressionNode : ConstantExpressionNode {
  FloatConstantExpressionNode(std::string, std::unique_ptr<TypeNode>&&);
  std::string format(std::size_t) const override;
  bool operator==(const Node& other) const override;
  void typecheck(TypecheckContext&) override;

  std::string value;
  std::unique_ptr<TypeNode> suffix;
};

struct IntegerConstantExpressionNode : ConstantExpressionNode {
  IntegerConstantExpressionNode(uint64_t, std::unique_ptr<TypeNode>&&);
  std::string format(std::size_t) const override;
  bool operator==(const Node& other) const override;
  void typecheck(TypecheckContext&) override;

  inline types::IntegerTypeInfo* get_integer_type() const {
    return static_cast<types::IntegerTypeInfo*>(type_info.get());
  }

  std::uint64_t value;
  std::unique_ptr<TypeNode> suffix;
};

struct StringConstantExpressionNode : ConstantExpressionNode {
  StringConstantExpressionNode(std::string);
  std::string format(std::size_t) const override;
  bool operator==(const Node& other) const override;
  void typecheck(TypecheckContext&) override;

  std::string value;
};

struct FunctionDefinitionArgumentNode : Node {
  FunctionDefinitionArgumentNode(std::unique_ptr<IdentifierNode> &&identifier,
                             std::unique_ptr<ast::TypeNode> &&type);
  std::string format(std::size_t indent) const override;
  bool operator==(const Node& other) const override;
  void typecheck(TypecheckContext&) override;

  std::unique_ptr<IdentifierNode> identifier;
  std::unique_ptr<ast::TypeNode> argument_type;
};

struct BlockNode : Node {
  BlockNode(std::vector<std::unique_ptr<StatementNode>> &&statements);
  std::string format(std::size_t) const override;
  bool operator==(const Node& other) const override;
  void typecheck(TypecheckContext&) override;

  std::vector<std::unique_ptr<StatementNode>> statements;
};

struct FunctionDefinitionNode : BindingNode {
  FunctionDefinitionNode(
      std::vector<std::unique_ptr<FunctionDefinitionArgumentNode>> &&arguments,
      std::optional<std::unique_ptr<ast::TypeNode>> &&return_type,
      std::unique_ptr<BlockNode> &&block);
  std::string format(std::size_t) const override;
  bool operator==(const Node& other) const override;
  void typecheck(TypecheckContext&) override;

  std::vector<std::unique_ptr<FunctionDefinitionArgumentNode>> arguments;
  std::optional<std::unique_ptr<ast::TypeNode>> return_type;
  bool is_variadic = false;
  std::unique_ptr<BlockNode> block;
};

struct ParseError {
  enum class Type {
    UnexpectedToken,
    UnexpectedEOF,
    InvalidFloatSuffix,
    InvalidIntegerSuffix,
    IntegerOOB,
    InvalidInteger,
  };

  std::optional<lex::Token> source_token;
  std::vector<NodeType> node_stack;
  Type type;
  std::string message;
};

template <typename T = ProgramNode>
using ParseResult = std::expected<std::unique_ptr<T>, ParseError>;
ParseResult<> try_parse(std::vector<lex::Token> tokens);

using TypecheckResult = std::expected<std::unique_ptr<ast::ProgramNode>, TypecheckError>;
TypecheckResult try_typecheck(std::unique_ptr<ast::ProgramNode>&&);

} // namespace ast
} // namespace kl

template <>
struct std::formatter<kl::ast::NodeType> : std::formatter<std::string> {
  auto format(const kl::ast::NodeType &type, std::format_context &ctx) const {
    return std::formatter<std::string>::format(kl::ast::node_type_to_string(type), ctx);
  }
};

template <>
struct std::formatter<kl::ast::BinaryOperator> : std::formatter<std::string> {
  auto format(kl::ast::BinaryOperator op, std::format_context &ctx) const {
    return std::formatter<std::string>::format(kl::ast::binary_operator_to_string(op), ctx);
  }
};

template <>
struct std::formatter<kl::ast::ParseError> : std::formatter<std::string> {
  auto format(const kl::ast::ParseError &error,
              std::format_context &ctx) const {
    return std::formatter<std::string>::format(
        std::format("{}\nParse stack: {}", error.message,
                    error.node_stack |
                        std::views::transform([](const auto &type) {
                          return std::format("{}", type);
                        })),
        ctx);
  }
};

template <>
struct std::formatter<kl::ast::ProgramNode> : std::formatter<std::string> {
  auto format(const kl::ast::ProgramNode &program, std::format_context &ctx) const {
    return std::formatter<std::string>::format(
        std::format("{}", program.format(0)), ctx);
  }
};

template <>
struct std::formatter<kl::ast::TypecheckError::Type> : std::formatter<std::string> {
  auto format(kl::ast::TypecheckError::Type type,
              std::format_context &ctx) const {
    return std::formatter<std::string>::format(
	kl::ast::TypecheckError::type_to_string(type),
        ctx);
  }
};

template <>
struct std::formatter<kl::ast::TypecheckError> : std::formatter<std::string> {
  auto format(const kl::ast::TypecheckError &error,
              std::format_context &ctx) const {
    return std::formatter<std::string>::format(
	std::format("TypecheckError({}) {}", error.type, error.message),
        ctx);
  }
};

std::ostream& operator<<(std::ostream&, const kl::ast::Node& node);
