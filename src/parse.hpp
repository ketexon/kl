#pragma once

#include <format>
#include <lex.hpp>

#include <expected>
#include <memory>
#include <optional>
#include <ranges>
#include <string>
#include <vector>

namespace kl {
namespace ast {

struct Binding;
struct BindingDeclaration;
struct Node;
struct Program;
struct Block;
struct Statement;
struct Identifier;
struct TopLevelDeclaration;
struct Type;
struct FunctionDefinition;
struct FunctionDefinitionArgument;

struct Node {
  enum class Type {
    Binding,
    IntegerConstantExpression,
    FloatConstantExpression,
    StringConstantExpression,
    Block,
    BindingDeclaration,
    Node,
    ExpressionStatement,
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

  Node(Type type);
  virtual std::string format(std::size_t indent = 0) const {
    return make_indent(indent) + "<unknown_node>";
  };

  Type type;

  virtual ~Node() = default;
  virtual bool operator==(const Node& other) const = 0;

protected:
  static inline std::string make_indent(std::size_t size) {
    return std::string(4 * size, ' ');
  }
};

enum class BuiltinType {
  Unit,
};

struct Type : Node {
  Type(std::string name);
  std::string format(std::size_t indent) const override;
  bool operator==(const Node& other) const override;

  static std::optional<BuiltinType> get_builtin_type(const std::string&);
  std::variant<BuiltinType, std::string> name;

  template<typename T>
  constexpr auto get() const {
    return std::get<T>(name);
  }

  constexpr bool is_builtin() const {
    return std::holds_alternative<BuiltinType>(name);
  }

  constexpr bool is_builtin(BuiltinType type) const {
    return is_builtin() && get<BuiltinType>() == type;
  }
};

struct Identifier : Node {
  Identifier(std::string name);
  std::string format(std::size_t indent) const override;
  bool operator==(const Node& other) const override;

  std::string name;
};

struct TopLevelDeclaration : Node {
protected:
  using Node::Node;
};

struct BindingDeclaration : TopLevelDeclaration {
  BindingDeclaration(std::unique_ptr<Identifier> identifier,
                     std::unique_ptr<Binding> &&binding);
  ~BindingDeclaration();
  std::string format(std::size_t indent) const override;
  bool operator==(const Node& other) const override;

  std::unique_ptr<Identifier> identifier;
  std::unique_ptr<Binding> binding;
};

struct Program : Node {
  Program(std::vector<std::unique_ptr<TopLevelDeclaration>> &&);
  std::string format(std::size_t indent) const override;
  bool operator==(const Node& other) const override;

  std::vector<std::unique_ptr<TopLevelDeclaration>> declarations;
};

struct Binding : Node {
protected:
  using Node::Node;
};

struct Statement : Node {
protected:
  using Node::Node;
};

struct Expression : Binding {
protected:
  using Binding::Binding;
};

struct IdentifierExpression : Expression {
  IdentifierExpression(std::unique_ptr<Identifier> &&);
  std::string format(std::size_t indent) const override;
  bool operator==(const Node& other) const override;

  std::unique_ptr<Identifier> identifier;
};

struct ExpressionStatement : Statement {
  ExpressionStatement(std::unique_ptr<Expression> &&);
  std::string format(std::size_t) const override;
  bool operator==(const Node& other) const override;

  std::unique_ptr<Expression> expression;
};

enum class BinaryOperator {
  Add,
  Subtract,
  Multiply,
  Divide,
};

struct BinaryOperatorExpression : Expression {
  BinaryOperatorExpression(BinaryOperator, std::unique_ptr<Expression> &&,
                           std::unique_ptr<Expression> &&);

  std::string format(std::size_t) const override;
  bool operator==(const Node& other) const override;

  BinaryOperator op;
  std::unique_ptr<Expression> left;
  std::unique_ptr<Expression> right;
};

enum class UnaryOperator {
  Negative,
  Not,
};

struct UnaryOperatorExpression : Expression {
  UnaryOperatorExpression(UnaryOperator, std::unique_ptr<Expression> &&);
  std::string format(std::size_t) const override;
  bool operator==(const Node& other) const override;

  UnaryOperator op;
  std::unique_ptr<Expression> expression;
};

struct FunctionCallExpression : Expression {
  FunctionCallExpression(std::unique_ptr<Expression> &&,
                         std::vector<std::unique_ptr<Expression>> &&);
  std::string format(std::size_t) const override;
  bool operator==(const Node& other) const override;

  std::unique_ptr<Expression> function;
  std::vector<std::unique_ptr<Expression>> arguments;
};

struct ConstantExpression : Expression {
protected:
  using Expression::Expression;
};

struct FloatConstantExpression : ConstantExpression {
  FloatConstantExpression(std::string);
  std::string format(std::size_t) const override;
  bool operator==(const Node& other) const override;

  std::string value;
};

struct IntegerConstantExpression : ConstantExpression {
  IntegerConstantExpression(std::string);
  std::string format(std::size_t) const override;
  bool operator==(const Node& other) const override;

  std::string value;
};

struct StringConstantExpression : ConstantExpression {
  StringConstantExpression(std::string);
  std::string format(std::size_t) const override;
  bool operator==(const Node& other) const override;

  std::string value;
};

struct FunctionDefinitionArgument : Node {
  FunctionDefinitionArgument(std::unique_ptr<Identifier> &&identifier,
                             std::unique_ptr<ast::Type> &&type);

  std::unique_ptr<Identifier> identifier;
  std::unique_ptr<ast::Type> argument_type;

  std::string format(std::size_t indent) const override;
  bool operator==(const Node& other) const override;
};

struct Block : Node {
  Block(std::vector<std::unique_ptr<Statement>> &&statements);
  std::string format(std::size_t) const override;
  bool operator==(const Node& other) const override;

  std::vector<std::unique_ptr<Statement>> statements;
};

struct FunctionDefinition : Binding {
  FunctionDefinition(
      std::vector<std::unique_ptr<FunctionDefinitionArgument>> &&arguments,
      std::optional<std::unique_ptr<ast::Type>> &&return_type,
      std::unique_ptr<Block> &&block);
  std::string format(std::size_t) const override;
  bool operator==(const Node& other) const override;

  std::vector<std::unique_ptr<FunctionDefinitionArgument>> arguments;
  std::optional<std::unique_ptr<ast::Type>> return_type;
  std::unique_ptr<Block> block;
};

struct ParseError {
  enum class Type {
    UnexpectedToken,
    UnexpectedEOF,
  };

  std::optional<lex::Token> source_token;
  std::vector<Node::Type> node_stack;
  Type type;
  std::string message;
};

template <typename T = Program>
using ParseResult = std::expected<std::unique_ptr<T>, ParseError>;

ParseResult<> try_parse(std::vector<lex::Token> tokens);

} // namespace ast
} // namespace kl

template <>
struct std::formatter<kl::ast::Node::Type> : std::formatter<std::string> {
  auto format(const kl::ast::Node::Type &type, std::format_context &ctx) const {
    using Type = kl::ast::Node::Type;
    std::string s;
    switch (type) {
    case kl::ast::Node::Type::Program:
      s = "Program";
      break;
    case kl::ast::Node::Type::BindingDeclaration:
      s = "BindingDeclaration";
      break;
    case kl::ast::Node::Type::Binding:
      s = "Binding";
      break;
    case kl::ast::Node::Type::Node:
      s = "Node";
      break;
    case kl::ast::Node::Type::Identifier:
      s = "Identifier";
      break;
    case kl::ast::Node::Type::TopLevelDeclaration:
      s = "TopLevelDeclaration";
      break;
    case kl::ast::Node::Type::FunctionDefinition:
      s = "FunctionDefinition";
      break;
    case kl::ast::Node::Type::FunctionDefinitionArgument:
      s = "FunctionDefinitionArgument";
      break;
    case kl::ast::Node::Type::Block:
      s = "Block";
      break;
    case kl::ast::Node::Type::Type:
      s = "Type";
      break;
    case kl::ast::Node::Type::IntegerConstantExpression:
      s = "IntegerConstantExpression";
      break;
    case kl::ast::Node::Type::FloatConstantExpression:
      s = "FloatConstantExpression";
      break;
    case kl::ast::Node::Type::StringConstantExpression:
      s = "StringConstantExpression";
      break;
    case kl::ast::Node::Type::ExpressionStatement:
      s = "ExpressionStatement";
      break;
    case kl::ast::Node::Type::FunctionCallExpression:
      s = "FunctionCallExpression";
      break;
    case kl::ast::Node::Type::BinaryOperatorExpression:
      s = "BinaryOperatorExpression";
      break;
    case kl::ast::Node::Type::IdentifierExpression:
      s = "IdentifierExpression";
      break;
    case kl::ast::Node::Type::UnaryOperatorExpression:
      s = "UnaryOperatorExpression";
      break;
    }
    return std::formatter<std::string>::format(s, ctx);
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
struct std::formatter<kl::ast::Program> : std::formatter<std::string> {
  auto format(const kl::ast::Program &program, std::format_context &ctx) const {
    return std::formatter<std::string>::format(
        std::format("{}", program.format(0)), ctx);
  }
};

template <>
struct std::formatter<kl::ast::BuiltinType> : std::formatter<std::string> {
  auto format(const kl::ast::BuiltinType &builtin, std::format_context &ctx) const {
    std::string s;
    switch(builtin) {
    case kl::ast::BuiltinType::Unit:
      s = "Unit";
      break;
    }
    return std::formatter<std::string>::format(s, ctx);
  }
};

std::ostream& operator<<(std::ostream&, const kl::ast::Node& node);
