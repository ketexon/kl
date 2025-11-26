#pragma once

#include <format>
#include <lex.hpp>

#include <expected>
#include <memory>
#include <optional>
#include <ranges>
#include <string>
#include <vector>

#include <llvm/ADT/APFloat.h>
#include <llvm/ADT/APInt.h>

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

//

struct Node {
  enum class Type {
    Binding,
    Block,
    BindingDeclaration,
    Node,
    Program,
    Identifier,
    TopLevelDeclaration,
    FunctionDefinition,
    FunctionDefinitionArgument,
    Type,
  };

  Type type;
  virtual std::string format(std::size_t indent = 0) const {
    return make_indent(indent) + "<unknown_node>";
  };

  virtual ~Node() = default;

protected:
  static inline std::string make_indent(std::size_t size) {
    return std::string(4 * size, ' ');
  }
};

struct Type : Node {
  Type(std::string name);
  std::string format(std::size_t indent) const override;

  std::string name;
};

struct Identifier : Node {
  constexpr Identifier(std::string name) : name{name} {}
  std::string format(std::size_t indent) const override;

  std::string name;
};

struct TopLevelDeclaration : Node {};

struct BindingDeclaration : TopLevelDeclaration {
  BindingDeclaration(std::unique_ptr<Identifier> identifier,
                     std::unique_ptr<Binding> &&binding);
  ~BindingDeclaration();
  std::string format(std::size_t indent) const override;

  std::unique_ptr<Identifier> identifier;
  std::unique_ptr<Binding> binding;
};

struct Program : Node {
  Program(std::vector<std::unique_ptr<TopLevelDeclaration>> &&);
  std::string format(std::size_t indent) const override;

  std::vector<std::unique_ptr<TopLevelDeclaration>> declarations;
};

struct Binding : Node {
protected:
  Binding();
};

struct Statement : Node {
protected:
  Statement();
};

struct Expression : Binding {};

struct IdentifierExpression : Expression {
  IdentifierExpression(std::unique_ptr<Identifier>&&);
  std::string format(std::size_t indent) const override;

  std::unique_ptr<Identifier> identifier;
};

struct ExpressionStatement : Statement {
  ExpressionStatement(std::unique_ptr<Expression>&&);
  std::string format(std::size_t) const override;

  std::unique_ptr<Expression> expression;
};

enum class BinaryOperator {
  Add,
  Subtract,
  Multiply,
  Divide,
};

struct BinaryOperatorExpression : Expression {
  BinaryOperatorExpression(BinaryOperator, std::unique_ptr<Expression>&&, std::unique_ptr<Expression>&&);

  std::string format(std::size_t) const;

  BinaryOperator op;
  std::unique_ptr<Expression> left;
  std::unique_ptr<Expression> right;
};

enum class UnaryOperator {
  Negative,
  Not,
};

struct UnaryOperatorExpression : Expression {
  UnaryOperatorExpression(UnaryOperator, std::unique_ptr<Expression>&&);
  std::string format(std::size_t) const;

  UnaryOperator op;
  std::unique_ptr<Expression> expression;
};

struct FunctionCallExpression : Expression {
  FunctionCallExpression(std::unique_ptr<Expression>&&, std::vector<std::unique_ptr<Expression>>&&);
  std::string format(std::size_t) const;

  std::unique_ptr<Expression> function;
  std::vector<std::unique_ptr<Expression>> arguments;
};

struct ConstantExpression : Expression {
protected:
  ConstantExpression();
};

struct FloatConstantExpression : ConstantExpression {
  FloatConstantExpression(std::string);
  std::string format(std::size_t) const;

  llvm::APFloat value;
};

struct IntegerConstantExpression : ConstantExpression {
  IntegerConstantExpression(std::string);
  std::string format(std::size_t) const;

  llvm::APInt value;
};

struct StringConstantExpression : ConstantExpression {
  StringConstantExpression(std::string);
  std::string format(std::size_t) const;

  std::string value;
};

struct FunctionDefinitionArgument : Node {
  FunctionDefinitionArgument(std::unique_ptr<Identifier> &&identifier,
                             std::unique_ptr<ast::Type> &&type);

  std::unique_ptr<Identifier> identifier;
  std::unique_ptr<ast::Type> type;

  std::string format(std::size_t indent) const override;
};

struct Block : Node {
  Block(std::vector<std::unique_ptr<Statement>> &&statements);
  std::string format(std::size_t) const override;

  std::vector<std::unique_ptr<Statement>> statements;
};

struct FunctionDefinition : Binding {
  FunctionDefinition(
      std::vector<std::unique_ptr<FunctionDefinitionArgument>> &&arguments,
      std::optional<std::unique_ptr<ast::Type>> &&return_type,
      std::unique_ptr<Block> &&block);
  std::string format(std::size_t) const override;

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
