#pragma once

#include <format>
#include <lex.hpp>

#include <expected>
#include <optional>
#include <string>
#include <vector>

namespace kl {
namespace ast {

struct Node;

struct Program;

struct TopLevelDeclaration;

struct BindingDeclaration;

struct Node {
  enum class Type {
    Program,
    BindingDeclaration,
    Expression,
  };
};

struct TopLevelDeclaration : Node {};

struct BindingDeclaration : TopLevelDeclaration {};

struct Identifier : Node {
  constexpr Identifier(std::string name) : name{name} {}
  std::string name;
};

struct Type : Node {
  constexpr Type(std::string name) : name{name} {}
  std::string name;
};

struct Program : Node {
  std::vector<std::unique_ptr<TopLevelDeclaration>> declarations;

  std::string to_string() const;
};

struct Binding : Node {};

struct Expression : Binding {};

struct FunctionDefinitionArgument : Node {
  constexpr FunctionDefinitionArgument(std::string identifier, std::string type)
      : identifier{identifier}, type{type} {}
  std::string identifier;
  std::string type;
};

struct Block : Node {};

struct FunctionDefinition : Binding {
  FunctionDefinition(
      std::vector<std::unique_ptr<FunctionDefinitionArgument>>&& arguments,
      std::unique_ptr<Block>&& block)
      : arguments{std::move(arguments)}, block{std::move(block)} {}
  std::vector<std::unique_ptr<FunctionDefinitionArgument>> arguments;
  std::unique_ptr<Block> block;
};

struct ParseError {
  enum class Type {
    UnexpectedToken,
    UnexpectedEOF,
  };

  std::optional<lex::Token> source_token;
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
    case kl::ast::Node::Type::Expression:
      s = "Expression";
      break;
    }
    return std::formatter<std::string>::format(s, ctx);
  }
};
