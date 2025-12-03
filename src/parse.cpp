#include "lex.hpp"
#include <iostream>
#include <llvm/ADT/APFloat.h>
#include <memory>
#include <parse.hpp>

#include <expected>
#include <format>
#include <print>
#include <span>
#include <stdexcept>
#include <unordered_map>
#include <unordered_set>
#include <utility>

#include <cassert>

#include <llvm/ADT/APInt.h>

namespace kl {
namespace ast {

ParseError::ParseError(std::optional<lex::Token> source_token,
                       std::vector<NodeType> node_stack, Type type,
                       std::string message)
    : Error{Stage::Parse}, source_token{source_token}, node_stack{node_stack},
      type{type}, message{message} {
  error_what = std::format("{}", *this);
}

struct Context {
  std::span<lex::Token> tokens;

  std::vector<NodeType> node_stack;

  void begin_node(NodeType type) { node_stack.push_back(type); }
  void end_node() { node_stack.pop_back(); }

  bool is_end() const {
    return tokens.empty() || tokens[0].type == lex::Token::Type::End;
  }

  lex::Token eat() {
    lex::Token first = peek();
    tokens = tokens.subspan(1);
    return first;
  }

  std::optional<lex::Token> eat_opt() {
    if (is_end())
      return {};
    return eat();
  }

  std::optional<lex::Token> peek_opt() {
    return tokens.size() > 0 ? std::optional{peek()} : std::nullopt;
  }

  void throw_if_end() {
    if (is_end()) {
      throw ParseError{std::nullopt, node_stack,
                       ParseError::Type::UnexpectedEOF,
                       "Expected token, found EOF"};
    }
  }

  lex::Token eat_or_throw(std::string error_message = "") {
    throw_if_end();
    return eat();
  }

  lex::Token peek_or_throw(std::string error_message = "") {
    throw_if_end();
    return peek();
  }

  lex::Token eat_type_or_throw(lex::Token::Type expected_type,
                               std::string error_message = "") {
    auto token_opt = peek_opt();
    if (!token_opt || token_opt->type != expected_type) {
      throw_unexpected(expected_type);
    }
    return eat();
  }

  lex::Token eat_if_in_or_throw(std::unordered_set<lex::Token::Type> set) {
    if (is_in(set)) {
      return eat();
    }
    throw_unexpected(set);
  }

  lex::Token peek() const { return tokens.front(); }

  std::optional<lex::Token> try_peek() const {
    return is_end() ? std::nullopt : std::optional{tokens.front()};
  }

  bool is(lex::Token::Type type) const {
    return !is_end() && peek().type == type;
  }

  bool is_in(const std::unordered_set<lex::Token::Type> &types) const {
    return !is_end() && types.contains(peek().type);
  }

  template <typename T>
  bool is_in(const std::unordered_map<lex::Token::Type, T> &types) const {
    return !is_end() && types.contains(peek().type);
  }

  std::optional<lex::Token> eat_type_opt(lex::Token::Type type) {
    if (is(type)) {
      return eat();
    }
    return {};
  }

  void discard_whitespace(bool include_newlines = false) {
    while (!is_end()) {
      auto type = peek().type;
      if (type == lex::Token::Type::WhiteSpace ||
          include_newlines && type == lex::Token::Type::NewLine) {
        eat();
        continue;
      }
      break;
    }
  }

  void discard_whitespace_and_newlines() { discard_whitespace(true); }

  [[noreturn]] void throw_error(ParseError::Type type, std::string message) {
    throw ParseError{peek_opt(), node_stack, ParseError::Type::UnexpectedToken,
                     message};
  }

  [[noreturn]] void throw_unexpected() {
    auto token_opt = peek_opt();
    std::string found =
        token_opt ? std::format("{}", token_opt.value()) : std::string("EOF");
    throw ParseError{
        token_opt,
        node_stack,
        ParseError::Type::UnexpectedToken,
        std::format("Unexpected token {}", found),
    };
  }

  [[noreturn]] void throw_unexpected(std::string_view expected) {
    auto token_opt = peek_opt();
    std::string found =
        token_opt ? std::format("{}", token_opt.value()) : std::string("EOF");
    throw ParseError{token_opt, node_stack, ParseError::Type::UnexpectedToken,
                     std::format("Expected {}, found {}", expected, found)};
  }

  [[noreturn]] void throw_unexpected(std::optional<lex::Token::Type> expected) {
    if (expected.has_value()) {
      throw_unexpected(std::format("{}", expected.value()));
    } else {
      throw_unexpected();
    }
  }

  template <typename R>
    requires std::ranges::range<R> &&
             std::convertible_to<std::ranges::range_value_t<R>,
                                 lex::Token::Type>
  [[noreturn]] void throw_unexpected(R range) {
    std::string list =
        range | std::views::transform([](const auto &type) {
          return std::format("{}", static_cast<lex::Token::Type>(type));
        }) |
        std::views::join_with(std::string(", ")) |
        std::ranges::to<std::string>();
    throw_unexpected(std::format("{{ {} }}", list));
  }
};

struct NodeParseContext {
  Context &ctx;
  NodeParseContext(Context &ctx, NodeType type) : ctx{ctx} {
    ctx.begin_node(type);
  }

  ~NodeParseContext() { ctx.end_node(); }
};

std::unique_ptr<IdentifierNode> parse_identifier(Context &ctx) {
  std::println("{}", __FUNCTION__);
  NodeParseContext _{ctx, NodeType::Identifier};

  auto token = ctx.eat_type_or_throw(lex::Token::Type::Identifier);
  auto identifier = token.value.value();

  return std::make_unique<IdentifierNode>(identifier);
}

std::unique_ptr<TypeNode> parse_type(Context &ctx) {
  std::println("{}", __FUNCTION__);
  NodeParseContext _{ctx, NodeType::Type};

  auto token = ctx.eat_type_or_throw(lex::Token::Type::Type);
  auto type = token.value.value();

  return std::make_unique<TypeNode>(type);
}

std::unique_ptr<FunctionDefinitionArgumentNode>
parse_function_definition_argument(Context &ctx) {
  std::println("{}", __FUNCTION__);
  NodeParseContext _{ctx, NodeType::FunctionDefinitionArgument};

  auto identifier = parse_identifier(ctx);
  ctx.eat_type_or_throw(lex::Token::Type::Colon);

  ctx.discard_whitespace();
  auto type = parse_type(ctx);

  return std::make_unique<FunctionDefinitionArgumentNode>(std::move(identifier),
                                                          std::move(type));
}

static std::unordered_map<lex::Token::Type, BinaryOperator> add_infix_operators{
    {lex::Token::Type::Plus, BinaryOperator::Add},
    {lex::Token::Type::Minus, BinaryOperator::Subtract},
};

static std::unordered_map<lex::Token::Type, BinaryOperator> mul_infix_operators{
    {lex::Token::Type::Star, BinaryOperator::Multiply},
    {lex::Token::Type::ForwardSlash, BinaryOperator::Divide},
};

static std::unordered_map<lex::Token::Type, UnaryOperator> prefix_operator_map{
    {lex::Token::Type::Minus, UnaryOperator::Negative},
};

std::unique_ptr<ExpressionNode> parse_expression(Context &ctx);

std::unique_ptr<ExpressionNode> parse_integer(Context &ctx) {
  std::println("{}", __FUNCTION__);
  auto token = ctx.eat();

  std::uint64_t value_ull;
  try {
    value_ull = std::stoull(token.value.value());
  } catch (const std::invalid_argument &e) {
    throw ParseError{
        token,
        ctx.node_stack,
        ParseError::Type::InvalidInteger,
        std::format("stoull failed", e.what()),
    };
  } catch (const std::out_of_range &e) {
    throw ParseError{
        token,
        ctx.node_stack,
        ParseError::Type::IntegerOOB,
        std::format("stoull failed", e.what()),
    };
  }

  std::unique_ptr<TypeNode> suffix;
  if (ctx.is(lex::Token::Type::Type)) {
    suffix = parse_type(ctx);
  }

  return std::make_unique<IntegerConstantExpressionNode>(value_ull,
                                                         std::move(suffix));
}

std::unique_ptr<ExpressionNode> parse_float(Context &ctx) {
  std::println("{}", __FUNCTION__);
  auto token = ctx.eat();

  std::unique_ptr<TypeNode> suffix;
  if (ctx.is(lex::Token::Type::Type)) {
    suffix = parse_type(ctx);
  }
  return std::make_unique<FloatConstantExpressionNode>(token.value.value(),
                                                       std::move(suffix));
}

std::unique_ptr<ExpressionNode> parse_factor(Context &ctx) {
  std::println("{}", __FUNCTION__);
  if (ctx.eat_type_opt(lex::Token::Type::LParen)) {
    auto expr = parse_expression(ctx);
    ctx.eat_type_or_throw(lex::Token::Type::RParen);
    return expr;
  }

  if (ctx.is(lex::Token::Type::Identifier)) {
    auto identifier = parse_identifier(ctx);
    return std::make_unique<IdentifierExpressionNode>(std::move(identifier));
  }

  if (ctx.is(lex::Token::Type::Float)) {
    return parse_float(ctx);
  }
  if (ctx.is(lex::Token::Type::Integer)) {
    return parse_integer(ctx);
  }
  if (ctx.is(lex::Token::Type::String)) {
    auto token = ctx.eat();
    return std::make_unique<StringConstantExpressionNode>(token.value.value());
  }
  ctx.throw_unexpected();
}

std::vector<std::unique_ptr<ExpressionNode>>
parse_function_call_args(Context &ctx) {
  std::println("{}", __FUNCTION__);
  std::vector<std::unique_ptr<ExpressionNode>> args;

  bool last = false;
  while (true) {
    ctx.discard_whitespace_and_newlines();
    if (ctx.is(lex::Token::Type::RParen)) {
      break;
    }

    // last argument didn't have a comma after it
    // so this is invalid syntax
    if (last) {
      ctx.throw_unexpected();
    }

    args.push_back(parse_expression(ctx));
    if (!ctx.eat_type_opt(lex::Token::Type::Comma)) {
      last = true;
    }
  }

  return args;
}

std::unique_ptr<ExpressionNode> parse_postfix_expression(Context &ctx) {
  std::println("{}", __FUNCTION__);
  auto factor = parse_factor(ctx);
  if (ctx.eat_type_opt(lex::Token::Type::LParen)) {
    auto args = parse_function_call_args(ctx);
    ctx.eat_type_or_throw(lex::Token::Type::RParen);
    return std::make_unique<FunctionCallExpressionNode>(std::move(factor),
                                                        std::move(args));
  }

  return factor;
}

std::unique_ptr<ExpressionNode> parse_prefix_expression(Context &ctx) {
  std::println("{}", __FUNCTION__);
  if (ctx.is_in(prefix_operator_map)) {
    auto token = ctx.eat();
    auto op = prefix_operator_map.at(token.type);
    auto expr = parse_prefix_expression(ctx);
    return std::make_unique<UnaryOperatorExpressionNode>(op, std::move(expr));
  }
  return parse_postfix_expression(ctx);
}

std::unique_ptr<ExpressionNode> parse_mul_expression(Context &ctx) {
  std::println("{}", __FUNCTION__);
  auto left = parse_prefix_expression(ctx);
  if (ctx.is_in(mul_infix_operators)) {
    auto token = ctx.eat();
    auto op = mul_infix_operators.at(token.type);
    auto right = parse_mul_expression(ctx);
    return std::make_unique<BinaryOperatorExpressionNode>(op, std::move(left),
                                                          std::move(right));
  }
  return left;
}

std::unique_ptr<ExpressionNode> parse_add_expression(Context &ctx) {
  std::println("{}", __FUNCTION__);
  auto left = parse_prefix_expression(ctx);
  if (ctx.is_in(add_infix_operators)) {
    auto token = ctx.eat();
    auto op = add_infix_operators.at(token.type);
    auto right = parse_add_expression(ctx);
    return std::make_unique<BinaryOperatorExpressionNode>(op, std::move(left),
                                                          std::move(right));
  }
  return left;
}

std::unique_ptr<ExpressionNode> parse_expression(Context &ctx) {
  std::println("{}", __FUNCTION__);
  return parse_add_expression(ctx);
}

std::unordered_set<lex::Token::Type> statement_terminators{
    lex::Token::Type::NewLine,
    lex::Token::Type::Semicolon,
};

std::unique_ptr<ReturnStatementNode> parse_return_statement(Context &ctx) {
  std::println("{}", __FUNCTION__);
  ctx.eat();
  ctx.discard_whitespace();
  std::optional<std::unique_ptr<ExpressionNode>> expression;
  if (!ctx.is_in(statement_terminators) && !ctx.is(lex::Token::Type::RBrace)) {
    expression = parse_expression(ctx);
  }
  return std::make_unique<ReturnStatementNode>(std::move(expression));
}

std::unique_ptr<StatementNode> parse_statement(Context &ctx) {
  std::println("{}", __FUNCTION__);
  if (ctx.is(lex::Token::Type::Return)) {
    return parse_return_statement(ctx);
  }
  return std::make_unique<ExpressionStatementNode>(parse_expression(ctx));
}

std::unique_ptr<BlockNode> parse_block(Context &ctx) {
  std::println("{}", ctx.peek());
  std::println("{}", __FUNCTION__);
  NodeParseContext _{ctx, NodeType::Block};

  std::vector<std::unique_ptr<StatementNode>> statements;

  ctx.eat_type_or_throw(lex::Token::Type::LBrace);

  bool last = false;
  while (!ctx.is_end()) {
    ctx.discard_whitespace_and_newlines();
    if (ctx.is(lex::Token::Type::RBrace)) {
      break;
    } else if (last) {
      auto expected = statement_terminators |
                      std::ranges::to<std::vector<lex::Token::Type>>();
      expected.push_back(lex::Token::Type::RBrace);
      ctx.throw_unexpected(expected);
    }
    statements.push_back(parse_statement(ctx));
    // need to terminate a statement either with a newline or a semicolon
    ctx.discard_whitespace();
    if (!ctx.is_in(statement_terminators)) {
      last = true;
    } else {
      ctx.eat();
    }
  }

  ctx.eat_type_or_throw(lex::Token::Type::RBrace);

  return std::make_unique<BlockNode>(std::move(statements));
}

std::unique_ptr<ExternalFunctionImplementationNode>
parse_external_function_implementation(Context &ctx) {
  ctx.eat_type_opt(lex::Token::Type::Extern);
  ctx.discard_whitespace();
  auto symbol_opt = ctx.eat_type_opt(lex::Token::Type::String);
  return std::make_unique<ExternalFunctionImplementationNode>(
      symbol_opt.transform(
          [](const auto &tok) -> std::string { return tok.value.value(); }));
}

std::unique_ptr<InternalFunctionImplementationNode>
parse_internal_function_implementation(Context &ctx) {
  return std::make_unique<InternalFunctionImplementationNode>(parse_block(ctx));
}

std::unique_ptr<FunctionImplementationNode>
parse_function_implementation(Context &ctx) {
  if (ctx.is(lex::Token::Type::Extern)) {
    return parse_external_function_implementation(ctx);
  } else {
    return parse_internal_function_implementation(ctx);
  }
}

std::unique_ptr<FunctionDefinitionNode>
parse_function_definition(Context &ctx) {
  std::println("{}", __FUNCTION__);
  NodeParseContext _{ctx, NodeType::FunctionDefinition};

  ctx.eat_type_or_throw(lex::Token::Type::Fn);
  ctx.eat_type_or_throw(lex::Token::Type::LParen);

  std::vector<std::unique_ptr<FunctionDefinitionArgumentNode>> arguments;

  bool last = false;
  bool is_variadic = false;
  while (true) {
    ctx.discard_whitespace_and_newlines();

    if (ctx.eat_type_opt(lex::Token::Type::RParen)) {
      break;
    } else if (last) {
      throw ParseError{
          ctx.try_peek(),
          ctx.node_stack,
          ParseError::Type::UnexpectedToken,
          "Expected Comma or RParen",
      };
    }

    if (ctx.is(lex::Token::Type::Identifier)) {
      auto arg = parse_function_definition_argument(ctx);
      arguments.push_back(std::move(arg));

      // if followed by comma eat
      // otherwise, we expect a RParen next iter
      if (ctx.is(lex::Token::Type::Comma)) {
        ctx.eat();
      } else {
        last = true;
      }
    } else if (ctx.eat_type_opt(lex::Token::Type::Ellipses)) {
      is_variadic = true;
      ctx.eat_type_opt(lex::Token::Type::Comma);
      last = true;
    } else {
      throw ParseError{
          ctx.try_peek(),
          ctx.node_stack,
          ParseError::Type::UnexpectedToken,
          std::format("Unexpected token while parsing arguments"),
      };
    }
  }

  std::optional<std::unique_ptr<ast::TypeNode>> return_type;
  // return type
  if (ctx.is(lex::Token::Type::Colon)) {
    ctx.eat();
    ctx.discard_whitespace();
    return_type = parse_type(ctx);
  }

  ctx.discard_whitespace();
  return std::make_unique<FunctionDefinitionNode>(FunctionDefinitionNode{
      std::move(arguments),
      std::move(return_type),
      is_variadic,
      parse_function_implementation(ctx),
  });
}

std::unique_ptr<BindingNode> parse_binding(Context &ctx) {
  std::println("{}", __FUNCTION__);
  NodeParseContext _{ctx, NodeType::Binding};

  auto token = ctx.peek_or_throw();
  if (token.type == lex::Token::Type::Fn) {
    return parse_function_definition(ctx);
  }
  std::println(std::cerr, "unimplemented: parse_binding");
  assert(false);
}

std::unique_ptr<BindingDeclarationNode>
parse_binding_declaration(Context &ctx) {
  std::println("{}", __FUNCTION__);
  NodeParseContext _{ctx, NodeType::BindingDeclaration};

  auto identifier = parse_identifier(ctx);
  ctx.discard_whitespace();
  ctx.eat_type_or_throw(lex::Token::Type::Equals);
  ctx.discard_whitespace();
  auto binding = parse_binding(ctx);

  return std::make_unique<BindingDeclarationNode>(std::move(identifier),
                                                  std::move(binding));
}

std::unique_ptr<TopLevelDeclarationNode>
parse_top_level_declaration(Context &ctx) {
  std::println("{}", __FUNCTION__);
  NodeParseContext _{ctx, NodeType::TopLevelDeclaration};

  ctx.discard_whitespace_and_newlines();
  auto binding_declaration = parse_binding_declaration(ctx);
  return std::move(binding_declaration);
}

std::unique_ptr<ProgramNode> parse_program(Context &ctx) {
  std::println("{}", __FUNCTION__);
  NodeParseContext _{ctx, NodeType::Program};

  std::vector<std::unique_ptr<TopLevelDeclarationNode>> declarations;
  ctx.discard_whitespace(true);
  while (!ctx.is_end()) {
    declarations.push_back(parse_top_level_declaration(ctx));
    ctx.discard_whitespace(true);
  }
  return std::make_unique<ProgramNode>(std::move(declarations));
}

ParseResult<> try_parse(std::vector<lex::Token> tokens) {
  Context ctx{
      tokens,
  };

  try {
    return parse_program(ctx);
  } catch (ParseError e) {
    return std::unexpected{e};
  }
}

} // namespace ast
} // namespace kl
