#include "lex.hpp"
#include <memory>
#include <parse.hpp>

#include <expected>
#include <format>
#include <span>
#include <unordered_map>
#include <unordered_set>
#include <utility>

#include <cassert>

namespace kl {
namespace ast {

struct Context {
  std::span<lex::Token> tokens;

  std::vector<Node::Type> node_stack;

  void begin_node(Node::Type type) { node_stack.push_back(type); }
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
    auto token_opt = eat_opt();
    if (!token_opt || token_opt->type != expected_type) {
      throw_unexpected(expected_type);
    }
    return token_opt.value();
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
    std::string list = range | std::views::transform([](const auto &type) {
                       return std::format("{}",
                                          static_cast<lex::Token::Type>(type));
                     }) |
                     std::views::join_with(std::string(", ")) |
                     std::ranges::to<std::string>();
    throw_unexpected(std::format("{{ {} }}", list));
  }
};

struct NodeParseContext {
  Context &ctx;
  NodeParseContext(Context &ctx, Node::Type type) : ctx{ctx} {
    ctx.begin_node(type);
  }

  ~NodeParseContext() { ctx.end_node(); }
};

std::unique_ptr<Identifier> parse_identifier(Context &ctx) {
  NodeParseContext _{ctx, Node::Type::Identifier};

  auto token = ctx.eat_type_or_throw(lex::Token::Type::Identifier);
  auto identifier = token.value.value();

  return std::make_unique<Identifier>(identifier);
}

std::unique_ptr<Type> parse_type(Context &ctx) {
  NodeParseContext _{ctx, Node::Type::Type};

  auto token = ctx.eat_type_or_throw(lex::Token::Type::Type);
  auto type = token.value.value();

  return std::make_unique<Type>(type);
}

std::unique_ptr<FunctionDefinitionArgument>
parse_function_definition_argument(Context &ctx) {
  NodeParseContext _{ctx, Node::Type::FunctionDefinitionArgument};

  auto identifier = parse_identifier(ctx);
  ctx.eat_type_or_throw(lex::Token::Type::Colon);

  ctx.discard_whitespace();
  auto type = parse_type(ctx);

  return std::make_unique<FunctionDefinitionArgument>(std::move(identifier),
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

std::unique_ptr<Expression> parse_expression(Context &ctx);

std::unique_ptr<Expression> parse_factor(Context &ctx) {
  if (ctx.eat_type_opt(lex::Token::Type::LParen)) {
    auto expr = parse_expression(ctx);
    ctx.eat_type_or_throw(lex::Token::Type::RParen);
    return expr;
  }

  if (ctx.is(lex::Token::Type::Identifier)) {
    auto identifier = parse_identifier(ctx);
    return std::make_unique<IdentifierExpression>(std::move(identifier));
  }

  if (ctx.is(lex::Token::Type::Float)) {
    auto token = ctx.eat();
    return std::make_unique<FloatConstantExpression>(token.value.value());
  }
  if (ctx.is(lex::Token::Type::Integer)) {
    auto token = ctx.eat();
    return std::make_unique<IntegerConstantExpression>(token.value.value());
  }
  if (ctx.is(lex::Token::Type::String)) {
    auto token = ctx.eat();
    return std::make_unique<StringConstantExpression>(token.value.value());
  }
  ctx.throw_unexpected();
}

std::vector<std::unique_ptr<Expression>>
parse_function_call_args(Context &ctx) {
  std::vector<std::unique_ptr<Expression>> args;

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

std::unique_ptr<Expression> parse_postfix_expression(Context &ctx) {
  auto factor = parse_factor(ctx);
  if (ctx.eat_type_opt(lex::Token::Type::LParen)) {
    auto args = parse_function_call_args(ctx);
    ctx.eat_type_or_throw(lex::Token::Type::RParen);
    return std::make_unique<FunctionCallExpression>(std::move(factor),
                                                    std::move(args));
  }

  return factor;
}

std::unique_ptr<Expression> parse_prefix_expression(Context &ctx) {
  if (ctx.is_in(prefix_operator_map)) {
    auto token = ctx.eat();
    auto op = prefix_operator_map.at(token.type);
    auto expr = parse_prefix_expression(ctx);
    return std::make_unique<UnaryOperatorExpression>(op, std::move(expr));
  }
  return parse_postfix_expression(ctx);
}

std::unique_ptr<Expression> parse_mul_expression(Context &ctx) {
  auto left = parse_prefix_expression(ctx);
  if (ctx.is_in(mul_infix_operators)) {
    auto token = ctx.eat();
    auto op = mul_infix_operators.at(token.type);
    auto right = parse_mul_expression(ctx);
    return std::make_unique<BinaryOperatorExpression>(op, std::move(left),
                                                      std::move(right));
  }
  return left;
}

std::unique_ptr<Expression> parse_add_expression(Context &ctx) {
  auto left = parse_prefix_expression(ctx);
  if (ctx.is_in(add_infix_operators)) {
    auto token = ctx.eat();
    auto op = add_infix_operators.at(token.type);
    auto right = parse_add_expression(ctx);
    return std::make_unique<BinaryOperatorExpression>(op, std::move(left),
                                                      std::move(right));
  }
  return left;
}

std::unique_ptr<Expression> parse_expression(Context &ctx) {
  return parse_add_expression(ctx);
}

std::unique_ptr<Statement> parse_statement(Context &ctx) {
  return std::make_unique<ExpressionStatement>(parse_expression(ctx));
}

std::unordered_set<lex::Token::Type> statement_terminators{
    lex::Token::Type::NewLine,
    lex::Token::Type::Semicolon,
};

std::unique_ptr<Block> parse_block(Context &ctx) {
  NodeParseContext _{ctx, Node::Type::Block};

  std::vector<std::unique_ptr<Statement>> statements;

  ctx.eat_type_or_throw(lex::Token::Type::LBrace);

  bool last = false;
  while (!ctx.is_end()) {
    ctx.discard_whitespace_and_newlines();
    if (ctx.is(lex::Token::Type::RBrace)) {
      break;
    }
    else if (last) {
      auto expected = statement_terminators | std::ranges::to<std::vector<lex::Token::Type>>();
      expected.push_back(lex::Token::Type::RBrace);
      ctx.throw_unexpected(expected);
    }
    statements.push_back(parse_statement(ctx));
    // need to terminate a statement either with a newline or a semicolon
    ctx.discard_whitespace();
    if (!ctx.is_in(statement_terminators)) {
      last = true;
    }
  }

  ctx.eat_type_or_throw(lex::Token::Type::RBrace);

  return std::make_unique<Block>(std::move(statements));
}

std::unique_ptr<FunctionDefinition> parse_function_definition(Context &ctx) {
  NodeParseContext _{ctx, Node::Type::FunctionDefinition};

  ctx.eat_type_or_throw(lex::Token::Type::Fn);
  ctx.eat_type_or_throw(lex::Token::Type::LParen);

  std::vector<std::unique_ptr<FunctionDefinitionArgument>> arguments;

  bool last = false;
  while (true) {
    ctx.discard_whitespace_and_newlines();

    if (ctx.is(lex::Token::Type::RParen)) {
      ctx.eat();
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
    }
  }

  std::optional<std::unique_ptr<ast::Type>> return_type;
  // return type
  if (ctx.is(lex::Token::Type::Colon)) {
    ctx.eat();
    ctx.discard_whitespace();
    return_type = parse_type(ctx);
  }

  ctx.discard_whitespace();
  auto block = parse_block(ctx);

  return std::make_unique<FunctionDefinition>(FunctionDefinition{
      std::move(arguments),
      std::move(return_type),
      std::move(block),
  });
}

std::unique_ptr<Binding> parse_binding(Context &ctx) {
  NodeParseContext _{ctx, Node::Type::Binding};

  auto token = ctx.peek_or_throw();
  if (token.type == lex::Token::Type::Fn) {
    return parse_function_definition(ctx);
  }
  throw "Not implemented";
}

std::unique_ptr<BindingDeclaration> parse_binding_declaration(Context &ctx) {
  NodeParseContext _{ctx, Node::Type::BindingDeclaration};

  auto identifier = parse_identifier(ctx);
  ctx.discard_whitespace();
  ctx.eat_type_or_throw(lex::Token::Type::Equals);
  ctx.discard_whitespace();
  auto binding = parse_binding(ctx);

  return std::make_unique<BindingDeclaration>(std::move(identifier),
                                              std::move(binding));
}

std::unique_ptr<TopLevelDeclaration> parse_top_level_declaration(Context &ctx) {
  NodeParseContext _{ctx, Node::Type::TopLevelDeclaration};

  ctx.discard_whitespace_and_newlines();
  auto binding_declaration = parse_binding_declaration(ctx);
  return std::move(binding_declaration);
}

std::unique_ptr<Program> parse_program(Context &ctx) {
  NodeParseContext _{ctx, Node::Type::Program};

  std::vector<std::unique_ptr<TopLevelDeclaration>> declarations;
  while (!ctx.is_end()) {
    declarations.push_back(parse_top_level_declaration(ctx));
  }
  return std::make_unique<Program>(std::move(declarations));
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
