#include "lex.hpp"
#include <algorithm>
#include <memory>
#include <parse.hpp>

#include <expected>
#include <format>
#include <ranges>
#include <span>
#include <unordered_map>
#include <unordered_set>
#include <utility>
#include <variant>

namespace kl {
namespace ast {

struct Context {
  std::span<lex::Token> tokens;

  bool is_empty() const { return tokens.empty(); }

  lex::Token eat() {
    lex::Token first = peek();
    tokens = tokens.subspan(1);
    return first;
  }

  void throw_if_empty() {
    if (is_empty() || peek().type == lex::Token::Type::End) {
      throw ParseError{std::nullopt, ParseError::Type::UnexpectedEOF,
                       std::format("Expected token, found EOF{}{}",
                                   error_message.empty() ? "" : ": ",
                                   error_message)};
    }
  }

  lex::Token eat_or_throw(std::string error_message = "") {
    throw_if_empty();
    return eat();
  }

  lex::Token peek_or_throw(std::string error_message = "") {
    throw_if_empty();
    return peek();
  }

  lex::Token eat_type_or_throw(lex::Token::Type expected_type,
                               std::string error_message = "") {
    lex::Token token = eat_or_throw(error_message);
    if (token.type != expected_type) {
      throw ParseError{
          token,
          ParseError::Type::UnexpectedToken,
          std::format("Expected {}, got {}{}{}", expected_type, token,
                      error_message.empty() ? "" : ": ", error_message),
      };
    }
    return token;
  }

  lex::Token peek() { return tokens.front(); }
  std::optional<lex::Token> try_peek() {
    return is_empty() ? std::nullopt : std::optional{tokens.front()};
  }

  bool is(lex::Token::Type type) { return !is_empty() && peek().type == type; }

  void discard_whitespace(bool include_newlines = false) {
    while (!is_empty()) {
      auto type = peek().type;
      if (type == lex::Token::Type::WhiteSpace ||
          include_newlines && type == lex::Token::Type::NewLine) {
        continue;
      }
      break;
    }
  }

  void discard_whitespace_and_newlines() { discard_whitespace(true); }
};

static std::unordered_set<lex::Token::Type> infix_operators{
    lex::Token::Type::Plus,    lex::Token::Type::Minus,
    lex::Token::Type::Star,    lex::Token::Type::ForwardSlash,
    lex::Token::Type::Percent,
};

std::unique_ptr<FunctionDefinitionArgument>
parse_function_definition_argument(Context &ctx) {
  auto ident_token = ctx.eat_type_or_throw(lex::Token::Type::Identifier);
  ctx.eat_type_or_throw(lex::Token::Type::Colon);

  ctx.discard_whitespace();
  auto type_token = ctx.eat_type_or_throw(lex::Token::Type::Type);

  return std::make_unique<FunctionDefinitionArgument>(
      FunctionDefinitionArgument(ident_token.get_string_value(),
                                 type_token.get_string_value()));
}

std::unique_ptr<Block> parse_block(Context &ctx) {
  ctx.eat_type_or_throw(lex::Token::Type::LBrace);

  ctx.discard_whitespace_and_newlines();

  ctx.eat_type_or_throw(lex::Token::Type::RBrace);
}

std::unique_ptr<FunctionDefinition> parse_function_definition(Context &ctx) {
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

  std::string return_type = "Unit";
  // return type
  if (ctx.is(lex::Token::Type::Colon)) {
    ctx.eat();
    ctx.discard_whitespace();
    auto type_token = ctx.eat_type_or_throw(lex::Token::Type::Type);
    return_type = type_token.get_string_value();
  }

  ctx.discard_whitespace();
  auto block = parse_block(ctx);

  return std::make_unique<FunctionDefinition>(FunctionDefinition{
      std::move(arguments),
      std::move(block),
  });
}

std::unique_ptr<Binding> parse_binding(Context &ctx) {
  auto token = ctx.peek_or_throw();
  if (token.type == lex::Token::Type::Fn) {
    return parse_function_definition(ctx);
  }
  throw "Not implemented";
}

std::unique_ptr<Identifier> parse_identifier(Context &ctx) {
  auto token = ctx.eat_type_or_throw(lex::Token::Type::Identifier);
  auto identifier = std::get<lex::Token::StringValue>(token.value);

  return std::make_unique<Identifier>(Identifier(identifier));
}

std::unique_ptr<BindingDeclaration> parse_binding_declaration(Context &ctx) {
  auto identifier = parse_identifier(ctx);
  ctx.discard_whitespace();
  ctx.eat_type_or_throw(lex::Token::Type::Equals);
  ctx.discard_whitespace();
  auto binding = parse_binding(ctx);
}

std::unique_ptr<TopLevelDeclaration> parse_top_level_declaration(Context &ctx) {
  ctx.discard_whitespace_and_newlines();
}

std::unique_ptr<Program> parse_program(Context &ctx) {
  auto program = std::make_unique<Program>();
  return program;
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

std::string Program::to_string() const {
  return declarations |
         std::views::transform(
             [](const std::unique_ptr<TopLevelDeclaration> &decl)
                 -> std::string { return ""; }) |
         std::views::join_with(std::string("\n")) |
         std::ranges::to<std::string>();
};

} // namespace ast
} // namespace kl
