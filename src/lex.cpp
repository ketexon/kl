#include "lex.hpp"

#include <array>
#include <cctype>
#include <expected>
#include <iostream>
#include <optional>
#include <print>
#include <sstream>
#include <stdexcept>
#include <string>
#include <string_view>
#include <unordered_map>

namespace kl {
namespace lex {

struct Context {
  Context(std::string_view sv, std::string filename)
    : filename{filename}, sv{sv}, location {
      filename,
	0, // index
	1, // line
	1  //  column
    }
  {}

  std::string_view sv;
  std::string filename;
  SourceLocation location;
  std::vector<Token> tokens;

  SourceLocation token_start_location;

  char peek() const {
    return sv[location.index];
  }

  char eat() {
    char result = sv[location.index];
    location.index += 1;
    location.column += 1;
    if (is_newline(result)) {
      location.line += 1;
      location.column = 1;
    }
    return result;
  }

  char is(char ch) {
    return !is_end() && peek() == ch;
  }

  char is_in(std::string s) {
    return !is_end() && s.contains(peek());
  }

  bool is_newline() const {
    return !is_end() && is_newline(peek());
  }

  static bool is_newline(char ch) {
    return ch == '\n' || ch == '\r';
  }

  bool is_whitespace() const {
    return !is_end() && is_whitespace(peek());
  }

  static bool is_whitespace(char ch) {
    return std::isblank(static_cast<int>(ch));
  }

  bool is_end() const {
    return location.index >= sv.size();
  }

  void begin_token() {
    token_start_location = location;
  }

  size_t token_length() const {
    return location.index - token_start_location.index;
  }

  SourceSpan get_source_span() const {
    return SourceSpan {
      token_start_location,
      location,
    };
  }

  std::string_view get_token_substring() const {
    return sv.substr(
      token_start_location.index,
      location.index - token_start_location.index
    );
  }

  void end_token(Token::Type type, Token::Value value = std::monostate{}) {
    size_t count = location.index - token_start_location.index;
    if (count == 0) {
      return;
    }
    push(Token{
	type,
	value,
	get_source_span(),
    });
  }

  void push(Token&& token) {
    tokens.push_back(std::move(token));
  }

  static bool is_lower(char ch) { return (ch >= 'a' && ch <= 'z'); }
  static bool is_upper(char ch) { return (ch >= 'A' && ch <= 'Z'); }
  static bool is_alpha(char ch) { return is_lower(ch) || is_upper(ch); }

  bool is_numeric() const {
    return !is_end() && is_numeric(peek());
  }
  static bool is_numeric(char ch) { return (ch >= '0' && ch <= '9'); }
  static bool is_alphanumeric(char ch) { return is_alpha(ch) || is_numeric(ch); }


  bool is_valid_identifier_start() const {
    return !is_end() && is_valid_identifier_start(peek());
  }
  static bool is_valid_identifier_start(char ch) { return ch == '_' || is_lower(ch); }

  bool is_valid_type_start() const {
    return !is_end() && is_valid_type_start(peek());
  }
  static bool is_valid_type_start(char ch) { return is_upper(ch); }

  bool is_valid_identifier_continue() const {
    return !is_end() && is_valid_identifier_continue(peek());
  }
  static bool is_valid_identifier_continue(char ch) {
    return ch == '_' || is_lower(ch) || is_numeric(ch);
  }

  bool is_valid_type_continue() const {
    return !is_end() && is_valid_type_continue(peek());
  }
  static bool is_valid_type_continue(char ch) { return is_alphanumeric(ch); }

  bool is_valid_type_or_ident_end() const {
    return !is_end() && is_valid_type_or_ident_end(peek());
  }
  static bool is_valid_type_or_ident_end(char ch) {
    return !(is_valid_type_continue(ch) || is_valid_identifier_continue(ch));
  }
};

using SublexResult = std::expected<bool, LexError>;

SublexResult try_lex_whitespace(Context& ctx) {
  ctx.begin_token();
  while (ctx.is_whitespace()) {
    ctx.eat();
  }
  if (ctx.token_length() == 0) {
    return false;
  }
  ctx.end_token(Token::Type::WhiteSpace);
  return true;
}

SublexResult try_lex_newline(Context& ctx) {
  ctx.begin_token();
  while (ctx.is_newline()) {
    ctx.eat();
  }
  if (ctx.token_length() == 0) {
    return false;
  }
  ctx.end_token(Token::Type::NewLine);
  return true;
}

static std::unordered_map<char, Token::Type> symbol_map{
    {'(', Token::Type::LParen},
    {')', Token::Type::RParen},
    {'{', Token::Type::LBrace},
    {'}', Token::Type::RBrace},
    {':', Token::Type::Colon},
    {',', Token::Type::Comma},
    {'.', Token::Type::Period},
    // arithmetic
    {'=', Token::Type::Equals},
    {'+', Token::Type::Plus},
    {'-', Token::Type::Minus},
    {'*', Token::Type::Star},
    {'/', Token::Type::ForwardSlash},
};

SublexResult try_lex_symbol(Context& ctx) {
  ctx.begin_token();
  char symbol = ctx.peek();
  if (!symbol_map.contains(symbol)) {
    return false;
  }
  ctx.eat();
  ctx.end_token(symbol_map.at(symbol));
  return true;
}

static std::unordered_map<std::string, Token::Type> keyword_map{
    {"fn", Token::Type::Fn},
};

SublexResult try_lex_type_identifier_or_keyword(Context& ctx) {
  ctx.begin_token();
  bool is_type = false;

  if (ctx.is_valid_type_start()) {
    is_type = true;
  } else if (ctx.is_valid_identifier_start()) {
    is_type = false;
  } else {
    return false;
  }

  auto is_valid_continue = [&, is_type]() {
    return is_type ? ctx.is_valid_type_continue() : ctx.is_valid_identifier_continue();
  };

  while (!ctx.is_end()) {
    if (!is_valid_continue()) {
      // an type/identifier cannot "end" with the start to another identifier
      // as that would look like one type/identifier
      if (!ctx.is_valid_type_or_ident_end()) {
	char ch = ctx.eat();
        LexError error{
	  ctx.get_source_span(),
	  LexError::Type::InvalidIdentifier,
	  std::format("Invalid character '{}' in {}", ch,
		      is_type ? "type" : "identifier"),
        };

        return std::unexpected{error};
      }
      break;
    }
    ctx.eat();
  }

  std::string value{ctx.get_token_substring()};
  if (keyword_map.contains(value)) {
    ctx.end_token(keyword_map.at(value));
  } else if (is_type) {
    ctx.end_token(Token::Type::Type, value);
  } else {
    ctx.end_token(Token::Type::Identifier, value);
  }

  return true;
}

SublexResult try_lex_number(Context& ctx) {
  if (!ctx.is_numeric()) {
    return false;
  }

  bool has_decimal = false;
  while (!ctx.is_end()) {
    if (ctx.is_numeric()) {
      ctx.eat();
      continue;
    }

    if (ctx.is('.')) {
      ctx.eat();
      if (has_decimal) {
        LexError error{
	  ctx.get_source_span(),
	  LexError::Type::UnexpectedSymbol,
	  "Multiple dots in number literal",
        };
        return std::unexpected<LexError>{error};
      }
      has_decimal = true;
      continue;
    }

    break;
  }

  std::string value_string{ctx.get_token_substring()};
  try {
    if (has_decimal) {
      long double value = std::stold(value_string.c_str());
      ctx.end_token(Token::Type::Float, value);
    } else {
      unsigned long long value = std::stoull(value_string.c_str());
      ctx.end_token(Token::Type::Integer, value);
    }
  } catch (const std::out_of_range &e) {
    LexError error{ctx.get_source_span(), LexError::Type::OutOfRange,
                   "Number out of range"};
    return std::unexpected{error};
  }

  return true;
}

std::unordered_map<char, char> escape_sequence_map {
  { 't', '\t' },
  { 'r', '\r' },
  { 'n', '\n' },
  { '\\', '\\' },
  { '\'', '\'' },
  { '"', '"' },
  { '?', '?' },
  { '0', '\0' },
};

SublexResult try_lex_string(Context& ctx) {
  ctx.begin_token();
  if (!ctx.is('"')) {
    return false;
  }
  ctx.eat();

  std::stringstream ss;

  bool is_escaping = false;
  bool terminated = false;
  while(!ctx.is_end()) {
    char ch = ctx.eat();
    if (is_escaping) {
      if (escape_sequence_map.contains(ch)) {
	ss << escape_sequence_map.at(ch);
      }
      else {
	return std::unexpected{LexError {
	  ctx.get_source_span(),
	    LexError::Type::InvalidEscapeSequence,
	    std::format("Unknown escape char: {}", ch),
	}};
      }
      is_escaping = false;
      continue;
    }

    if (ch == '\\') {
      is_escaping = true;
      continue;
    }

    if (ch == '"') {
      terminated = true;
      break;
    }

    ss << ch;
  }

  if (!terminated) {
    return std::unexpected{LexError {
      ctx.get_source_span(),
      LexError::Type::UnterminatedString,
      "Unterminated string",
    }};
  }

  ctx.end_token(Token::Type::String, ss.str());
  return true;
}

using Sublexer = SublexResult (*)(Context&);

static auto sublexers = std::to_array<Sublexer>({
    try_lex_whitespace,
    try_lex_newline,
    try_lex_number,
    try_lex_symbol,
    try_lex_type_identifier_or_keyword,
    try_lex_string,
});

LexResult try_lex(std::string_view sv, std::string filename) {
  Context ctx{sv, filename};

  while (!ctx.is_end()) {
    bool found_sublexer = false;
    for (const auto &sublexer : sublexers) {
      SublexResult result = sublexer(ctx);

      if (!result.has_value()) {
        return std::unexpected{result.error()};
      }

      if (!result.value()) {
        continue;
      }

      found_sublexer = true;
    }

    if (found_sublexer) {
      continue;
    }

    // no sublexers matched, return error
    SourceSpan unexpected_span {
      ctx.location,
      ctx.location,
    };
    unexpected_span.end.index += 1;

    LexError error{
        unexpected_span,
        LexError::Type::UnexpectedSymbol,
        std::format("Unrecognized symbol: {:?}", ctx.peek()),
    };

    return std::unexpected{error};
  }

  ctx.push(Token {
      Token::Type::End,
      SourceSpan {
	ctx.location,
	ctx.location,
      }
  });

  return ctx.tokens;
}

} // namespace lex
} // namespace kl

std::ostream &operator<<(std::ostream &os,
                         const kl::lex::SourceLocation &source_location) {
  std::print(os, "{}", source_location);
  return os;
}

std::ostream &operator<<(std::ostream &os, const kl::lex::Token::Type &type) {
  std::print(os, "{}", type);
  return os;
}

std::ostream &operator<<(std::ostream &os, const kl::lex::Token &token) {
  std::print(os, "{}", token);
  return os;
}
