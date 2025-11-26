#pragma once

#include <expected>
#include <format>
#include <optional>
#include <string>
#include <vector>

namespace kl {
namespace lex {

struct SourceLocation {
  std::string filename;
  size_t index = 0;
  size_t line = 1;
  size_t column = 1;

  bool operator==(const SourceLocation &) const = default;
};

struct SourceSpan {
  SourceLocation start;
  SourceLocation end;
};

struct Token {
  using Value = std::optional<std::string>;

  enum class Type {
    // symbols
    WhiteSpace,
    NewLine,

    LParen,
    RParen,

    LBrace,
    RBrace,

    Colon,
    Semicolon,
    Equals,
    Comma,
    Period,
    
    // arithmetic
    Plus,
    Minus,
    Star,
    ForwardSlash,
    Percent,

    // Keywords
    Fn,

    // 
    Type,
    Identifier,

    // Literals
    Integer,
    Float,
    String,

    End,
  };

  Token(Type type, Value value, SourceSpan source_span) : type{type}, value{value}, source_span{source_span} {}
  Token(Type type, SourceSpan source_span) : type{type}, source_span{source_span} {}

  Type type;
  Value value;
  SourceSpan source_span;
};

struct LexError {
  enum class Type {
    UnexpectedSymbol,
    OutOfRange,
    InternalError,
    InvalidIdentifier,
    InvalidEscapeSequence,
    UnterminatedString,
  };

  SourceSpan source_span;
  Type type;
  std::string message;
};

using LexResult = std::expected<std::vector<Token>, LexError>;

LexResult try_lex(std::string_view sv, std::string filename = "");

} // namespace lex
} // namespace kl

std::ostream &operator<<(std::ostream &os,
                         const kl::lex::SourceLocation &source_location);

std::ostream &operator<<(std::ostream &os,
                         const kl::lex::Token &source_location);

std::ostream &operator<<(std::ostream &os,
                         const kl::lex::Token::Type &source_location);

template <>
struct std::formatter<kl::lex::Token::Type> : std::formatter<std::string> {
  auto format(const kl::lex::Token::Type &type,
              std::format_context &ctx) const {
    using Type = kl::lex::Token::Type;
    std::string s;
    switch (type) {
    case Type::LBrace:
      s = "LBrace";
      break;
    case Type::RBrace:
      s = "RBrace";
      break;
    case Type::LParen:
      s = "LParen";
      break;
    case Type::RParen:
      s = "LParen";
      break;
    case Type::WhiteSpace:
      s = "WhiteSpace";
      break;
    case Type::NewLine:
      s = "NewLine";
      break;
    case Type::Colon:
      s = "Colon";
      break;
    case Type::Semicolon:
      s = "Semicolon";
      break;
    case Type::Comma:
      s = "Comma";
      break;
    case Type::Period:
      s = "Period";
      break;
    case Type::Fn:
      s = "Fn";
      break;
    case kl::lex::Token::Type::Type:
      s = std::format("Type");
      break;
    case kl::lex::Token::Type::Identifier:
      s = std::format("Identifier");
      break;
    case kl::lex::Token::Type::Integer:
      s = std::format("Integer");
      break;
    case kl::lex::Token::Type::Float:
      s = std::format("Float");
      break;
    case kl::lex::Token::Type::String:
      s = std::format("String");
      break;
    case kl::lex::Token::Type::End:
      s = std::format("End");
      break;
    case kl::lex::Token::Type::Equals:
      s = std::format("Equals");
      break;
    case kl::lex::Token::Type::Plus:
      s = std::format("Plus");
      break;
    case kl::lex::Token::Type::Minus:
      s = std::format("Minus");
      break;
    case kl::lex::Token::Type::Star:
      s = std::format("Star");
      break;
    case kl::lex::Token::Type::ForwardSlash:
      s = std::format("ForwardSlash");
      break;
    case kl::lex::Token::Type::Percent:
      s = std::format("Percent");
      break;
    }
    return std::formatter<std::string>::format(s, ctx);
  }
};


template <>
struct std::formatter<kl::lex::Token> : std::formatter<std::string> {
  auto format(const kl::lex::Token &token,
              std::format_context &ctx) const {
    return std::formatter<std::string>::format(
	std::format(
	  "{}{}",
	  token.type,
	  token.value.has_value()
	    ? std::format("{{ {} }}", token.value.value())
	    : std::string{}
	)
	, ctx);
  }
};


template <>
struct std::formatter<kl::lex::SourceLocation> : std::formatter<std::string> {
  auto format(const kl::lex::SourceLocation &source_location,
              std::format_context &ctx) const {
    std::string filename = source_location.filename;
    if (filename.empty()) {
      filename = "<unnamed>";
    }
    return std::formatter<std::string>::format(
        std::format("{}:{}:{}", filename, source_location.line,
                    source_location.column),
        ctx);
  }
};


#undef KL_LEX_TOKEN
