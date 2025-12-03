#pragma once

#include "error.hpp"
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

    Ellipses,

    // arithmetic
    Plus,
    Minus,
    Star,
    ForwardSlash,
    Percent,

    // Keywords
    Fn,
    Return,
    Extern,

    // Identifiers
    Type,
    Identifier,

    // Literals
    Integer,
    Float,
    String,

    End,
  };

  constexpr static const char *type_to_string(Type t) {
    switch (t) {
    case Type::WhiteSpace:
      return "WhiteSpace";
    case Type::NewLine:
      return "NewLine";
    case Type::LParen:
      return "LParen";
    case Type::RParen:
      return "RParen";
    case Type::LBrace:
      return "LBrace";
    case Type::RBrace:
      return "RBrace";
    case Type::Colon:
      return "Colon";
    case Type::Semicolon:
      return "Semicolon";
    case Type::Equals:
      return "Equals";
    case Type::Comma:
      return "Comma";
    case Type::Period:
      return "Period";
    case Type::Plus:
      return "Plus";
    case Type::Minus:
      return "Minus";
    case Type::Star:
      return "Star";
    case Type::ForwardSlash:
      return "ForwardSlash";
    case Type::Percent:
      return "Percent";
    case Type::Fn:
      return "Fn";
    case Type::Return:
      return "Return";
    case Type::Extern:
      return "Extern";
    case Type::Type:
      return "Type";
    case Type::Identifier:
      return "Identifier";
    case Type::Integer:
      return "Integer";
    case Type::Float:
      return "Float";
    case Type::String:
      return "String";
    case Type::End:
      return "End";
    case Type::Ellipses:
      return "Ellipses";
    }
  };

  Token(Type type, Value value, SourceSpan source_span)
      : type{type}, value{value}, source_span{source_span} {}
  Token(Type type, SourceSpan source_span)
      : type{type}, source_span{source_span} {}

  Type type;
  Value value;
  SourceSpan source_span;
};

struct LexError : Error {
  enum class Type {
    UnexpectedSymbol,
    OutOfRange,
    InternalError,
    InvalidIdentifier,
    InvalidEscapeSequence,
    UnterminatedString,
  };

  LexError(SourceSpan, Type, std::string);

  SourceSpan source_span;
  Type type;
  std::string message;

  constexpr static const char *type_to_string(Type t) {
    switch (t) {
    case Type::UnexpectedSymbol:
      return "UnexpectedSymbol";
    case Type::OutOfRange:
      return "OutOfRange";
    case Type::InternalError:
      return "InternalError";
    case Type::InvalidIdentifier:
      return "InvalidIdentifier";
    case Type::InvalidEscapeSequence:
      return "InvalidEscapeSequence";
    case Type::UnterminatedString:
      return "UnterminatedString";
    }
  }
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
struct std::formatter<kl::lex::LexError::Type> : std::formatter<std::string> {
  auto format(kl::lex::LexError::Type type, std::format_context &ctx) const {
    return std::formatter<std::string>::format(
        kl::lex::LexError::type_to_string(type), ctx);
  }
};

template <>
struct std::formatter<kl::lex::Token::Type> : std::formatter<std::string> {
  auto format(const kl::lex::Token::Type &type,
              std::format_context &ctx) const {
    return std::formatter<std::string>::format(
        kl::lex::Token::type_to_string(type), ctx);
  }
};

template <>
struct std::formatter<kl::lex::Token> : std::formatter<std::string> {
  auto format(const kl::lex::Token &token, std::format_context &ctx) const {
    return std::formatter<std::string>::format(
        std::format("{}{}", token.type,
                    token.value.has_value()
                        ? std::format("{{ {:?} }}", token.value.value())
                        : std::string{}),
        ctx);
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

template <>
struct std::formatter<kl::lex::LexError> : std::formatter<std::string> {
  auto format(const kl::lex::LexError &err, std::format_context &ctx) const {
    return std::formatter<std::string>::format(
        std::format("LexError {} at {} to {} \"{}\"", err.type,
                    err.source_span.start, err.source_span.end, err.message),
        ctx);
  }
};

#undef KL_LEX_TOKEN
