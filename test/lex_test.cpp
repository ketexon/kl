#include <algorithm>
#include <lex.hpp>

#include <ktest/KTest.hpp>

#include <ranges>
#include <sstream>
#include <string>

TEST_CASE("Lexing integer", LexInteger) {
  using namespace kl::lex;

  for (size_t i = 0; i < 20; ++i) {
    std::stringstream ss;
    ss << i;

    LexResult result = try_lex(ss.str(), "");

    test.Assert(result.has_value(), "Lex should succeed");

    std::vector<Token> tokens = result.value();
    test.AssertEq(tokens.size(), 2, "Should lex 2 tokens");

    Token token = tokens[0];

    test.AssertEq(token.type, Token::Type::Integer, "Should hold an integer");
    test.Assert(token.value.has_value(), "Should hold value");
    test.AssertEq(token.value.value(), std::to_string(i), "Should be right integer");
  }
}

TEST_CASE("Lexing float", LexFloat) {
  using namespace kl::lex;

  auto test_for = [&](std::string s, long double double_value) {
    LexResult result = try_lex(s, "");

    if (!result.has_value()) {
      test.Assert(false, std::format("Lex should succeed, got error: \"{}\"",
                                     result.error().message));
    }

    std::vector<Token> tokens = result.value();
    test.AssertEq(tokens.size(), 2, "Should lex 2 tokens");

    Token token = tokens[0];

    test.AssertEq(token.type, Token::Type::Float, "Should lex an float");
    test.Assert(token.value.has_value(), "Should hold value");
    long double token_double = std::stold(token.value.value());
    test.AssertEq(token_double, double_value, "Should be right float");
  };

  test_for("1.", 1.0l);
  test_for("1.0", 1.0l);
  test_for("1.00", 1.0l);

  test_for("2.00", 2.0l);
}

TEST_CASE("Lexing symbols", LexSymbols) {
  using namespace kl::lex;

  std::string input = "(){}:";
  LexResult result = try_lex(input, "");

  if (!result.has_value()) {
    test.Assert(false, std::format("Lex should succeed, got error: \"{}\"",
				   result.error().message));
  }

  auto tokens = result.value();
  std::vector<Token::Type> expected_tokens {
    Token::Type::LParen,
    Token::Type::RParen,
    Token::Type::LBrace,
    Token::Type::RBrace,
    Token::Type::Colon,
  };

  for(int i = 0; i < input.size(); ++i) {
    char symbol = input[i];
    auto token = tokens[i];
    auto expected = expected_tokens[i];
    test.AssertEq(token.type, expected, "Should be right symbol");
  }
}

TEST_CASE("Lexing keywords", LexKeyword) {
  using namespace kl::lex;

  auto test_for = [&](std::string s, Token::Type type) {
    LexResult result = try_lex(s, "");

    if (!result.has_value()) {
      test.Assert(false, std::format("Lex should succeed, got error: \"{}\"",
                                     result.error().message));
    }

    std::vector<Token> tokens = result.value();
    test.AssertEq(tokens.size(), 2, "Should lex 2 token");

    Token token = tokens[0];

    test.AssertEq(token.type, type, "Should have right type.");
  };

  test_for("fn", Token::Type::Fn);
}

TEST_CASE("Lexing identifiers and types", LexIdentifierType) {
  using namespace kl::lex;

  auto test_for = [&](std::string s, Token::Type type, std::string expected_value) {
    LexResult result = try_lex(s, "");

    if (!result.has_value()) {
      test.Assert(false, std::format("Lex should succeed, got error: \"{}\"",
                                     result.error().message));
    }

    std::vector<Token> tokens = result.value();
    test.AssertEq(tokens.size(), 2, "Should lex 2 token");

    Token token = tokens[0];
    test.AssertEq(token.type, type, "Should have right type");
    test.Assert(token.value.has_value(), "Should hold value");
    test.AssertEq(token.value.value(), expected_value, "Should hold right string");
  };

  auto test_fail = [&](std::string s, size_t location) {
    LexResult result = try_lex(s, "");

    test.Assert(!result.has_value(), "Should fail");

    LexError error = result.error();
    if (error.type != LexError::Type::InvalidIdentifier) {
      test.Assert(false, "Expected LexError::Type::InvalidIdentifier");
    }
  };

  test_for("Type", Token::Type::Type, "Type");
  test_for("Type2", Token::Type::Type, "Type2");
  test_for("ident", Token::Type::Identifier, "ident");
  test_for("ident2", Token::Type::Identifier, "ident2");
  test_for("_ident", Token::Type::Identifier, "_ident");
  test_for("_ident2", Token::Type::Identifier, "_ident2");
  test_for("_ident_3", Token::Type::Identifier, "_ident_3");

  test_fail("myIdent", 2);
  test_fail("My_Type", 2);
  test_fail("My_", 2);
}

TEST_CASE("Basic lexing", LexBasic) {
  using namespace kl::lex;

  LexResult result = try_lex("fn my_function(arg: Type): () {}");
  if (!result.has_value()) {
    LexError error = result.error();
    test.Assert(false, std::format("Lex should pass. Error: {} at {}",
                                   error.message, error.source_span.start));
  }

  std::vector<Token> tokens = result.value();

  std::vector<std::tuple<Token::Type, Token::Value>> expected {
    { Token::Type::Fn, {}},
    { Token::Type::WhiteSpace, {}},
    { Token::Type::Identifier, "my_function"},
    { Token::Type::LParen, {}},
    { Token::Type::Identifier, "arg"},
    { Token::Type::Colon, {}},
    { Token::Type::WhiteSpace, {}},
    { Token::Type::Type, "Type"},
    { Token::Type::RParen, {}},
    { Token::Type::Colon, {}},
    { Token::Type::WhiteSpace, {}},
    { Token::Type::LParen, {}},
    { Token::Type::RParen, {}},
    { Token::Type::WhiteSpace, {}},
    { Token::Type::LBrace, {}},
    { Token::Type::RBrace, {}},
    { Token::Type::End, {}},
  };

  size_t min_size = std::min(expected.size(), tokens.size());
  for (int i = 0; i < min_size; ++i) {
    auto& tok = tokens[i];
    auto& [type, value] = expected[i];
    test.cout << i << '\n';
    test.AssertEq(tok.type, type, "Should hold right type");
    test.Assert(tok.value.has_value() == value.has_value(), "Should both either hold or not hold value");
    if (tok.value.has_value()) {
      test.AssertEq(tok.value.value(), value.value(), "Should hold same value type");
    }
  }
}

TEST_CASE("All lexing", LexAll) {
  using namespace kl::lex;

  // WhiteSpace,
  // LParen,
  // RParen,
  // LBrace,
  // RBrace,
  // Colon,
  // Comma,
  // Period,
  // Fn,
  // Type,
  // Identifier,
  // Integer,
  // Float,
  // String,
  // End,
  LexResult result = try_lex(" \t\n (\n){}:,.fn Type ident 1 1.0 \"hello\"");
  //                          00 0 000 000011111111112222222222333 33333 3
  //                          01 2 345 678901234567890123456789012 34567 8
  if (!result.has_value()) {
    LexError error = result.error();
    test.Assert(false, std::format("Lex should pass. Error: {} at {}",
                                   error.message, error.source_span.start));
  }

  std::vector<Token> tokens = result.value();

  std::vector<std::tuple<Token::Type, Token::Value>> expected {
    { Token::Type::WhiteSpace, {}},
    { Token::Type::NewLine, {}},
    { Token::Type::WhiteSpace, {}},
    { Token::Type::LParen, {}},
    { Token::Type::NewLine, {}},
    { Token::Type::RParen, {}},
    { Token::Type::LBrace, {}},
    { Token::Type::RBrace, {}},
    { Token::Type::Colon, {}},
    { Token::Type::Comma, {}},
    { Token::Type::Period, {}},
    { Token::Type::Fn, {}},
    { Token::Type::WhiteSpace, {}},
    { Token::Type::Type, "Type"},
    { Token::Type::WhiteSpace, {}},
    { Token::Type::Identifier, "ident"},
    { Token::Type::WhiteSpace, {}},
    { Token::Type::Integer, "1"},
    { Token::Type::WhiteSpace, {}},
    { Token::Type::Float, "1.0"},
    { Token::Type::WhiteSpace, {}},
    { Token::Type::String, "hello"},
    { Token::Type::End, {}},
  };

  size_t min_size = std::min(expected.size(), tokens.size());
  for (int i = 0; i < min_size; ++i) {
    auto& tok = tokens[i];
    auto& [type, value] = expected[i];
    test.cout << i << '\n';
    test.AssertEq(tok.type, type, "Should hold right type");
    test.AssertEq(tok.value.has_value(), value.has_value(), "Should hold same value type");
    if (tok.value.has_value()) {
      test.AssertEq(tok.value.value(), value.value(), "Should hold same value type");
    }
  }
}
