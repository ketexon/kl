#include "lex.hpp"
#include <parse.hpp>

#include <ktest/KTest.hpp>

#include <print>

TEST_CASE("Simple Program", ParseSimpleProgram) {
  using namespace kl;

  std::string_view source = "main = fn(): Unit { print(\"hello\") }";
  lex::LexResult lex_result = lex::try_lex(source);

  test.Assert(lex_result.has_value(), "Lex should succeed");

  std::vector<lex::Token> tokens = lex_result.value();

  ast::ParseResult parse_result = ast::try_parse(tokens);

  if (!parse_result.has_value()) {
    ast::ParseError error = parse_result.error();
    if (error.source_token.has_value()) {
      const lex::Token &token = error.source_token.value();
      std::string_view token_string = source.substr(token.source_span.start.index,
                                   token.source_span.end.index -
                                       token.source_span.start.index);
      test.Assert(false, std::format("Parse failed on token {:?} at {}: {}", token_string,
	    token.source_span.start,
				     error));
    }
    test.Assert(false, std::format("Parse failed: {}", error));
  }

  auto program = std::move(parse_result.value());
  test.Assert(program != nullptr, "Program should not be nullptr");
  std::println("{}", *program);
}
