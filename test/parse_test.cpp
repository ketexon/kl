#include "lex.hpp"
#include <parse.hpp>

#include <ktest/KTest.hpp>

#include <print>

TEST_CASE("Simple Program", ParseSimpleProgram) {
  using namespace kl;

  std::string_view source = "fn main(): Unit {}";
  lex::LexResult lex_result = lex::try_lex(source);

  test.Assert(lex_result.has_value(), "Expected lex to succeed");

  std::vector<lex::Token> tokens = lex_result.value();

  ast::ParseResult parse_result = ast::try_parse(tokens);

  if (!parse_result.has_value()) {
    ast::ParseError error = parse_result.error();
    std::string token_string = "<unknown>";
    if (error.source_token.has_value()) {
      const lex::Token &token = error.source_token.value();
      token_string = source.substr(token.source_span.start.index,
                                   token.source_span.length);
    }
    test.Assert(false, std::format("Parse failed on token {}: {}", token_string,
                                   error.message));
  }

  auto program = std::move(parse_result.value());

  auto expected = std::make_unique<ast::Program>();

  {
    auto main_fn_def = std::make_unique<ast::FunctionDefinition>();
    main_fn_def->identifier = "main";
    main_fn_def->return_type = std::make_unique<ast::Type>("Unit");
    main_fn_def->block = std::make_unique<ast::Block>();

    expected->statements.push_back(std::move(main_fn_def));
  }

  test.cout << ">>>>>> EXPECTED\n" << expected->format() << "\n>>>>>> ACTUAL\n" << program->format() << std::endl;

  test.Assert(*program == *expected, "Expected program to equal expected.");
}
