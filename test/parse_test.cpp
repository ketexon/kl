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
      std::string_view token_string = source.substr(
          token.source_span.start.index,
          token.source_span.end.index - token.source_span.start.index);
      test.Assert(false,
                  std::format("Parse failed on token {:?} at {}: {}",
                              token_string, token.source_span.start, error));
    }
    test.Assert(false, std::format("Parse failed: {}", error));
  }

  auto program = std::move(parse_result.value());
  test.Assert(program != nullptr, "Program should not be nullptr");

  auto function_call = std::make_unique<ast::FunctionCallExpression>(
      std::make_unique<ast::IdentifierExpression>(
          std::make_unique<ast::Identifier>("print")),
      std::vector<std::unique_ptr<ast::Expression>>{
	
      });

  function_call->arguments.push_back(
      std::make_unique<ast::StringConstantExpression>(
	"hello"
      )
  );

  auto block = std::make_unique<ast::Block>(
      std::vector<std::unique_ptr<ast::Statement>>{});
  block->statements.push_back(
      std::make_unique<ast::ExpressionStatement>(std::move(function_call)));

  auto expected_program = std::make_unique<ast::Program>(
      std::vector<std::unique_ptr<ast::TopLevelDeclaration>>{});

  expected_program->declarations.push_back(std::make_unique<ast::BindingDeclaration>(
      std::make_unique<ast::Identifier>("main"),
      std::make_unique<ast::FunctionDefinition>(
          std::vector<std::unique_ptr<ast::FunctionDefinitionArgument>>{},
          std::optional{std::make_unique<ast::Type>("Unit")},
          std::move(block))));

  test.AssertEq(static_cast<ast::Node&>(*program), *expected_program, "Expected programs to match");
}
