#include <pipeline.hpp>

#include <ktest/KTest.hpp>

TEST_CASE("Basic Pipeline", PipelineBasic) {
  using namespace kl::pipeline;
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

TEST_CASE("Full parse pipeline", PipelineFullParse) {
  using namespace kl;

  std::string_view source = "main = fn(): Unit { print(\"hello\") }";
  lex::LexResult lex_result = lex::try_lex(source);

  test.Assert(lex_result.has_value(), "Lex should succeed");
  std::vector<lex::Token> tokens = lex_result.value();

  ast::ParseResult parse_result = ast::try_parse(tokens);
  test.Assert(parse_result.value() != nullptr, "Program should not be nullptr");
  ast::Node& program = *parse_result.value();

  auto pipelined_result = "main = fn(): Unit { print(\"hello\") }"
    | pipeline::LexStage{}
    | pipeline::ParseStage{};

  test.Assert(pipelined_result.has_value(), "Pipelined result should succeed");
  test.Assert(pipelined_result.value() != nullptr, "Pipelined result shold not be nullptr");

  ast::Node& pipelined_program = *pipelined_result.value();
  test.AssertEq(pipelined_program, program, "Should be same as manual");
}
