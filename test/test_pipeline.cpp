#include <ktest/KTest.hpp>

#include <pipeline.hpp>

TEST_CASE("Basic Pipeline", PipelineBasic) {
  using namespace kl::pipeline;

  "main = fn() { print(\"hi\") }" | LexStage{} | ParseStage{};
}
