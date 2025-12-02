#include <ktest/KTest.hpp>

#include <pipeline.hpp>
#include <codegen.hpp>

#include <llvm/IR/Module.h>
#include <llvm/Support/raw_ostream.h>

TEST_CASE("BasicCodegen", BasicCodegen) {
  using namespace kl;
  using namespace kl::codegen;
  auto llvm_context = std::make_shared<llvm::LLVMContext>();

  auto result = "main = fn(): Unit { puts(\"hello\") }"
    | pipeline::LexStage{}
    | pipeline::ParseStage{} 
    | pipeline::CodegenStage{llvm_context};

    if (!result.has_value()) {
      test.Assert(std::holds_alternative<CodegenError>(result.error()), "Should be a codegen error");
      auto err = std::get<CodegenError>(result.error());
      test.Assert(false, std::format("Should succeed. Error({}): {}", err.type, err.message));
    }

  auto module = std::move(result.value());

  module.module->print(llvm::errs(), nullptr);
}
