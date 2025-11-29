#pragma once

#include "codegen.hpp"
#include "lex.hpp"
#include "parse.hpp"

#include <concepts>
#include <expected>
#include <llvm/IR/LLVMContext.h>
#include <string_view>
#include <type_traits>

#include <llvm/IR/Module.h>

namespace kl {
namespace pipeline {

template <typename TInput, typename TOutput, typename TError> struct Stage {
  using Input = TInput;
  using Output = TOutput;
  using Error = TError;
  using Result = std::expected<TOutput, TError>;
  virtual Result pipe(Input input) = 0;
};

struct LexStage
    : Stage<std::string_view, std::vector<kl::lex::Token>, kl::lex::LexError> {
  Result pipe(Input input) override { return kl::lex::try_lex(input); }
};

struct ParseStage
    : Stage<std::vector<kl::lex::Token>, std::unique_ptr<kl::ast::Program>,
            kl::ast::ParseError> {
  Result pipe(Input input) override { return kl::ast::try_parse(input); }
};

struct CodegenStage
    : Stage<const std::unique_ptr<ast::Program> &,
            std::unique_ptr<llvm::Module>, kl::codegen::CodegenError> {
  std::unique_ptr<llvm::LLVMContext> &llvm_context;
  CodegenStage(std::unique_ptr<llvm::LLVMContext> &llvm_context)
      : llvm_context{llvm_context} {}
  Result pipe(Input input) override { return codegen::try_codegen(llvm_context, input); }
};

} // namespace pipeline
} // namespace kl

namespace kl {
namespace pipeline {
namespace detail {

template <typename T> struct is_variant : std::false_type {};

template <typename... Args>
struct is_variant<std::variant<Args...>> : std::true_type {};

template <typename T> inline constexpr bool is_variant_v = is_variant<T>::value;

template <class... Args> struct VariantUnionHelper;

template <class... Args1, class... Args2>
struct VariantUnionHelper<std::variant<Args1...>, std::variant<Args2...>> {
  using type = std::variant<Args1..., Args2...>;
};

template <class Variant1, class Variant2>
using VariantUnion = typename VariantUnionHelper<Variant1, Variant2>::type;

} // namespace detail
} // namespace pipeline
} // namespace kl

template <typename TInput, typename TStageInput, typename TStageOutput,
          typename TStageError>
  requires std::constructible_from<TStageInput, TInput>
std::expected<TStageOutput, std::variant<TStageError>>
operator|(TInput input,
          kl::pipeline::Stage<TStageInput, TStageOutput, TStageError> &&stage) {
  return stage.pipe(input);
}

template <typename TInput, typename TStageInput, typename TStageOutput,
          typename TStageError, typename... TPreviousErrors>
  requires std::constructible_from<TStageInput, TInput>
std::expected<TStageOutput, std::variant<TPreviousErrors..., TStageError>>
operator|(std::expected<TInput, std::variant<TPreviousErrors...>> input,
          kl::pipeline::Stage<TStageInput, TStageOutput, TStageError> &&stage) {
  if (input.has_value()) {
    return stage.pipe(input.value());
  }
  return std::unexpected{std::visit(
      [](const auto &e) -> std::variant<TPreviousErrors..., TStageError> {
        return e;
      },
      input.error())};
}
