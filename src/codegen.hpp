#pragma once

#include <parse.hpp>

#include <expected>
#include <llvm/IR/Value.h>
#include <string>

namespace kl {
namespace codegen {

struct CodegenError {
  enum class Type {
    TypeError,
    UndeclaredIdentifier,
  };
  Type type;
  std::string message;

  static const char* type_to_string(Type t) {
    switch(t) {
    case Type::TypeError: return "TypeError";
    case Type::UndeclaredIdentifier:return "UndecelaredIdentifier";
    }
    std::unreachable();
  }
};

using CodegenResult =
    std::expected<std::unique_ptr<llvm::Module>, CodegenError>;

CodegenResult try_codegen(std::unique_ptr<llvm::LLVMContext> &,
                          const std::unique_ptr<ast::Program> &program);

} // namespace codegen
} // namespace kl

template <>
struct std::formatter<kl::codegen::CodegenError::Type>
    : std::formatter<std::string> {
  auto format(const kl::codegen::CodegenError::Type &err,
              std::format_context &ctx) const {
    return std::formatter<std::string>::format(kl::codegen::CodegenError::type_to_string(err), ctx);
  }
};
