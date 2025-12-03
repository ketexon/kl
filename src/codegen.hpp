#pragma once

#include <parse.hpp>

#include <expected>
#include <llvm/IR/Value.h>
#include <string>
#include <error.hpp>

namespace kl {
namespace codegen {

struct CodegenError : Error {
  enum class Type {
    TypeError,
    UndeclaredIdentifier,
    VerificationError,
    ReturnTypeMismatch,
    InvalidIntegerSuffix,
    InvalidIntegerLiteral,
    IntegerOutOfBounds,
  };

  CodegenError(Type, std::string);

  Type type;
  std::string message;

  static const char *type_to_string(Type t) {
    switch (t) {
    case Type::TypeError:
      return "TypeError";
    case Type::UndeclaredIdentifier:
      return "UndecelaredIdentifier";
    case Type::VerificationError:
      return "VerificationError";
    case Type::ReturnTypeMismatch:
      return "ReturnTypeMismatch";
    case Type::InvalidIntegerSuffix:
      return "InvalidIntegerSuffix";
    case Type::InvalidIntegerLiteral:
      return "InvalidIntegerLiteral";
    case Type::IntegerOutOfBounds:
      return "IntegerOutOfBounds";
    }
    std::unreachable();
  }
};

struct Module {
  std::shared_ptr<llvm::LLVMContext> llvm_context;
  std::unique_ptr<llvm::Module> module;
};

using CodegenResult = std::expected<Module, CodegenError>;

CodegenResult try_codegen(std::shared_ptr<llvm::LLVMContext>,
                          const std::unique_ptr<ast::ProgramNode> &program);

} // namespace codegen
} // namespace kl

template <>
struct std::formatter<kl::codegen::CodegenError::Type>
    : std::formatter<std::string> {
  auto format(const kl::codegen::CodegenError::Type &err,
              std::format_context &ctx) const {
    return std::formatter<std::string>::format(
        kl::codegen::CodegenError::type_to_string(err), ctx);
  }
};

template <>
struct std::formatter<kl::codegen::CodegenError> : std::formatter<std::string> {
  auto format(const kl::codegen::CodegenError &err,
              std::format_context &ctx) const {
    return std::formatter<std::string>::format(
        std::format("CodegenError({}) \"{}\"", err.type, err.message), ctx);
  }
};
