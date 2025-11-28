#pragma once

#include <parse.hpp>

#include <expected>
#include <llvm/IR/Value.h>
#include <string>

namespace kl {
namespace codegen {

struct CodeGenError {
  enum class Type {

  };
  Type type;
  std::string message;
};

using CodegenResult = std::expected<llvm::Value, CodeGenError>;

CodegenResult try_codegen(std::unique_ptr<ast::Program> program);

}
}
