#pragma once

#include <string>
#include <format>

namespace kl {

enum class Stage {
  Lex = 1,
  Parse = 2,
  Type = 3,
  Codegen = 4,
};

enum class ErrorID {
  None = 0,

  MismatchedReturnType = 30000,
};

struct Error {
  Stage stage;
  ErrorID id;
  std::string message;
};

};
