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

struct Error : std::exception {
  Error(Stage stage) : stage{stage} {}
  const char* what() const override {
    return error_what.c_str();
  }

  Stage stage;
protected:
  std::string error_what;
};

};
