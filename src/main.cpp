#include "llvm/IR/LLVMContext.h"
#include <codegen.hpp>
#include <filesystem>
#include <fstream>
#include <iostream>
#include <iterator>
#include <pipeline.hpp>
#include <print>

#include <argparse/argparse.hpp>

enum MainOutput {
  COMPILE_RESULT_OK = 0,
  COMPILE_RESULT_ARGUMENTS_ERROR = 1,
  COMPILE_RESULT_FILE_ERROR = 2,
  COMPILE_RESULT_LEX_ERROR = 3,
  COMPILE_RESULT_PARSE_ERROR = 4,
  COMPILE_RESULT_CODEGEN_ERROR = 5,
};

int main_impl(int argc, char *argv[]) {
  argparse::ArgumentParser parser("kl_compiler");

  parser.add_argument("-o", "--output")
      .default_value(std::string("-"))
      .required()
      .help("the output file");

  auto &output_type_group = parser.add_mutually_exclusive_group();

  bool emit_ast;
  bool emit_tokens;
  bool should_typecheck;
  bool emit_llvm;

  output_type_group.add_argument("--emit-ast").store_into(emit_ast);
  output_type_group.add_argument("--emit-tokens").store_into(emit_tokens);
  output_type_group.add_argument("--typecheck").store_into(should_typecheck);
  output_type_group.add_argument("--emit-llvm").store_into(emit_llvm);

  parser.add_argument("file").required();

  try {
    parser.parse_args(argc, argv);
  } catch (const std::exception &err) {
    std::println(std::cerr, "{}", err.what());
    std::cerr << parser << std::endl;
    return COMPILE_RESULT_ARGUMENTS_ERROR;
  }

  bool output_is_binary = !(emit_ast || emit_tokens || emit_llvm);

  std::filesystem::path input_path{parser.get("file")};
  std::filesystem::path output_path{parser.get("output")};

  std::optional<std::ifstream> input_fstream;
  std::optional<std::ofstream> output_fstream;

  if (input_path != "-") {
    if (!std::filesystem::exists(input_path)) {
      std::println(std::cerr, "error: no such file \"{}\"",
                   input_path.string());
      return COMPILE_RESULT_FILE_ERROR;
    }

    input_fstream = std::ifstream{};
    input_fstream->exceptions(std::ios::failbit);
    try {
      input_fstream->open(input_path, std::ios::in);
    } catch (const std::exception &e) {
      std::println(std::cerr, "error: could not open input file \"{}\": {}",
                   input_path.string(), e.what());
      return COMPILE_RESULT_FILE_ERROR;
    }
    // turn off exceptions, since getline causes an exception
    input_fstream->exceptions(std::ios::goodbit);
  }

  if (output_path != "-") {
    std::ios_base::openmode flags = std::ios::out | std::ios::trunc;
    if (output_is_binary) {
      flags |= std::ios::binary;
    }
    output_fstream = std::ofstream{};
    output_fstream->exceptions(std::ios::failbit);
    try {
      output_fstream->open(output_path, flags);
    } catch (const std::exception &e) {
      std::println(std::cerr, "error: could not open output file \"{}\": {}",
                   input_path.string(), e.what());
      return COMPILE_RESULT_FILE_ERROR;
    }
  }

  std::istream &input_stream =
      input_fstream.has_value() ? input_fstream.value() : std::cin;
  std::ostream &output_stream =
      output_fstream.has_value() ? output_fstream.value() : std::cout;

  std::string input {
    std::istreambuf_iterator<char>(input_stream),
    std::istreambuf_iterator<char>(),
  };

  auto tokenize_result = kl::lex::try_lex(input, input_path.string());
  if (!tokenize_result.has_value()) {
    auto error = tokenize_result.error();
    std::println(std::cerr, "error: could not tokenize input: {}", error);
    return COMPILE_RESULT_LEX_ERROR;
  }

  auto tokens = tokenize_result.value();

  if (emit_tokens) {
    for (auto tok : tokens) {
      std::println(output_stream, "{}", tok);
    }
    return COMPILE_RESULT_OK;
  }

  auto ast_result = kl::ast::try_parse(tokens);
  if (!ast_result.has_value()) {
    auto error = ast_result.error();
    std::println(std::cerr, "error: could not parse tokens: {}", error);
    return COMPILE_RESULT_PARSE_ERROR;
  }

  auto ast = std::move(ast_result.value());

  if (emit_ast) {
    std::println(output_stream, "{}", *ast);
    return COMPILE_RESULT_OK;
  }

  auto typecheck_result = kl::ast::try_typecheck(std::move(ast));

  if (!typecheck_result.has_value()) {
    auto error = typecheck_result.error();
    std::println(std::cerr, "error: could not typecheck: {}", error);
    return COMPILE_RESULT_PARSE_ERROR;
  }

  if (should_typecheck) {
    std::println("Typechecked successfully.");
    return COMPILE_RESULT_OK;
  }

  auto module_result =
      kl::codegen::try_codegen(std::make_shared<llvm::LLVMContext>(), ast);
  if (!module_result.has_value()) {
    auto error = module_result.error();
    std::println(std::cerr, "error: could not generate code: {}", error);
    return COMPILE_RESULT_CODEGEN_ERROR;
  }

  auto module = std::move(module_result.value());
  if (emit_llvm) {
    std::string out_string;
    llvm::raw_string_ostream out_llvm_ostream{out_string};
    module.module->print(out_llvm_ostream, nullptr);
    std::println(output_stream, "{}", out_string);
    return COMPILE_RESULT_OK;
  }

  return 0;
}

int main(int argc, char* argv[]) {
  try {
    return main_impl(argc, argv);
  } catch(const std::exception& e) {
    std::println(std::cerr, "Unhandled exception: {}", e.what());
  } catch(...) {
    std::println(std::cerr, "Unhandled unknown exception :(");
  }
}
