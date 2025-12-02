include(FetchContent)
FetchContent_Declare(
  argparse
  URL https://github.com/p-ranav/argparse/archive/refs/tags/v3.2.zip
)

FetchContent_MakeAvailable(argparse)

add_library(
  kl_compiler_core
  lex.hpp lex.cpp
  parse.hpp parse.cpp parse_node.cpp
  types.hpp types.cpp
  typecheck.hpp typecheck.cpp
  codegen.hpp codegen.cpp
)

add_executable(
  kl_compiler
  main.cpp
)

set_target_properties(
  kl_compiler_core kl_compiler
  PROPERTIES
  CXX_STANDARD 23
  CXX_STANDARD_REQUIRED ON
  EXPORT_COMPILE_COMMANDS ON
)

target_include_directories(
  kl_compiler_core
  PUBLIC
  ${LLVM_INCLUDE_DIRS}
)

message(${LLVM_INCLUDE_DIRS})

target_include_directories(
  kl_compiler_core
  PUBLIC
  .
)

separate_arguments(LLVM_DEFINITIONS_LIST NATIVE_COMMAND ${LLVM_DEFINITIONS})
target_compile_definitions(
  kl_compiler
  PUBLIC
  ${LLVM_DEFINITIONS_LIST}
)

set(llvm_config_flags "")

if (WIN32)
else()
  list(APPEND llvm_config_flags "USE_SHARED")
endif()

llvm_config(kl_compiler_core ${llvm_config_flags} support core irreader)

message("${LLVM_LIBS}")

target_link_libraries(
  kl_compiler_core
  PUBLIC
  ${LLVM_LIBS}
)

target_link_libraries(
  kl_compiler
  PRIVATE
  kl_compiler_core
  argparse::argparse
)
