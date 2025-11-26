include(FetchContent)

FetchContent_Declare(
  ktest
  GIT_REPOSITORY https://github.com/ketexon/ktest
  GIT_TAG main
)

FetchContent_MakeAvailable(ktest)

add_executable(
  kl_compiler_test
  lex_test.cpp
  parse_test.cpp
)

set_target_properties(
  kl_compiler_test
  PROPERTIES
  CXX_STANDARD 23
  CXX_STANDARD_REQUIRED	ON
  EXPORT_COMPILE_COMMANDS ON
)

target_link_libraries(
  kl_compiler_test
  PRIVATE
  KTestWithMain
  kl_compiler_core
)
