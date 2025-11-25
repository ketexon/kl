cmake_minimum_required(VERSION 4.0)
project(kl)

find_package(LLVM REQUIRED CONFIG)

add_subdirectory(src)
add_subdirectory(test)
