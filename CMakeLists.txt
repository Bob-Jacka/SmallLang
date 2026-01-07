cmake_minimum_required(VERSION 3.31)
project(SmallParser)

set(CMAKE_CXX_STANDARD 23)

find_package(LLVM REQUIRED CONFIG)
message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")

include_directories(${LLVM_INCLUDE_DIRS})
message(STATUS "Include dirs of llvm: ${LLVM_INCLUDE_DIRS}")
separate_arguments(LLVM_DEFINITIONS_LIST NATIVE_COMMAND ${LLVM_DEFINITIONS})
add_definitions(${LLVM_DEFINITIONS_LIST})
add_executable(parser
        main.cpp
        core/entities/CustomLang.hpp
        core/entities/Environment.hpp
        core/data/Constants.hpp
        core/entities/Parser.h
        core/entities/CustomLangException.hpp
)
llvm_map_components_to_libnames(llvm_libs support core irreader)
target_link_libraries(parser ${llvm_libs})