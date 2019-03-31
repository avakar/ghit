cmake_minimum_required(VERSION 3.8)
project(ghit)

include(${CMAKE_BINARY_DIR}/conanbuildinfo.cmake)
conan_basic_setup()
conan_define_targets()

add_executable(ghit
    src/main.cpp
    )

target_compile_features(ghit PUBLIC cxx_std_17)
target_link_libraries(ghit PUBLIC CONAN_PKG::zlib projectedfslib.lib)
