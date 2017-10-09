cmake_minimum_required (VERSION 2.6)
set (CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/CMake")

#include(cotire)
#set (COTIRE_MINIMUM_NUMBER_OF_TARGET_SOURCES 1)
project (halide_test)
set(CMAKE_CXX_EXTENSIONS ON)
set(CMAKE_CXX_STANDARD 14)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
#set ( CMAKE_CXX_COMPILER  clang++)
set ( CMAKE_CXX_COMPILER  g++)
# for clang and libFuzzer:
#set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -O3 -march=native -fomit-frame-pointer") # fuzzer
#set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fsanitize=undefined  -fsanitize=address -O2 -g -march=native -fno-omit-frame-pointer") # fuzzer
# -fsanitize=undefined -fno-sanitize-recover=undefined
# this does not work with clang-4.0 it requires 5.0 from ttp://apt.llvm.org/
# 5.0 requires sudo ln -s /usr/lib/llvm-5.0/lib/libFuzzer.a /usr/lib/libLLVMFuzzer.a

# for gcc:

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -frounding-math -fext-numeric-literals -fpermissive -march=native -mtune=native -O2 -std=c++17 -Werror -Wall -Wdouble-promotion  -Wduplicated-cond -Wformat -Wformat=2  -Wlogical-op -Wnull-dereference    -L/home/martin/big/halide/bin -lHalide -Wl,-rpath=/home/martin/big/halide/bin ")
# -Wduplicated-branches -Wrestrict -Wjump-misses-init -Wold-style-cast -Werror=shadow  -Wuseless-cast


add_executable(halide_test main.cpp)
target_compile_features(halide_test PUBLIC PRIVATE cxx_lambdas cxx_range_for)

target_include_directories(halide_test PUBLIC "/home/martin/big/halide/include/")
#target_link_libraries(drm_draw "/home/martin/libbla.a")

#find_library(halide_lib Halide)
#target_link_libraries(halide_test ${halide_lib})
  
#cotire(halide_test)



