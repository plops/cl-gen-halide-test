//! \file main.cpp
// compile gradient-halide with
// pacman -S openblas eigen
// export HL_DEBUG_CODEGEN=1; export HL_TARGET=host-opencl; cmake
// -DCMAKE_BUILD_TYPE=Release -DOpenGL_GL_PREFERENCE=GLVND .. g++ main.cpp -g
// -std=c++11 -I /usr/local/share/halide -I /usr/local/share/halide/tools -I
// /usr/local/share/halide/tutorial -I /usr/local/include -L /usr/local/lib
// -lHalide `libpng-config --cflags --ldflags` -ljpeg -lpthread -ldl -o main
#include "Halide.h"
using namespace Halide;
#include "halide_image_io.h"
using namespace Halide::Tools;
#include "clock.h"
#include <array>
#include <dlfcn.h>
#include <iostream>

//! \mainpage test halide
//! \section Introduction
//! this is a test of the halide code generator
//! \section Dependencies
//! - Halide

//! - cxxopts for commandline argument parsing.

//! - sbcl to generate c++ code
//! - g++ to compile c++ code

//! - For the documentation (optional for use):
//!   + doxygen

//! \section References
//! 1. http://halide-lang.org/tutorials/tutorial_lesson_12_using_the_gpu.html
//! 2. https://github.com/halide/Halide/wiki/Debugging-Tips
Var x;
Var y;
Var z;
Var c;
Var i;
Var ii;
Var xo;
Var yo;
Var xi;
Var yi;

class MyPipeline {
public:
  Func lut;
  Func padded;
  Func p16;
  Func sharpen;
  Func curved;
  Buffer<uint8_t> input;

  MyPipeline(Buffer<uint8_t> in) : input(in) {
    lut(i) = cast<uint8_t>(
        clamp(((2.55e+2f) * pow((cast<float>(i) / (2.55e+2f)), 1.2f)),
              (0.0e+0f), (2.55e+2f)));
    padded(x, y, c) = input(clamp(x, 0, (input.width() - 1)),
                            clamp(y, 0, (input.height() - 1)), c);
    p16(x, y, c) = cast<uint16_t>(padded(x, y, c));
    sharpen(x, y, c) =
        ((2 * p16(x, y, c)) - ((p16((x - 1), y, c) + p16(x, (y - 1), c) +
                                p16((x + 1), y, c) + p16(x, (y + 1), c)) /
                               4));
    curved(x, y, c) = lut(sharpen(x, y, c));
  }
  void schedule_for_cpu() {
    lut.compute_root();
    {
      Var yo;
      Var yi;
      curved.reorder(c, x, y).bound(c, 0, 3).unroll(c);
      curved.split(y, yo, yi, 16).parallel(yo);
      sharpen.compute_at(curved, yi);
      sharpen.vectorize(x, 8);
      padded.store_at(curved, yo).compute_at(curved, yi);
      padded.vectorize(x, 16);
      curved.compile_jit();
    }
  }
  void schedule_for_gpu() {
    lut.compute_root();
    {
      Var block;
      Var thread;
      lut.split(i, block, thread, 16);
      lut.gpu_blocks(block).gpu_threads(thread);
      curved.reorder(c, x, y).bound(c, 0, 3).unroll(c);
      curved.gpu_tile(x, y, xo, yo, xi, yi, 8, 8);
      padded.compute_at(curved, xo);
      padded.gpu_threads(x, y);
      {
        Target target = get_host_target();
        target.with_feature(Target::OpenGL);
        target.set_feature(Target::Debug);
      }
      curved.compile_jit();
    }
  }
  void test_performance() {
    {
      Buffer<uint8_t> output(input.width(), input.height(), input.channels());
      curved.realize(output);
      {
        auto best_time = (0.0e+0);
        for (unsigned int i = 0; (i < 3); i += 1) {
          {
            auto t1 = current_time();
            for (unsigned int j = 0; (j < 100); j += 1) {
              curved.realize(output);
            }
            output.copy_to_host();
            {
              auto t2 = current_time();
              auto elapsed = ((t2 - t1) / (1.e+2));
              if (((i == 0) || (elapsed < best_time))) {
                best_time = elapsed;
              }
            }
          }
        }
      }
    }
  }
};

bool have_opencl_or_metal();
bool load_opencl() { return (NULL != dlopen("libOpenCL.so", RTLD_LAZY)); }

//! @brief main function
//!
//! @usage parse command line parameters and draw to screen
//!
//! @param argc input number of command line arguments
//! @param argv input
//!
//! @return Integer

int main(int argc, char **argv) {
  if ((!(load_opencl()))) {
    write(1, "error", 5);
  }
  {
    Buffer<uint8_t> input = load_image("images/rgb.png");
    MyPipeline p2(input);
    p2.schedule_for_gpu();
    p2.test_performance();
  }
  return 0;
}