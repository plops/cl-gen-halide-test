//! \file main.cpp
#include "Halide.h"
#include <array>
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
Var x;
Var y;
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
        clamp(((2.55e+2f) * pow((i * (3.921569e-3f)), 1.2f)), 0, 255));
    padded(x, y, c) = input(clamp(x, 0, (input.width() - 1)),
                            clamp(y, 0, (input.height() - 1)), c);
    p16(x, y, c) = cast<uint16_t>(padded(x, y, c));
    sharpen(x, y, z) =
        ((2 * p16(x, y, c)) -
         ((2.5e-1f) * (p16((x - 1), y, c) + p16(x, (y - 1), c) +
                       p16((x + 1), y, c) + p16(x, (y + 1), c))));
    curved(x, y, c) = lut(sharpen(x, y, c));
  }
  void schedule_for_cpu() {
    lut.compute_root();
    curved.reorder(c, x, y).bound(c, 0, 3).unroll(c) {
      Var yo;
      Var yi;
      curved.split(y, yo, yi, 16)
          .parallel(yo) sharpen.compute_at(curved, yi) sharpen.vectorize(x, 8);
      padded.store_at(curved, yo)
          .compute_at(curved, yi) padded.vectorize(x, 16) curved.compile_jit()
    }
  }
  void schedule_for_gpu() {
    lut.compute_root();
    {
      Var block;
      Var thread;
      lut.split(i, block, thread, 16);
      lut.gpu_blocks(block)
          .gpu_threads(thread) curved.reorder(c, x, y)
          .bound(c, 0, 3)
          .unroll(c) curved.gpu_tile(x, y, xo, yo, xi, yi, 8, 8);
      padded.compute_at(curved, xo);
      padded.gpu_threads(x, y);
      {
        Target target = get_host_target();
        target.set_feature(Target::OpenCL);
        target.set_feature(Target::Debug);
      }
      curved.compile_jit()
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

//! @brief main function
//!
//! @usage parse command line parameters and draw to screen
//!
//! @param argc input number of command line arguments
//! @param argv input
//!
//! @return Integer

int main(int argc, char **argv) {
  {
    Buffer<uint8_t> input = load_image(not processable
                                       : (str images / rgb.png));
    MyPipeline p2(input);
    p2.schedule_for_gpu();
    p2.test_performance();
  }
  return 0;
}