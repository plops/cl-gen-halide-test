//! \file main.cpp
#include "Halide.h"
#include "cxxopts.hpp"
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
  try {
    cxxopts::Options options("drm_draw", "draw to drm device");
    options.add_options()("h,help", "Print help");
    options.parse(argc, argv);
    if (options.count("help")) {
      (std::cout << options.help() << std::endl);
      exit(0);
    }
    {
      Halide::Func gradient;
      Halide::Var x;
      Halide::Var y;
      Halide::Expr e((x + y));
      gradient(x, y) = e;
      {
        Halide::Buffer<int32_t> output(gradient.realize(800, 600));
        for (int j = 0; (j < output.height()); j++) {
          for (int i = 0; (i < output.width()); i++) {
            if (((i + j) != output(i, j))) {
              (std::cerr << "error, expected " << (i + j) << " but result is "
                         << output(i, j) << std::endl);
            }
          }
        }
      }
    }
  } catch (const cxxopts::OptionException &e) {

    (std::cout << "error parsing options: " << e.what() << std::endl);
    exit(1);
  }
  return 0;
}