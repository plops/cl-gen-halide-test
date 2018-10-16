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
class MyPipeline {
public:
  Func lut;
  Func padded;
  Func padded16;
  Func sharpen;
  Func curved;
  Buffer<uint8_t> input;

  MyPipeline(Buffer<uint8_t> in) : input(in);
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