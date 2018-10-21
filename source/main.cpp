//! \file main.cpp
// compile gradient-halide with
// pacman -S openblas eigen
// export HL_DEBUG_CODEGEN=1; export HL_TARGET=host-opencl;export
// HL_JIT_TARGET=host-opencl; cmake -DCMAKE_BUILD_TYPE=Release
// -DOpenGL_GL_PREFERENCE=GLVND .. g++ main.cpp -g -std=c++11 -I
// /usr/local/share/halide -I /usr/local/share/halide/tools -I
// /usr/local/share/halide/tutorial -I /usr/local/include -L /usr/local/lib
// -lHalide `libpng-config --cflags --ldflags` -ljpeg -lpthread -ldl -o main
// -Wl,-rpath=/usr/local/lib
#include "Halide.h"
using namespace Halide;

//! \mainpage test halide
//! \section Introduction
//! this is a test of the gradient halide code generator
//! \section Dependencies
//! - Halide

//! - cxxopts for commandline argument parsing.

//! - sbcl to generate c++ code
//! - g++ to compile c++ code

//! - For the documentation (optional for use):
//!   + doxygen

//! \section References
//! 1.
//! https://github.com/halide/Halide/blob/5d298f0d1731e5b89ee6ff1ae948a5201e6ea471/test/correctness/fit_function.cpp
//! 2. https://github.com/halide/Halide/wiki/Debugging-Tips

//! @brief main function
//!
//! @usage
//!
//! @param argc input number of command line arguments
//! @param argv input
//!
//! @return Integer

int main(int argc, char **argv) {
  {
    ImageParam coeffs(Float(64), 1);
    Param<double> learning_rate;
    Param<int> order;
    Param<int> samples;
    Func approx_sin;
    Var x;
    Var y;
    Expr fx = ((x / cast<double>(samples)) * Expr((1.5707963267948966e+0)));
    RDom r(0, order);
    Expr r_flipped = (1 - r);
    Func exact_sin;
    approx_sin(x, y) = cast<double>((0.0e+0f));
    approx_sin(x, r_flipped) =
        ((fx * coeffs(r_flipped)) + (approx_sin(x, (1 + r_flipped)) * fx));
    exact_sin(x) = sin(fx);
    {
      Func err;
      err(x) = pow(((approx_sin(x, 0) - exact_sin(x)) / exact_sin(x)), 2);
      {
        RDom d(1, (samples - 1));
        Func avg_err;
        avg_err() = (sum(err(d)) / samples);
      }
    }
  }
}