#include <string>
#include <fstream>
#include "cpp11/doubles.hpp"
using namespace cpp11;

[[cpp11::register]]
int count_rows(std::string fname) {

  int numLines = 0;
  std::ifstream in(fname);
  std::string unused;
  while ( std::getline(in, unused) )
    ++numLines;
return numLines;
}
