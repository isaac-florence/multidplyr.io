// Generated by cpp11: do not edit by hand
// clang-format off


#include "cpp11/declarations.hpp"
#include <R_ext/Visibility.h>

// count_rows.cpp
int count_rows(std::string fname);
extern "C" SEXP _multidplyrio_count_rows(SEXP fname) {
  BEGIN_CPP11
    return cpp11::as_sexp(count_rows(cpp11::as_cpp<cpp11::decay_t<std::string>>(fname)));
  END_CPP11
}

extern "C" {
static const R_CallMethodDef CallEntries[] = {
    {"_multidplyrio_count_rows", (DL_FUNC) &_multidplyrio_count_rows, 1},
    {NULL, NULL, 0}
};
}

extern "C" attribute_visible void R_init_multidplyrio(DllInfo* dll){
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
  R_forceSymbols(dll, TRUE);
}
