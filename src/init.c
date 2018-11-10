#include <R_ext/Rdynload.h>
#include <R_ext/Visibility.h>
#include "gram.h"

static const R_CallMethodDef CallEntries[] = {
    {"parse_with_annotations", (DL_FUNC)&parse_with_annotations, 1},
    {NULL, NULL, 0}};

void attribute_visible R_init_annotatr(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
    R_forceSymbols(dll, TRUE);
}
