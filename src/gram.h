#ifndef ANNOTATR_GRAM_H
#define ANNOTATR_GRAM_H

#include <Rinternals.h>

SEXP parse_with_annotations(SEXP text);
int yyparse(void);

#endif /* ANNOTATR_GRAM_H */
