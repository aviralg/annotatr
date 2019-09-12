#include "version.h"

SEXP get_git_version() { return mkString(GIT_COMMIT_INFO); }
