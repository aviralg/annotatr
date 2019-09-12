

.onAttach <- function(libname, pkgname) {
    .Call("override_parser");
}


.onDetach <- function(libpath) {
    .Call("reset_parser");
}
