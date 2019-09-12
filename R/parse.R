
#' @export
parse_text <- function(..., sep = "\n", collapse = NULL) {
    .Call("parse_with_annotations", paste(..., sep = sep, collapse = NULL))
}

#' @export
parse_file <- function(file) {
    .Call("parse_with_annotations", readLines(file))
}
