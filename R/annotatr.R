#' annotatr: annotations for R

#' @useDynLib annotatr, .registration = TRUE, .fixes = "C_"
"_PACKAGE"

#' @export
`@:` <- function(annotation, code) {
    name <- names(annotation)[[1]]
    ## TODO
}
