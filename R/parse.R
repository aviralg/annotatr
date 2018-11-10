parse_text <- function(text) {
    .Call("parse_with_annotations", as.character(text))
}


parse_file <- function(file) {
    .Call("parse_with_annotations", readLines(file))
}
