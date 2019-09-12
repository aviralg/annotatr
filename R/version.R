
#' @export
get_version <- function() {
    .Call("get_git_version")
}
