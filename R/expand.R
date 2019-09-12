
#' @export
expand <- function(code) {
    expander <- get_expander()
    expander(code)
}


#' @export
top_down_expander <- function(code) {

    type <- typeof(code)

    if(is_vector(type)) {
        result <- code
    } else if(is_symbol(type)) {
        result <- code
    } else if(is_expression(code)) {
        l <- length(code)
        result <- Recall(code[[1]])
    } else if(is_language(code)) {
        if(is_annotated(code)) {
            result <- Recall(process_annotations(code))
        }
        else {
        }
    }

    result
}

is_vector <- function(type) {
    type %in% c("logical", "integer", "double", "complex", "character", "raw")
}

is_null <- function(type) {
    type == "NULL"
}


 and ‘"list"’,  ‘"closure"’
(function), ‘"special"’ and ‘"builtin"’ (basic functions and
    operators), ‘"environment"’, ‘"S4"’ (some S4 objects) and others
that are unlikely to be seen at user level (‘"symbol"’,
                                            ‘"pairlist"’, ‘"promise"’, ‘"language"’, ‘"char"’, ‘"..."’,
                                            ‘"any"’, ‘"expression"’, ‘"externalptr"’, ‘"bytecode"’ and
                                            ‘"weakref"’).
