
compare_exprs <- function(expr1, expr2) {
    if(typeof(expr1) != typeof(expr2)) {
        print("expr1")
        print(typeof(expr1))
        print(expr1)
        print("expr2")
        print(typeof(expr2))
        print(expr2)
        return(FALSE)
    }

    if(typeof(expr1) == "NULL") {
        return(TRUE)
    }
    if(typeof(expr1) %in% c("double", "integer", "complex")) {
        return(expr1 == expr2)
    }
    if(typeof(expr1) == "symbol") {
        return(expr1 == expr2)
    }
    else if(typeof(expr1) %in% c("language", "expression", "pairlist")) {
        if(length(expr1) != length(expr2)) {
            return(FALSE)
        }
        len <- length(expr1)
        if(expr1[[1]] == "function") {
            len <- len - 1
        }
        for(i in 1:len) {
            if(!compare_exprs(expr1[[i]], expr2[[i]])) {
                return(FALSE)
            }
        }
    } else {
        print("unhandled type")
        print(typeof(expr1))
        return(FALSE)
    }

    return(TRUE)
}

## https://testthat.r-lib.org/articles/custom-expectation.html
expect_same <- function(expr1, expr2) {

    ## 1. Capture object and label
    act1 <- quasi_label(rlang::enquo(expr1))
    act2 <- quasi_label(rlang::enquo(expr2))

    expect(
        compare_exprs(act1$val, act2$val),
        sprintf("%s is not the same as %s.", act1$lab, act2$lab)
    )

    ## 3. Invisibly return the value
    invisible(act1$val)
}
