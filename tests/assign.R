if(FALSE) {

    cond = foo()
    if(cond) {
        x = 1
        y <- 2
    }

    z <<- 3

    x$abc = 3
    attr(x, "abc") = 4

    z <- 3

    foo(x, 2) = 110

    foo(x^2, 2) = 110    
}

a = parse("assign.R")[[1]][[3]]

b = findAssignsTo(a, "x")
stopifnot(length(b) == 1)

b = findAssignsTo(a, "x", simpleAssigns = FALSE)
stopifnot(length(b) == 3)

b = findAssignsTo(a, "z", assignmentOp = c("=", "<-"))
stopifnot(length(b) == 1)


