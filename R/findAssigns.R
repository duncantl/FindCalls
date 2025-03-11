findAssignsTo =
function(code,
         vars = character(),
         simpleAssigns = FALSE,
         assignmentOps = c("<-", "=", "<<-"), ...
         )
{
    if(length(vars) == 0)
        pred =  (if(simpleAssigns) mkIsSimpleAssign else mkIsCallTo)(assignmentOps)
    else
        pred = function(call, isName, ...) {

            isAssign = is.name(call[[1]]) && as.character(call[[1]]) %in% assignmentOps
            if(!isAssign)
                return(FALSE)
            
            simple = is.name(call[[2]]) #  isSimpleAssign(call, isName, assignmentOps)

            if(simpleAssigns || simple)
                  simple && is.name(call[[2]]) && as.character(call[[2]]) %in% vars
            else {
                to = complexAssignTo(call[[2]], call[[1]])
                is.name(to) && as.character(to) %in% vars
            }
        }
    
    findAllCalls(code, pred, ...)
}

mkIsSimpleAssign =
function(assignmentOps = c("<-", "=", "<<-"))    
{
    #    environment(isSimpleAssign) = environment()
    formals(isSimpleAssign)[[3]] = assignmentOps
    isSimpleAssign
}

isSimpleAssign =
function(call, isName, assignmentOps = c("<-", "=", "<<-"))
   isName && as.character(call[[1]]) %in% assignmentOps && is.name(call[[2]])    


complexAssignTo =
function(lhs, op)
{
    fn = lhs[[1]]

    if(is.name(fn)) {

        switch(as.character(fn),
               attr = ,
               "[" =,
               "[["=,
               "$" = lhs[[2]],
               lhs[[2]]
        )
    } else {
        # e.g. foo()(a,2) = 1

        k = lhs[[2]]
    browser()        
    }
}
