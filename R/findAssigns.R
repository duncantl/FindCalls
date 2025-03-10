findAssigns =
function(code,
         simpleAssigns = FALSE,
         assignmentOps = c("<-", "=", "<<-"),
         pred = (if(simpleAssigns) mkIsSimpleAssign else mkIsCallTo)(assignmentOps))
{
    findAllCalls(code, pred)
}

mkIsSimpleAssign =
function(assignmentOps = c("<-", "=", "<<-"))    
{
    function(call, isName)
        isName && as.character(call[[1]]) %in% assignmentOps && is.name(call[[2]])
}
