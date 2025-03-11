#
# Conceptually simpler version than in CodeAnalysis.
# Finds all function() definitions, including nested and anonymous functions used in calls.
#

getFunctionDefs =
function(code, ...)
{
    findCallsTo(code, "function", ...)
}

getFunctionDefs2 =
function(code, anonymous = FALSE, ...)
{
    if(anonymous)
       return( findCallsTo(code, "function", ...) )

    defs = findAssignsTo(code)
    # FIX:  if code is a vector > 1 of file names, the result is a list() of lists
    
    w = sapply(defs, function(x) isFunctionDef(x[[3]]))
    defs[w]
}
