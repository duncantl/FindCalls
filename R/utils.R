isFunctionDef =
function(call)
    callTo(call) == "function"

callTo =
function(x)
{
    if(length(x) > 0)
        deparse(x[[1]])
    else
        NA
}
