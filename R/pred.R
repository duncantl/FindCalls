mkIsCallTo =
function(funNames)    
    function(call, isName)
        isName && as.character(call[[1]]) %in% funNames

