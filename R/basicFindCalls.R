findCallsTo =
function(code, funNames = character(), ...)    
{
    pred = if(length(funNames))
               mkIsCallTo(funNames)
           else
               skipFunctions
    findAllCalls(code, pred, ...)
}

findAllCalls =
function(code, pred = function(...) TRUE, skipIfFalse = TRUE,
         walker = callFinder(pred, skipIfFalse = skipIfFalse)
         )
    # should we add a recurive = and a pattern =
    # for use in list.files.
    # Or just let the caller get the list of files themselves.
{

    if(is.character(code)) {

        if(length(code) > 1)
            # should pass walker but that will accumulate answers. Need to reset after each one.
            # Putting the names here is bad when the content is code, not file names!
            return(structure(lapply(code, findAllCalls, pred, skipIfFalse), names = code))
        
        
        if(file.exists(code)) {

            if(file.info(code)$isdir)
                return(findAllCalls(list.files(code, pattern = "\\.[RrSq]$", full.names = TRUE),
                                    pred, skipIfFalse)) # walker again!

            
            code = parse(code)
        } else
            code = parse(text = code)
    } else if(is.function(code))
        code = list(formals(code), body(code))
    
    walkCode(code, walker)
    walker$ans()
}

skipFunctions =
function(call, isName, ...)
{
   !isName || as.character(call[[1]]) != "function"
}

callFinder = 
function (pred, ..., skipIfFalse = FALSE) 
{
    calls = list()
    leaf = function(x, w, ...) {
        ty = typeof(x)
        if (ty %in% c("pairlist", "expression", "list", "language")) {
            lapply(x, walkCode, w)
            return(NULL)
        }
        else if (ty == "closure") {
            walkCode(formals(x), w)
            walkCode(body(x), w)
            return(NULL)
        }
    }
    call = function(x, w, ...) {
        if (skipIfFalse && skipIfFalse(x, w)) 
            return(NULL)
        isName = is.name(x[[1]])
        if (isSymbol(x[[1]], c("<-", "=")) && is.call(x[[2]])) 
            attr(x[[2]], "isLHS") = TRUE
        if (pred(x, isName, ...)) 
            calls[[length(calls) + 1L]] <<- x
        els = as.list(x)
        if (isName && as.character(x[[1]]) %in% c(".Internal", 
            ".Primitive")) 
            els = els[-2]
        for (ee in els) {
            if (!missing(ee)) 
                walkCode(ee, w)
        }
    }
    list(handler = function(x, w) NULL, leaf = leaf, call = call, 
        ans = function() calls)
}

skipIfFalse =
function (x, w) 
{
    if (isCallTo(x, "if") && isFALSE(x[[2]])) {
        if (length(x) == 4) 
            walkCode(x[[4]], w)
        return(TRUE)
    }
    FALSE
}


isCallTo =
function (code, funName, indirect = character(),  #getIndirectCallFunList(), 
    isLHS = NA) 
{
#   if (inherits(code, "ScriptNodeInfo")) 
#       x = code@code
#   if (inherits(code, "R6")) 
#       inherits(code, "Call") && is_symbol(code$fn) && code$fn$value %in% 
#           funName
#   else
        (is.call(code) || inherits(code, "call")) && (isSymbol(code[[1]], funName)
      #  || (!isFALSE(indirect) && isIndirectCall(code, 
      #                                           indirect, funName, TRUE, isLHS = isLHS))
    )
}

isSymbol =
function (x, sym = character()) 
    is.name(x) && (length(sym) == 0 || as.character(x) %in% sym)
