dir = "~/Book/ExploreCode/Variety_trial_analysis"

# Parse all the .R files in the directory.
vt = list.files(dir, full.names = TRUE, recursive = TRUE, pattern = "\\.R$")
vtp = lapply(vt, parse)
names(vtp) = vt


ac = unlist(lapply(unlist(vtp), findCallsTo))
direct = sapply(ac, function(x) is.name(x[[1]]))
table(direct)
# All but one is direct.
# That is a call
#    colorRampPalette(cols)(255)

fns = sapply(ac[direct], callTo)
table(fns)

# Let's find calls to library or require
loadPkgs = unlist(lapply(vtp, findCallsTo, c("require", "library")))
# or
w = fns %in% c("require", "library")
pkgs = sapply(ac[direct][w], function(x) deparse(x[[2]]))
table(unname(pkgs))

############################
#
# Seeing what files or directories we read from.
#

# We know the names of some read functions. We'll look for those.
ReadFuns = c("read.csv", "read.table", "read.fwf", "readxl", "read_excel", "read.xls", "readLines")

# What are the names of functions used in the code that contain read in the name?
unique(grep("read", fns, ignore.case = TRUE, value = TRUE))
# In my example, we have readRiceFiles{,2} and these are probably functions defined in the scripts that will read files.
# So we want to include those in the

# getFunctionDefs is currently in CodeAnalysis.
# A simple version is findCallsTo(code, "function"). However, this gets all nested functions too.
fns2 = unlist(lapply(vt, getFunctionDefs))
grep("readRiceFiles", names(fns2), value = TRUE)
# So each appears to be defined in 3 places

# But that's not the point. We can look in each function and see if they call any function in ReadFuns.
calls = lapply(fns2, function(fun) {
                        k = findCallsTo(fun)
                        cfuns = sapply(k, function(x) if(is.name(x[[1]])) as.character(x[[1]]) else "")
                     })
tmp = lapply(calls, intersect, ReadFuns)
callsReadFuns = names(tmp)[ sapply(tmp, length) > 0]

# We see read_vt, getPOWER, get_ucipm, readRiceFiles, readRiceFiles2
# So we can add those to the ReadFuns.
ReadFuns = c(ReadFuns, callsReadFuns)

readCalls = lapply(vtp, findCallsTo, ReadFuns)
nc = sapply(readCalls, length)
readCalls = readCalls[nc > 0]

rc2 = unlist(readCalls)
fileLiterals = unlist(lapply(rc2, function(x) if(length(x) > 1 && is.character(x[[2]])) x[[2]] else NA))
unname(fileLiterals)

fn3 = sapply(rc2, callTo)
w2 = sapply(rc2, function(x) is.character(x[[2]]))

# Now are there any calls to these locally defined read functions that have a literal string as the first argument?
table(fn3[w2] %in% callsReadFuns)
# No.



###################
# Better to use match.call() for analyzing the call.
# Could have read.table(sep = "\t", file)
# But can't call match.call() unless we have the function definition.
# This is where having the names of the packages that are loaded in the scripts is useful.
# We could use install.packages() and the load them.
# For the functions defined locally within the scripts, we have then in fns


# We'll get the name of the first argument, which is typically the source from which to read.

ReadFunArgName = rep(NA, length(ReadFuns))
names(ReadFunArgName) = ReadFuns
avail = sapply(ReadFuns, function(x) length(find(x)) > 0)
ReadFunArgName[avail] = sapply(ReadFuns[avail], function(x) names(formals(x))[1])


# Here we do something slightly different but similar to avail above but this time
# we are looking at all the calls (rc2) and seeing if this is calling a function  by name ( or itself a call)
# and for this that refer to a name, we check if we can find the function on the search path.
# Then for the ones we do have a function defintion, we use match.call to convert the call into the
# canonical form, i.e., rearrange arguments, add names, etc.

# fnName = sapply(rc2, \(x) as.character(x[[1]]))
ex = sapply(rc2, function(x) is.symbol(x[[1]]) && exists(as.character(x[[1]]), mode = "function"))
table(ex)
# For me, read_excel and read.xls are not on the search path.
rc3 = rc2
rc3[ex] = lapply(rc3[ex], function(x) match.call(get(as.character(x[[1]]), mode = "function"), x))

# So now we can process these more accurately.
# The ones where ex == FALSE, we still are taking a guess that the first argument is the file.


# We can arrange to have all the functions available since we can
# find the calls to library & require and then install those packages
# and load them.  To be accurate we have to handle potential masking or multiple functions with the same name
# or get the order we load them to be the same as the code would, i.e.,
# in script A know where to find function fun1 based on which calls to library/require were made in that script.
# But in script B, know that the library/require calls would be different and change the search path so look for
# fun1 in that script based on its package loaded.



