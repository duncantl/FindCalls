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

fns = sapply(ac[direct], function(x) deparse(x[[1]]))
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

# What are the names of functions used in the code that contain read in the name?
unique(grep("read", fns, ignore.case = TRUE, value = TRUE))

# We know the names of some read functions. We'll look for those.
ReadFuns = c("read.csv", "read.table", "read.fwf", "readxl", "read_excel", "read.xls", "readLines")
# We'll get the name of the first argument, which is typically the source from which to read.
avail = sapply(ReadFuns, function(x) length(find(x)) > 0)
sapply(ReadFuns[avail], function(x) names(formals(x))[1])

ReadFunArgName = rep(NA, length(ReadFuns)


readCalls = lapply(vtp, findCallsTo, ReadFuns)
nc = sapply(readCalls, length)
readCalls = readCalls[nc > 0]

rc2 = unlist(readCalls)
fileLiterals = unlist(lapply(rc2, function(x) if(length(x) > 1 && is.character(x[[2]])) x[[2]] else NA))
unname(fileLiterals)


# Better to use match.call
#
ex = sapply(rc2, function(x) is.symbol(x[[1]]) && exists(as.character(x[[1]]), mode = "function"))
table(ex)
fnName = sapply(rc2, \(x) as.character(x[[1]]))
# For me, read_excel and read.xls are not on the search path.
rc3 = rc2
rc3[ex] = lapply(rc3[ex], function(x) match.call(get(as.character(x[[1]]), mode = "function"), x))




