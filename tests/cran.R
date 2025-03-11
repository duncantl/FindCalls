# path to a local copy of CRAN
CRAN = "~/CRAN2/Pkgs"
if(file.exists()) {
    rf = list.files(CRAN, recursive = TRUE, full.names = TRUE, pattern  = "\\.[RSrsq]$")
    rfs = sample(rf, 10000)
    ps = lapply(rfs, function(f) try(parse(f)))
    err = sapply(ps, inherits, 'try-error')
    table(err)

    ks = lapply(ps[!err], findAllCalls)
}

