.onLoad <- function(libname, pkgname) {
  .summarytabl$env <- NULL
}

utils::globalVariables(c("variable", "values", "sd", "nobs", "group",
                         "percent","count", "variable_label"))
