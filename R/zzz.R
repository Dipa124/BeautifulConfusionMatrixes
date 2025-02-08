.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Cargando ggplot2 y caret...")
  suppressPackageStartupMessages(library(ggplot2, quietly = TRUE, warn.conflicts = FALSE))
  suppressPackageStartupMessages(library(caret, quietly = TRUE, warn.conflicts = FALSE))
}
