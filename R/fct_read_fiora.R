#' @title Read a fiora result file (mgf) into a R list object.
#'
#' @param fl file.
#'
#' @description Allows to import a `fioRa` output file (msp like format) to a list like format.
#'
#' @return A result list of legth = n_compounds containing metadata fields as
#'     sub-lists and the predicted MS^2 spectrum as sub-list `spec`.
#'
#' @noRd
read_fiora <- function(fl) {
    x <- readLines(fl)
    i <- which(x=="BEGIN IONS")
    out <- lapply(split(x, factor(rep(1:length(i), times=diff(c(i, length(x)+1))))), function(y) {
      y <- y[-c(1,length(y))]
      idx <- grep("=", y)
      tmp <- strsplit(y[idx], "=")
      meta <- stats::setNames(lapply(tmp, function(z) { paste(z[-1], collapse="=") }), sapply(tmp, function(z) { z[1] }) )
      s <- matrix(as.numeric(unlist(strsplit(y[-idx], " "))), ncol=2, byrow = TRUE, dimnames = list(NULL, c("mz","int")))
      c(meta, list("spec" = s))
    })
    out <- stats::setNames(out, unname(sapply(out, function(x) { x$TITLE })))
    return(out)
}
