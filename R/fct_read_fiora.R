#' read_fiora
#' @param fl file.
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
read_fiora <- function(fl) {
    x <- readLines(fl)
    i <- which(x=="BEGIN IONS")
    out <- lapply(split(x, factor(rep(1:length(i), times=diff(c(i, length(x)+1))))), function(y) {
      y <- y[-c(1,length(y))]
      idx <- grep("=", y)
      tmp <- strsplit(y[idx], "=")
      meta <- setNames(lapply(tmp, function(z) { paste(z[-1], collapse="=") }), sapply(tmp, function(z) { z[1] }) )
      s <- matrix(as.numeric(unlist(strsplit(y[-idx], " "))), ncol=2, byrow = TRUE, dimnames = list(NULL, c("mz","int")))
      c(meta, list("spec" = s))
    })
    out <- setNames(out, unname(sapply(out, function(x) { x$TITLE })))
    return(out)
}
