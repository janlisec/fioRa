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
    #message("read_fiora")
    x <- readLines(fl)
    i <- which(x=="BEGIN IONS")
    out <- lapply(split(x, factor(rep(1:length(i), times=diff(c(i, length(x)+1))))), function(y) {
      y <- y[-c(1,length(y))]
      idx <- grep("^[[:digit:]]", y)
      tmp <- strsplit(y[-idx], "=")
      meta <- stats::setNames(lapply(tmp, function(z) { paste(z[-1], collapse="=") }), sapply(tmp, function(z) { z[1] }) )
      n <- as.character(length(strsplit(y[idx][1], " ")[[1]]))
      s <- switch(
        n,
        "2" = matrix(as.numeric(unlist(strsplit(y[idx], " "))), ncol=2, byrow = TRUE, dimnames = list(NULL, c("mz","int"))),
        "3" = matrix(unlist(strsplit(y[idx], " ")), ncol=3, byrow = TRUE, dimnames = list(NULL, c("mz","int","SMILES"))),
        #"3" = tapply(lapply(strsplit(y[idx], " "), function(z) { data.frame("mz" = z[1], "int" = z[2], "bla" = z[3]) }), rbind),
        NA
      )
      if (ncol(s)==3) {
        s <- as.data.frame(s)
        s[,1:2] <- apply(s[,1:2], 2, as.numeric)
        tmp <- strsplit(s[,3], "//")
        s$adduct <- sapply(tmp, function(z) { z[2] }, USE.NAMES = FALSE)
        s$SMILES <- sapply(tmp, function(z) { z[1] }, USE.NAMES = FALSE)
        s$formula <- sapply(s$SMILES, function(z) { smiles2formula(z) }, USE.NAMES = FALSE)
      }
      s <- s[order(s[,"mz"], decreasing = FALSE),]
      # $$ToDo$$
      # - remove mz redundancy for multiple SMILES giving the same mz at different int values
      # - possibly scale spectrum to max=100
      rownames(s) <- 1:nrow(s)
      c(meta, list("spec" = s))
    })
    out <- stats::setNames(out, unname(sapply(out, function(x) { x$TITLE })))
    return(out)
}
