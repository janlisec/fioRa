#' @title Read a fiora result file (mgf) into a R object.
#'
#' @param fl file.
#' @param fmt A named list object is returned if not specified otherwise. Set 'fmt=df'
#'     to simplify the return value to a data frame. Use 'fmt=Spectra' to convert to
#'     a [Spectra::Spectra()] object.
#' @param check Perform some sanity checks on a `FIORA` result file, i.e. avoiding
#'     invalid adducts defining [M-3H] where only one or two H atoms are contained
#'     in the formula.
#' @param scale Allows to scale spectra upon import. For scale=0 (default) no modification
#'     is applied. Otherwise the maximum intensity peak is set to the specified value.
#'     Values of 999 (for NIST), 1 or 100 would be common choices.
#'
#' @description Allows to import a `FIORA` output file (msp like format) to a list
#'     like format or simplified to a data frame or Spectra object.
#'
#' @return A result list of length = n_compounds containing metadata fields as
#'     sub-lists and the predicted MS^2 spectrum as sub-list `spec`. You can set
#'     parameter `fmt` to 'df' in which case each compound list will be coerced
#'     to a data frame row (spectra will be encoded as 'mz1:int1 mz2:int2 ...'
#'     and information regarding SMILES, adduct or formula per peak will be lost).
#'
#' @examples
#' fl <- system.file("extdata/annotated_output.mgf", package = "fioRa")
#'
#' # read as data.frame (return a simplified version)
#' str(fioRa::read_fiora(fl = fl, fmt = "df"))
#'
#' \donttest{
#'   # read as a list (standard case)
#'   fioRa::read_fiora(fl = fl)
#'
#'   # read as Spectra object (requires Spectra-package)
#'   if (requireNamespace("Spectra", quietly = TRUE)) {
#'     fioRa::read_fiora(fl = fl, fmt = "Spectra")
#'    }
#' }
#'
#' @export
#' @seealso [Spectra::Spectra()]
read_fiora <- function(fl, fmt = c("list", "df", "Spectra"), check = TRUE, scale = 0) {
  fmt <- match.arg(fmt)
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
    if (fmt == "df") {
      # keep only spectra information
      mz <- round(as.numeric(s[,1]),4)
      int <- scale_int(x = as.numeric(s[,2]), scale =scale)
      ord <- order(int, decreasing = TRUE)
      s <- paste(mz[ord], int[ord], sep=":", collapse=" ")
    } else {
      if (ncol(s)==3) {
        verify_suggested(pkg = "rcdk")
        s <- as.data.frame(s)
        s[,1:2] <- apply(s[,1:2], 2, as.numeric)
        tmp <- strsplit(s[,3], "//")
        s$adduct <- sapply(tmp, function(z) { z[2] }, USE.NAMES = FALSE)
        s$SMILES <- sapply(tmp, function(z) { z[1] }, USE.NAMES = FALSE)
        s$formula <- smiles2formula(s$SMILES)
        if (check) {
          # remove invalid adducts
          fml_with_add <- sapply(1:nrow(s), function(i) { add_adduct(s[i,"formula"], s[i,"adduct"]) })
          s <- s[!is.na(fml_with_add),]
          # $$ToDo$$
          # - remove mz redundancy for multiple SMILES giving the same mz at different int values
          #browser()
          s <- combine_isomers(s=s)
        }
      }
      s <- s[order(s[,"mz"], decreasing = FALSE),]
      s[,"int"] <- scale_int(x = s[,"int"], scale =scale)
      rownames(s) <- 1:nrow(s)
    }
    c(meta, list("spec" = s))
  })
  out <- stats::setNames(out, unname(sapply(out, function(x) { x$TITLE })))
  if (fmt == "df") {
    out <- ldply_base(out, unlist)
    out[,"PRECURSOR_MZ"] <- round(as.numeric(out[,"PRECURSOR_MZ"]),4)
    out[,"COLLISIONENERGY"] <- as.numeric(out[,"COLLISIONENERGY"])
  }
  if (fmt == "Spectra") {
    out <- convert2Spectra(out)
  }
  return(out)
}
