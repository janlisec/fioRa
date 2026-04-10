#' @title Plot MS^2 spectrum.
#' @description This is a wrapper around function `InterpretMSSpectrum::PlotSpec()`
#'     allowing to add chemical structures of MS^2 fragments to the plot.
#' @param s A valid spectrum as predicted by fiora.
#' @param show_neutral_losses Connect main peaks by gray lines and annotate the
#'     respective sum formula of the neutral loss.
#' @param show_smiles Overlay SMILES (if present in columns of s) in plot.
#' @param ... Passed on to `InterpretMSSpectrum::PlotSpec()`.
#' @details See examples and documentation of `InterpretMSSpectrum::PlotSpec()`.
#' @examples
#' fl <- system.file("extdata/annotated_output.mgf", package = "fioRa")
#' tmp <- fioRa::read_fiora(fl = fl)
#' s <- tmp[[3]][["spec"]]
#' plot_spec(s = s)
#' plot_spec(s = s, show_neutral_losses = FALSE)
#' plot_spec(s = s, masslab = 0.05, xlim=c(150, 200))
#' @return Creates an annotated plot of a mass spectrum and returns the spectrum invisibly.
#' @seealso [InterpretMSSpectrum::PlotSpec()]
#' @export
#'
plot_spec <- function(s, show_neutral_losses = TRUE, show_smiles = TRUE, ...) {
  verify_suggested(pkg = c("rcdk", "InterpretMSSpectrum"))

  # args to PlotSpec provided by user
  dots <- list(...)

  # Beispiel: xlim ggf. intern setzen, falls nicht vom User vorgegeben
  if ("masslab" %in% names(dots)) {
    masslab <- dots$masslab
    dots$masslab <- NULL
  } else {
    masslab <- 0.01
  }
  if ("cutoff" %in% names(dots)) {
    cutoff <- dots$cutoff
    dots$cutoff <- NULL
  } else {
    cutoff <- 0
  }
  if ("xlim" %in% names(dots)) {
    if (is.null(dots$xlim)) {
      xlim <- range(s[, "mz"], na.rm = TRUE)
    } else {
      xlim <- dots$xlim
    }
    dots$xlim <- NULL
  } else {
    xlim <- range(s[, "mz"], na.rm = TRUE)
  }
  if ("ylim" %in% names(dots)) {
    if (is.null(dots$ylim)) {
      ylim <- c(0, max(s[, "int"], na.rm = TRUE))
    } else {
      ylim <- dots$ylim
    }
    dots$ylim <- NULL
  } else {
    ylim <- c(0, max(s[, "int"], na.rm = TRUE))
  }

  # keep only the SMILES with the highest intensity from a group of SMILES with identical adduct/formula combinations
  s <- combine_isomers(s=s)
  s <- s[order(s[,"mz"]),]
  rownames(s) <- 1:nrow(s)

  # get the peaks with annotatable sum formulas
  flt <- sort(as.numeric(sapply(split(s, s[,"formula"]), function(x) { rownames(x)[which.max(x[,"int"])] })))

  # limit annotation to those above a relative int threshold
  flt <- flt[s[flt,"int"]>=masslab*max(s[flt,"int"])]

  # limit annotation to peaks within plot mz range
  flt <- flt[s[flt,"mz"]>=min(xlim) & s[flt,"mz"]<=max(xlim)]
  flt <- flt[s[flt,"int"]>=min(ylim) & s[flt,"int"]<=max(ylim)]

  # correct formula by adduct information for number of H and filter invalid combinations
  if (length(flt)>=1) {
    for (i in flt) s[i,"formula"] <- add_adduct(s[i,"formula"], s[i,"adduct"])
    flt <- flt[!is.na(s[flt,"formula"])]
    txt <- data.frame("x"=s[flt,"mz"], "txt"=s[flt,"formula"], "expr"=TRUE)
    txt$txt <- sapply(txt$txt, function(x) {
      x <- cce(x = x)
      paste(names(x), sapply(x, function(n) { if(n==1) "" else paste0("[",n,"]")}), sep="", collapse="~")
    })

    # attach SMILES codes for structure plotting
    if (show_smiles && "SMILES" %in% colnames(s)) {
      txt[,"SMILES"] <- s[flt,"SMILES"]
    }
  } else {
    txt <- NULL
  }

  # define neutral_losses
  neutral_losses <- data.frame("Name"="", "Formula"="", "Mass"=0L)
  if (show_neutral_losses && length(flt)>=2) {
    neutral_losses <- get_neutral_loss_df(s = s[flt,])
  }

  # prepare function call
  args <- c(
    list(
      x = s[, 1:2],
      masslab = masslab,
      cutoff = cutoff,
      txt = txt,
      ionization = "ESI",
      neutral_losses = neutral_losses,
      precursor = s[nrow(s), 1],
      xlim = xlim,
      ylim = ylim,
      mar = NULL
    ),
    dots  # remaining ... args to PlotSpec
  )
  do.call(InterpretMSSpectrum::PlotSpec, args)

  invisible(s)
}

