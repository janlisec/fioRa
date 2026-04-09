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
#' plot_spec(s = s, masslab = 0.05)
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
  # limit to those above a relative int threshold
  flt <- flt[s[flt,"int"]>=masslab*max(s[flt,"int"])]
  # correct formula by adduct information for number of H and filter invalid combinations
  for (i in flt) s[i,"formula"] <- add_adduct(s[i,"formula"], s[i,"adduct"])
  flt <- flt[!is.na(s[flt,"formula"])]
  txt <- data.frame("x"=s[flt,"mz"], "txt"=s[flt,"formula"], "expr"=TRUE)
  txt$txt <- sapply(txt$txt, function(x) {
    x <- cce(x = x)
    paste(names(x), sapply(x, function(n) { if(n==1) "" else paste0("[",n,"]")}), sep="", collapse="~")
  })
  neutral_losses <- data.frame("Name"="", "Formula"="", "Mass"=0L)
  if (show_neutral_losses && length(flt)>=2) {
    neutral_losses <- get_neutral_loss_df(s = s[flt,])
  }


  # set "mar" similar to what PlotSpec does to minimize/eliminate on.exit effects
  opar_mar <- graphics::par("mar")
  on.exit(graphics::par("mar" = opar_mar), add = TRUE)
  graphics::par("mar" = c(2, 2, 0.5, 0) + 0.5)

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
      ylim = ylim
    ),
    dots  # remaining ... args to PlotSpec
  )

  # b <- par_snapshot()
  do.call(InterpretMSSpectrum::PlotSpec, args)
  # a <- par_snapshot()
  # print(par_diff(b, a))
  #
  # message("mfg: ", paste(graphics::par("mfg"), collapse = ", "))
  # message("usr: ", paste(signif(graphics::par("usr"), 4), collapse = ", "))

  if (show_smiles && "SMILES" %in% colnames(s)) {

    # # force user coordinates
    # graphics::par(new = TRUE)
    # graphics::plot.window(xlim = xlim, ylim = ylim)

    for (i in flt) {
      if (s[i,"mz"]>=xlim[1] & s[i,"mz"]<=xlim[2] & s[i,"int"]>=ylim[1] & s[i,"int"]<=ylim[2]) {
        gp_usr <- c(xlim+c(-1,1)*0.039*diff(xlim), ylim+c(-1,1)*0.039*diff(ylim))
        coords <- square_subplot_coord(x = s[i,"mz"], y = s[i,"int"], gp_usr = gp_usr, w = 0.25)
        renderSMILES(smiles = s[i,"SMILES"], coords = coords, gp_usr = gp_usr)
      }
    }
  }

  # b <- par_snapshot()
  # print(par_diff(a, b))

  # force user coordinates
  graphics::par(new = TRUE)
  graphics::plot.window(xlim = xlim, ylim = ylim)

  invisible(s)
}

