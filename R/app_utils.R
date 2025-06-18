#' @title Calculate Molecular Formula from SMILES with rcdk.
#' @description A small wrapper function to calculate the molecular formula
#' from SMILES with the rcdk, as recommended. Note: current version does not handle labelling.
#' @param smiles Valid SMILES code used to calculate the formula.
#' @author Emma Schymanski <emma.schymanski@@uni.lu>
#' @examples
#' smiles2formula(smiles = "OC(=O)C")
#' @return Returns the molecular formula only (the \code{rcdk} function contains additional text).
#' @noRd
#' @keywords internal
smiles2formula <- function(smiles) {
  mol <- rcdk::parse.smiles(smiles)[[1]]
  rcdk::convert.implicit.to.explicit(mol)
  formula <- rcdk::get.mol2formula(mol, charge=0)
  return(formula@string)
}

#' @title Render SMILES into 2D image for plotting via rcdk.
#' @description This function uses the rcdk to parse the smiles into a mol, with
#'     options to switch kekulise (aromaticity detection) on or off and to define
#'     desired coordinates. Output requires that plot.new has been called, i.e.
#'     this is designed to be used directly during plotting.
#'     This function uses default depiction options.
#' @param smiles A valid SMILES code for rendering (e.g. \code{"c1ccccc1"}).
#' @param kekulise If \code{TRUE}, performs CDK aromaticiy detection, which is
#'     recommended. Setting \code{FALSE} can force rendering of invalid SMILES
#'     with undefined bond orders. Older rcdk versions may behave differently.
#' @param coords This is used to control the size of the image within the plot.
#'     Values \code{c(xmin,ymin,xmax,ymax)}.
#' @author Emma Schymanski <emma.schymanski@@uni.lu>
#' @details More information about aromaticity: \url{https://github.com/CDK-R/cdkr/issues/49}
#' @examples
#' smiles <- "OS(=O)(=O)c1ccc(cc1)C(CC(=O)O)CC(=O)O"
#' plot.new()
#' plot.window(xlim=c(0,200), ylim=c(0,100))
#' renderSMILES(smiles,kekulise=FALSE)
#' renderSMILES(smiles,kekulise=TRUE)
#' @return Returns an image for use during plotting
#' @noRd
#' @keywords internal
renderSMILES <- function(smiles, kekulise=TRUE, coords=c(0,0,100,100)) {
  if (nchar(smiles)>1) {
    mol <- rcdk::parse.smiles(smiles,kekulise=kekulise)[[1]]
    img <- tryCatch({
      (rcdk::view.image.2d(mol))
    }, error = function(e) {
      img <- ""
      message(paste("Invalid SMILES not rendered: ", smiles, sep=""))
    })
    if (length(img)<=2 && kekulise) {
      message("Replotting with kekulise=FALSE")
      mol <- rcdk::parse.smiles(smiles,kekulise=FALSE)[[1]]
      img <- tryCatch({
        (rcdk::view.image.2d(mol))
      }, error = function(e) {
        img <- ""
        message(paste("Invalid SMILES not plotted without kekulise either: ", smiles, sep=""))
      })
    }
    if (length(img)>2) {
      # make points of white color transparent
      img[,,4] <- matrix(abs(as.numeric(img[,,1]==1 & img[,,2]==1 & img[,,3]==1)-1), nrow = nrow(img[,,1]))
      # raster image
      graphics::rasterImage(img, coords[1], coords[2], coords[3], coords[4])
    }
  }
  invisible(img)
}

#' @title Plot MS^2 spectrum.
#' @description This function...
#' @param s A valid spectrum as predicted by fiora.
#' @param n Code defining the annotation.
#' @param ... Further parameters to `plot()`.
#' @details More information about aromaticity: \url{https://github.com/CDK-R/cdkr/issues/49}
#' @examples
#' fl <- system.file("extdata/annotated_output.mgf", package = "fioRa")
#' tmp <- fioRa:::read_fiora(fl = fl)
#' s <- tmp[[1]][["spec"]]
#' plot_spec(s = s, n=-1)
#' @return Returns a plot.
#' @noRd
#' @keywords internal
# plot_spec_old <- function(s, n = NA, ...) {
#   x = s[,"mz"]
#   y = s[,"int"]
#   plot(x = x, y = y, type="h", xlab="m/z", ylab="Int (relative)", ...)
#   if (!is.na(n)) {
#     if (n>=0) {
#       # annotate mz and round to n precision
#       flt <- y > 0.05*max(y, na.rm=T) & y < 0.9*max(y, na.rm=T)
#       graphics::text(x = x[flt], y = y[flt], labels=round(x[flt],n), adj=c(0.5,0), col=4)
#       flt <- y > 0.9*max(y, na.rm=T)
#       graphics::text(x = x[flt], y = y[flt], labels=round(x[flt],n), adj=c(0.5,1), col=4)
#     }
#     if (n==-1) {
#       flt <- y > 0.05*max(y, na.rm=T)
#       graphics::text(x = x[flt], y = y[flt], labels=s[flt,"formula"], adj=c(0.5,0), col=4)
#     }
#   }
# }
#
plot_spec <- function(s, n = NA, ...) {
  neutral_losses <- data.frame("Name"="test", "Formula"="SO3", "Mass"=79.956)
  flt <- s[,"int"] > 0.05*max(s[,"int"], na.rm=T)
  txt <- data.frame("x"=s[flt,"mz"], "txt"=s[flt,"formula"],"expr"=TRUE)
  txt$txt <- sapply(txt$txt, function(x) {
    x <- InterpretMSSpectrum::CountChemicalElements(x = x)
    paste(names(x), sapply(x, function(n) { if(n==1) "" else paste0("[",n,"]")}), sep="", collapse="~")
  })
  # opar <- par(no.readonly = TRUE)
  # on.exit(graphics::par(cex = opar$cex))
  # par("cex"=1.25)
  InterpretMSSpectrum::PlotSpec(x=s[,1:2], cutoff = 0, txt = txt, neutral_losses = neutral_losses, masslab = 0.01)
  for (i in which(flt)) {
    renderSMILES(smiles = s[i,"SMILES"], coords = square_subplot_coord(x = s[i,"mz"], y = s[i,"int"]))
  }
}

#' @title Determine square_subplot_coord.
#' @param x x coordinate.
#' @param y y coordinate.
#' @param w Relative proportion of figure region to cover.
#' @return Returns coordinates for a squqre shape sub-plot.
#' @noRd
#' @keywords internal
square_subplot_coord <- function(x, y, w = 0.2) {
  # Ermitteln der Abmessungen des plotting device
  device_width <- grDevices::dev.size("in")[1]
  device_height <- grDevices::dev.size("in")[2]

  # Berechnung der Größe des Subplots (10% der kleineren Seite)
  subplot_size <- w * min(device_width, device_height)

  # Ermittlung der user coordinates
  usr <- par("usr")
  x_range <- usr[2] - usr[1]
  y_range <- usr[4] - usr[3]
  x_len <- subplot_size * x_range / device_width
  y_len <- subplot_size * y_range / device_height

  # Überprüfen, ob subplot innerhalb des Plots liegt und verschieben wenn nötig
  if (x + x_len/2 > usr[2]) {
    x <- usr[2] - x_len/2
  }
  if (y + y_len > usr[4]) {
    y <- usr[4] - y_len
  }

  # Berechnung der Position des Subplots
  x_len <- subplot_size * x_range / device_width
  x_start <- x - x_len/2
  y_start <- y
  x_end <- x + x_len/2
  y_end <- y + y_len

  # Rückgabe der Koordinaten des Subplots
  return(c(x_start, y_start, x_end, y_end))
}

#' @title Determine python and firo installation status.
#' @param silent TRUE.
#' @return Returns NULL.
#' @noRd
#' @keywords internal
check_fiora_python_installation <- function(silent = TRUE) {
  if (!tryCatch(file.exists(reticulate::conda_binary()), error = function(e) FALSE)) {
    reticulate::install_miniconda(path = reticulate::miniconda_path(), update = FALSE, force = FALSE)
  }
  if (reticulate::condaenv_exists(envname = "fiora", conda = "auto")) {
    if (!silent) message("A python conda environment named 'fiora' was found and will be used.")
  } else {
    if (!silent) message("A python conda environment named 'fiora' will be installed...")
    reticulate::conda_create("fiora")
    reticulate::conda_install(envname = "fiora", packages = "git+https://github.com/BAMeScience/fiora.git", pip = TRUE, channel = "defaults")
  }
  reticulate::use_condaenv(condaenv = "fiora", conda = "auto", required = NULL)
  invisible(NULL)
}
