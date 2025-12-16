#' @title verify_suggested.
#' @description Check if packages are available and stop function otherwise.
#' @param pkg Package names to be checked.
#' @return NULL.
#' @keywords internal
#' @noRd
verify_suggested <- function(pkg) {
  # verify that suggested packages are available
  check_pkg <- sapply(pkg, requireNamespace, quietly = TRUE)
  if (!all(check_pkg)) {
    msg <- paste0(
      "The use of this function requires package", ifelse(sum(!check_pkg)>1, "s", ""),
      paste(names(check_pkg)[!check_pkg], collapse=", "),
      ". Please install."
    )
    stop(msg)
  }
  invisible(NULL)
}

#' @title Calculate Molecular Formula from SMILES with rcdk.
#' @description A small wrapper function to calculate the molecular formula
#'     from SMILES with the rcdk, as recommended. Note: current version does
#'     not handle labeling.
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
    mol <- rcdk::generate.2d.coordinates(mol)
    depictor <- rcdk::get.depictor(width = 200, height = 200, zoom = 1.0, fillToFit = FALSE)
    img <- tryCatch({
      (rcdk::view.image.2d(mol, depictor = depictor))
    }, error = function(e) {
      img <- ""
      message(paste("Invalid SMILES not rendered: ", smiles, sep=""))
    })
    if (length(img)<=2 && kekulise) {
      message("Replotting with kekulise=FALSE")
      mol <- rcdk::parse.smiles(smiles,kekulise=FALSE)[[1]]
      mol <- rcdk::parse.smiles(smiles,kekulise=kekulise)[[1]]
      mol <- rcdk::generate.2d.coordinates(mol)
      depictor <- rcdk::get.depictor(width = coords[3]-coords[1], height = coords[4]-coords[2], zoom = 1.0, fillToFit = FALSE)
      img <- tryCatch({
        (rcdk::view.image.2d(mol, depictor = depictor))
      }, error = function(e) {
        img <- ""
        message(paste("Invalid SMILES not plotted without kekulise either: ", smiles, sep=""))
      })
    }
    if (length(img)>2) {
      # make points of white color transparent
      img[,,4] <- matrix(abs(as.numeric(img[,,1]==1 & img[,,2]==1 & img[,,3]==1)-1), nrow = nrow(img[,,1]))
      # raster image
      #graphics::rasterImage(img, coords[1], coords[2], coords[3], coords[4])
      #browser()
      # compute y-shift according to empty bottom rows
      empty_rows <- apply(img[,,4],1,function(x){all(x==0)})
      strip_height_percent <- 1-(table(diff(which(empty_rows)))[1])/nrow(img[,,4])
      coords[4] <- coords[2]+(coords[4]-coords[2])*strip_height_percent
      # shift one line of text up
      usr <- graphics::par("usr")
      x_range <- usr[2] - usr[1]
      y_range <- usr[4] - usr[3]
      coords[2] <- coords[2]+y_range/30
      coords[4] <- coords[4]+y_range/30

      stripped_img <- img[!empty_rows,,]
      graphics::rasterImage(stripped_img, coords[1], coords[2], coords[3], coords[4])
      #graphics::rect(coords[1], coords[2], coords[3], coords[4])
    }
  }
  invisible(img)
}

#' @title Determine square_subplot_coord.
#' @param x x coordinate.
#' @param y y coordinate.
#' @param w Relative proportion of figure region to cover.
#' @return Returns coordinates for a square shape sub-plot.
#' @noRd
#' @keywords internal
square_subplot_coord <- function(x, y, w = 0.2) {
  # Ermitteln der Abmessungen des plotting device
  device_width <- grDevices::dev.size("in")[1]
  device_height <- grDevices::dev.size("in")[2]

  # Berechnung der Größe des Subplots (w % der kleineren Seite)
  subplot_size <- w * min(device_width, device_height)

  # Ermittlung der user coordinates
  usr <- graphics::par("usr")
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
  x_start <- x - x_len/2
  y_start <- y
  x_end <- x + x_len/2
  y_end <- y + y_len

  # Rückgabe der Koordinaten des Subplots
  return(c(x_start, y_start, x_end, y_end))
}

#' @title add_adduct.
#' @description Add FIORA adduct to formula.
#' @param fml SumFormula.
#' @param ad Adduct.
#' @return SumFormula.
#' @examples
#' add_adduct(fml = "CHO", ad = "[M+2H]+")
#' @keywords internal
#' @noRd
add_adduct <- function(fml, ad) {
  possible_adds <- stats::setNames(c(0,0,-1*(1:3),1:3,-1*(1:3),1:3), gsub("1", "", c("[M]-", "[M]+", paste0("[M-",1:3,"H]-"), paste0("[M+",1:3,"H]-"), paste0("[M-",1:3,"H]+"), paste0("[M+",1:3,"H]+"))))
  if(!(ad %in% names(possible_adds))) {
    message("Dont know this adduct definition", ad)
    stop()
  }
  fml_ad <- cce(fml)
  if (possible_adds[ad]!=0) {
    fml_ad["H"] <- fml_ad["H"]+possible_adds[ad]
  }
  fml_ad <- paste(names(fml_ad), fml_ad, sep="", collapse="")
  return(fml_ad)
}

#' @title cce.
#' @description CountChemicalElements.
#' @param x x.
#' @param ele ele.
#' @return CountChemicalElements.
#' @keywords internal
#' @noRd
cce <- InterpretMSSpectrum::CountChemicalElements

#' @title Plot MS^2 spectrum.
#' @description This function...
#' @param s A valid spectrum as predicted by fiora.
#' @param show_neutral_losses show_neutral_losses.
#' @param ... Passed on to InterpretMSSpectrum::PlotSpec().
#' @details More information about aromaticity: \url{https://github.com/CDK-R/cdkr/issues/49}
#' @examples
#' fl <- system.file("extdata/annotated_output.mgf", package = "fioRa")
#' tmp <- fioRa::read_fiora(fl = fl)
#' s <- tmp[[1]][["spec"]]
#' plot_spec(s = s)
#' @return Returns a plot.
#' @noRd
#' @keywords internal
plot_spec <- function(s, show_neutral_losses = TRUE, ...) {
  verify_suggested(pkg = c("rcdk", "InterpretMSSpectrum"))
  #flt <- which(s[,"int"] > 0.05*max(s[,"int"], na.rm=T))
  rownames(s) <- 1:nrow(s)
  flt <- sort(as.numeric(sapply(split(s, s[,"formula"]), function(x) { rownames(x)[which.max(x[,"int"])] })))
  # correct formula by adduct information for number of H
  for (i in flt) s[i,"formula"] <- add_adduct(s[i,"formula"], s[i,"adduct"])
  txt <- data.frame("x"=s[flt,"mz"], "txt"=s[flt,"formula"], "expr"=TRUE)
  txt$txt <- sapply(txt$txt, function(x) {
    x <- cce(x = x)
    paste(names(x), sapply(x, function(n) { if(n==1) "" else paste0("[",n,"]")}), sep="", collapse="~")
  })
  neutral_losses <- NULL
  if (show_neutral_losses && length(flt)>=2) {
    ele_precursor <- cce(s[flt[length(flt)],"formula"])
    nl <- unname(sapply(s[flt[-length(flt)],"formula"], function(x) {
      x <- ele_precursor-cce(x, ele = names(ele_precursor))
      x <- x[x>0]
      paste(names(x), sapply(x, function(n) { if (n==1) "" else n }), sep="", collapse="")
      #paste(names(x), sapply(x, function(n) { if(n==1) "" else paste0("[",n,"]")}), sep="", collapse="~")
      #paste(names(x), x, sep="", collapse="")
    }))
    neutral_losses <- data.frame("Name"=nl, "Formula"=nl, "Mass"=s[flt[length(flt)],"mz"]-s[flt[-length(flt)],"mz"])
  }
  InterpretMSSpectrum::PlotSpec(x=s[,1:2], masslab = 0.01, cutoff = 0, txt = txt, ionization="ESI", neutral_losses = neutral_losses, precursor = s[nrow(s),1], ...)
  for (i in flt) {
    renderSMILES(smiles = s[i,"SMILES"], coords = square_subplot_coord(x = s[i,"mz"], y = s[i,"int"], w = 0.2))
  }
}

#' @title convert2Spectra.
#' @description Convert a FIORA result to Spectra format.
#' @param sp FIORA result with Annotation=TRUE.
#' @return A named list.
#' @keywords internal
#' @noRd
convert2Spectra <- function(sp) {
  verify_suggested(pkg = c("S4Vectors", "Spectra"))
  sp.l <- length(sp)
  msLevel <- rep(2L, sp.l)
  centroided <- rep(TRUE, sp.l)
  adduct <- sapply(1:sp.l, FUN = function(x) {sp[[x]]$PRECURSORTYPE})
  polarity <- as.integer(grepl("]+", adduct, fixed = TRUE))
  name <- sapply(1:sp.l, FUN = function(x) {sp[[x]]$TITLE})
  smiles <- sapply(1:sp.l, FUN = function(x) {sp[[x]]$SMILES})
  formula <- sapply(1:sp.l, FUN = function(x) {sp[[x]]$FORMULA})
  precursorMz <- as.numeric(sapply(1:sp.l, FUN = function(x) {sp[[x]]$PRECURSOR_MZ}))
  collisionEnergy <- as.integer(as.numeric(sapply(1:sp.l, FUN = function(x) {sp[[x]]$COLLISIONENERGY})))
  collisionMethod <- sapply(1:sp.l, FUN = function(x) {sp[[x]]$INSTRUMENTTYPE})

  spd <- S4Vectors::DataFrame(
    msLevel,
    centroided,
    adduct,
    polarity,
    name,
    smiles,
    formula,
    precursorMz,
    collisionEnergy,
    collisionMethod
  )

  ## Assign m/z and intensity values.
  spd$mz <- lapply(1:sp.l, FUN = function(x) sp[[x]]$spec$mz)
  spd$intensity <- lapply(1:sp.l, FUN = function(x) sp[[x]]$spec$int)
  spd$fragment.smiles <- lapply(1:sp.l, FUN = function(x) sp[[x]]$spec$SMILES)
  spd$fragment.adduct <- lapply(1:sp.l, FUN = function(x) sp[[x]]$spec$adduct)
  spd$fragment.formula <- lapply(1:sp.l, FUN = function(x) sp[[x]]$spec$formula)

  return(Spectra::Spectra(spd))
}

#' @title find_fiora_predict_paths.
#' @description Determin current OS, python installation path and script path and return all 3 as a named list.
#' @param default_path Default base path of python installation.
#' @param script_name script_name.
#' @return A named list.
#' @keywords internal
#' @noRd
find_fiora_predict_paths <- function(
    default_path = "/home/shiny_test/miniforge3",
    script_name = "fiora-predict"
) {
  # check if path is valid
  is_valid_path <- function(path) { !is.null(path) && file.exists(path) }

  # current OS
  os <- Sys.info()[["sysname"]]

  # if default_path is invalid try using reticulate::miniconda_path()
  if (!is_valid_path(default_path)) {
    verify_suggested("reticulate")
    default_path <- reticulate::miniconda_path()
    if (!is_valid_path(default_path)) {
      stop("No valid 'default_path' provided and no valid reticulate::miniconda_path")
    } else {
      message("No valid 'default_path' provided, using reticulate::miniconda_path '", default_path, "'.")
    }
  }

  if (os == "Windows") {
    # check if default path includes specific environment already and guess environment if not
    win_path_ele <- strsplit(normalizePath(default_path), "[\\]")[[1]]
    if ("envs" %in% win_path_ele && length(win_path_ele)>which(win_path_ele == "envs")) {
      default_path <- do.call(file.path, as.list(win_path_ele[1:(which(win_path_ele == "envs")+1)]))
    } else {
      default_path <- file.path(default_path, "envs", "fiora")
    }
    python_path <- file.path(default_path, "python.exe")
    script_path <- file.path(default_path, "Scripts", script_name)
  } else {
    if (!grepl("envs", default_path)) { default_path <- file.path(default_path, "envs", "fiora", "bin") }
    python_path <- file.path(default_path, "python")
    script_path <- file.path(default_path, script_name)
  }

  return(list(
    "os" = os,
    "python" = normalizePath(python_path, mustWork = FALSE),
    "script" = normalizePath(script_path, mustWork = FALSE)
  ))
}

#' @title ldply_base
#' @param .data list.
#' @param .fun fun.
#' @keywords internal
#' @noRd
ldply_base <- function(.data, .fun = identity) {
  result <- lapply(.data, .fun)
  df <- do.call(rbind, result)
  df <- data.frame(df, row.names = NULL, check.names = FALSE)
  return(df)
}

#' @title estimateSelectWidth
#' @param choices choices
#' @param min_width min_width
#' @param max_width max_width
#' @param px_per_char px_per_char
#' @examples
#' choices <- NULL
#' estimateSelectWidth(choices)
#' estimateSelectWidth(choices, min_width = 80)
#' choices <- c("", "A", "very long choice")
#' estimateSelectWidth(choices, px_per_char = 12)
#' estimateSelectWidth(choices, px_per_char = 12, max_width = 200)
#'
#' @keywords internal
#' @noRd
estimateSelectWidth <- function(choices, min_width = 120, max_width = 300, px_per_char = 8) {
  if (length(choices) == 0) return(paste0(min_width, "px"))
  max_chars <- max(nchar(choices), na.rm = TRUE)
  estimated <- 10 + 16 + (max_chars * px_per_char)
  width <- min(max(estimated, min_width), max_width)
  paste0(width, "px")
}

#' @title next_choice
#' @param current current
#' @param choices choices
#' @param direction direction
#' @keywords internal
#' @noRd
next_choice <- function(current, choices, direction = c("up", "down")) {
  direction <- match.arg(direction)
  index <- match(current, choices)

  if (is.na(index)) return(current)  # current not in choices

  if (direction == "up" && index > 1) {
    return(choices[index - 1])
  }
  if (direction == "down" && index < length(choices)) {
    return(choices[index + 1])
  }

  return(current)
}

#' @title is_valid_path
#' @param x x.
#' @keywords internal
#' @noRd
is_valid_path <- function(x) {
  if (!is.character(x) || length(x) != 1L || is.na(x)) return(FALSE)
  if (!nzchar(x) || grepl("^\\s+$", x)) return(FALSE)
  tryCatch({
    suppressWarnings(normalizePath(x, winslash = "/", mustWork = FALSE))
    TRUE
  }, error = function(e) FALSE)
}
