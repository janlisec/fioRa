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
      "The use of this function requires package", ifelse(sum(!check_pkg)>1, "s ", " "),
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
#' smiles <- c("CCO", "OC(=O)C", "OS(=O)(=O)c1ccc(cc1)C(CC(=O)O)CC(=O)O")
#' smiles2formula(smiles = smiles)
#' @return Returns the molecular formula only (the \code{rcdk} function contains additional text).
#' @noRd
#' @keywords internal
smiles2formula <- function(smiles) {
  mol <- rcdk::parse.smiles(smiles)
  # not sure if function rcdk::convert.implicit.to.explicit really is required at some point
  #mapply(rcdk::convert.implicit.to.explicit, mol)
  return(sapply(1:length(mol), function(i) { rcdk::get.mol2formula(mol[[i]], charge=0)@string }))
}
# smiles2formula_old <- function(smiles) {
#   mol <- rcdk::parse.smiles(smiles)[[1]]
#   #rcdk::convert.implicit.to.explicit(mol)
#   formula <- rcdk::get.mol2formula(mol, charge=0)
#   return(formula@string)
# }
# microbenchmark::microbenchmark(
#   "new" = smiles2formula(smiles = smiles),
#   "old" = sapply(smiles, smiles2formula_old)
# )

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
#' @param xx The desired x center position (to potentially shift the subplot to).
#' @author Emma Schymanski <emma.schymanski@@uni.lu>
#' @details More information about aromaticity: \url{https://github.com/CDK-R/cdkr/issues/49}
#' @examples
#' smiles <- "OS(=O)(=O)c1ccc(cc1)C(CC(=O)O)CC(=O)O"
#' plot.new()
#' plot.window(xlim=c(0,200), ylim=c(0,100))
#' renderSMILES(smiles,kekulise=FALSE)
#' renderSMILES(smiles,kekulise=TRUE)
#' renderSMILES(smiles, coords = c(100,0,150,50))
#' rect(100,0,150,50)
#' @return Returns an image for use during plotting
#' @noRd
#' @keywords internal
renderSMILES <- function(smiles, kekulise=TRUE, coords=c(0,0,100,100), xx = NULL) {
  if (nchar(smiles)>1) {
    mol <- rcdk::parse.smiles(smiles,kekulise=kekulise)[[1]]
    mol <- rcdk::generate.2d.coordinates(mol)
    #depictor <- rcdk::get.depictor(width = 200, height = 200, zoom = 1.0, fillToFit = FALSE)
    depictor <- rcdk::get.depictor(width = 300, height = 300, zoom = 1.0, fillToFit = FALSE)
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
      # compute y-shift according to empty bottom rows
      empty_rows <- apply(img[,,4],1,function(x){all(x==0)})
      strip_height_percent <- 1-(table(diff(which(empty_rows)))[1])/nrow(img[,,4])
      if (mean(coords[c(2,4)]) < diff(graphics::par("usr")[c(3:4)])/2) {
        coords[4] <- coords[2]+(coords[4]-coords[2])*strip_height_percent
      } else {
        coords[2] <- coords[4]-(coords[4]-coords[2])*strip_height_percent
      }
      stripped_img <- img[!empty_rows,,]

      # empty cols
      empty_cols <- apply(stripped_img[, , 4], 2, function(x) { all(x == 0) })
      if (!all(empty_cols)) {
        strip_width_percent <- sum(!empty_cols) / length(empty_cols)
        x_center <- mean(coords[c(1, 3)])
        half_width_new <- (coords[3] - coords[1]) * strip_width_percent / 2
        coords[1] <- x_center - half_width_new
        coords[3] <- x_center + half_width_new
        if (!is.null(xx) && abs(xx-x_center)>diff(coords[c(1,3)])/10) {
          shft <- xx - x_center
          coords[1] <- coords[1] + shft
          coords[3] <- coords[3] + shft
          # check borders
          gp_usr <- graphics::par("usr")
          if (coords[1] < gp_usr[1]) {
            shft <- gp_usr[1] - coords[1]
            coords[1] <- coords[1] + shft
            coords[3] <- coords[3] + shft
          }
          if (coords[3] > gp_usr[2]) {
            shft <- gp_usr[2] - coords[3]
            coords[1] <- coords[1] + shft
            coords[3] <- coords[3] + shft
          }
        }
        stripped_img <- stripped_img[, !empty_cols, , drop = FALSE]
      }

      graphics::rasterImage(stripped_img, coords[1], coords[2], coords[3], coords[4])
      #graphics::rect(coords[1], coords[2], coords[3], coords[4])
    }
  }
  invisible(img)
}

#' @title Determine square_subplot_coord.
#' @description Compute square subplot coordinates in user space
#' @param x x center coordinate of square.
#' @param y y center coordinate of square.
#' @param xlim numeric(2); xlim of spec.
#' @param ylim numeric(2); ylim of spec.
#' @param w Relative proportion of figure region to cover.
#' @return Returns coordinates for a square shape sub-plot.
#' @noRd
#' @keywords internal
#'
square_subplot_coord <- function(x, y, xlim = NULL, ylim = NULL, w = 0.2) {

  if (is.null(xlim)) xlim <- graphics::par("usr")[1:2]
  if (is.null(ylim)) ylim <- graphics::par("usr")[3:4]

  ## device size in inch
  dev <- grDevices::dev.size("in")
  device_width  <- dev[1]
  device_height <- dev[2]

  if (!all(is.finite(c(device_width, device_height)))) {
    device_width  <- 1
    device_height <- 1
  }

  ## subplot size in inch
  subplot_size_in <- w * min(device_width, device_height)

  ## ranges
  x_range <- xlim[2] - xlim[1]
  y_range <- ylim[2] - ylim[1]

  ## subplot size in user coords
  x_len <- subplot_size_in * x_range / device_width
  y_len <- subplot_size_in * y_range / device_height

  ## target center adjustment based on location:
  ## Compute approx text-line height in user coords
  text_in <- graphics::par("cin")[2] * graphics::par("cex")  # text height in inch
  text_user <- 2 * text_in * (y_range / device_height)

  ## shift direction
  mid_y <- mean(ylim)
  if (y < mid_y) {
    ## target is in lower half, shift subplot up
    y <- y + text_user
  } else {
    ## target in upper half above, shift down
    y <- y - text_user - y_len
  }

  ## keep inside bounds
  if (x + x_len/2 > xlim[2]) x <- xlim[2] - x_len/2
  if (x - x_len/2 < xlim[1]) x <- xlim[1] + x_len/2
  if (y + y_len > ylim[2])   y <- ylim[2] - y_len
  if (y < ylim[1])           y <- ylim[1]

  ## final coords
  x_start <- x - x_len/2
  x_end   <- x + x_len/2
  y_start <- y
  y_end   <- y + y_len

  c(x_start, y_start, x_end, y_end)
}


#' @title add_adduct.
#' @description Add FIORA adduct to formula.
#' @param fml SumFormula.
#' @param ad Adduct.
#' @return SumFormula.
#' @examples
#' add_adduct(fml = "CHO", ad = "[M+2H]+")
#' add_adduct(fml = "HF", ad = "[M-H]-")
#' add_adduct(fml = "HF", ad = "[M-2H]-")
#' @keywords internal
#' @noRd
add_adduct <- function(fml, ad) {
  possible_adds <- stats::setNames(c(0,0,-1*(1:3),1:3,-1*(1:3),1:3), gsub("1", "", c("[M]-", "[M]+", paste0("[M-",1:3,"H]-"), paste0("[M+",1:3,"H]-"), paste0("[M-",1:3,"H]+"), paste0("[M+",1:3,"H]+"))))
  if(!(ad %in% names(possible_adds))) {
    message("Dont know this adduct definition ", ad)
    stop()
  }
  fml_ad <- cce(fml)
  # initialize H=0 if not present in formula
  if (!("H" %in% names(fml_ad))) fml_ad["H"] <- 0
  if (possible_adds[ad]!=0) {
    if (fml_ad["H"]+possible_adds[ad] < 0) {
      message("Adduct definition and formula do not match (insufficient number of H); fml = ", fml, "; adduct = ", ad)
      return(NA)
    } else {
      fml_ad["H"] <- fml_ad["H"]+possible_adds[ad]
      # omit H=0 definitions
      if (fml_ad["H"]==0 & length(fml_ad)>=2) fml_ad <- fml_ad[names(fml_ad)!="H"]
    }
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

#' @title scale_int.
#' @description Scale an intensity vector of a mass spectrum.
#' @param x Numeric vector x.
#' @param scale scale.
#' @param d Rounding precision digits (if NULL, determined from scale).
#' @return Scaled intensity.
#' @keywords internal
#' @noRd
scale_int <- function(x, scale = 0, d = NULL) {
  if (is.numeric(scale) && scale>0) {
    x <- scale*x/max(x, na.rm=TRUE)
    if (is.null(d)) d <- ifelse(scale>=100, 0, 4)
    x <- round(x, digits = d)
  }
  return(x)
}

#' @title combine_isomers.
#' @description Keep only the SMILES with the highest intensity from a group of
#'     SMILES with identical adduct/formula combinations (isomers).
#' @param s Spectrum with columns 'int', 'adduct' and 'formula'.
#' @return Spectrum with combined isomers.
#' @keywords internal
#' @noRd
combine_isomers <- function(s) {
  ldply_base(split(s, s[,"formula"]), function(x) {
    ldply_base(split(x, x[,"adduct"]), function(y) {
      y[,"n_isomers"] <- 1
      if (nrow(y)>=2) {
        y[,"n_isomers"] <- nrow(y)
        int_sum <- sum(y[,"int"])
        y <- y[which.max(y[,"int"]),]
        y[,"int"] <- int_sum
      }
      return(y)
    })
  })
}

#' @title get_neutral_loss_df.
#' @description Keep only the SMILES with the highest intensity from a group of
#'     SMILES with identical adduct/formula combinations (isomers).
#' @param s Spectrum with columns 'int', 'adduct' and 'formula'.
#' @return Spectrum with combined isomers.
#' @keywords internal
#' @noRd
get_neutral_loss_df <- function(s) {
  flt <- 1:nrow(s)
  if (FALSE) {
    # this is the (old) version to show neutral losses just with respect to the precursor
    ele_precursor <- cce(s[flt[length(flt)],"formula"])
    nl <- unname(sapply(s[flt[-length(flt)],"formula"], function(x) {
      x <- ele_precursor-cce(x, ele = names(ele_precursor))
      x <- x[x>0]
      paste(names(x), sapply(x, function(n) { if (n==1) "" else n }), sep="", collapse="")
    }))
    neutral_losses <- data.frame("Name"=nl, "Formula"=nl, "Mass"=s[flt[length(flt)],"mz"]-s[flt[-length(flt)],"mz"])
  }
  # this is the (new) version to show neutral losses between all peaks
  eles <- stats::setNames(lapply(s[flt,"formula"], cce, ele = names(cce(s[flt[length(flt)],"formula"]))), flt)
  neutral_losses <- ldply_base(flt[-length(flt)], function(x) {
    ldply_base(flt[flt>x], function(y) {
      sm <- eles[[as.character(x)]]
      mm <- eles[[as.character(y)]]
      if (all(mm >= sm)) {
        nl <- mm-sm
        nl <- nl[nl>0]
        nl <- paste(names(nl), sapply(unname(nl),function(z){ifelse(z>1,z,"")}), sep="", collapse="")
        data.frame("Name"=nl, "Formula"=nl, "Mass"=s[y,"mz"]-s[x,"mz"])
      } else {
        NULL
      }
    })
  })
  return(neutral_losses)
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
#' @param verbose Set to FALSE to omit messages.
#' @return A named list.
#' @keywords internal
#' @noRd
find_fiora_predict_paths <- function(
    default_path = "/home/shiny_test/miniforge3",
    script_name = "fiora-predict",
    verbose = TRUE
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
      if (verbose) message("No valid 'default_path' provided, using reticulate::miniconda_path '", default_path, "'.")
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
