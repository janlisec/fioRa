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


#' Snapshot selected base graphics parameters
#'
#' @param keys character vector of par names.
#'
#' @return Named list of par values.
#' @keywords internal
#' @noRd
#'
#' @examples
#' grDevices::png(tempfile(), 800, 600)
#' on.exit(grDevices::dev.off(), add = TRUE)
#' before <- par_snapshot()
#' graphics::par(mar = c(2, 2, 0.5, 0) + 0.5)
#' after <- par_snapshot()
#' names(par_diff(before, after))
par_snapshot <- function(keys = c("mar", "mai", "plt", "pin", "usr", "xaxs", "yaxs")) {
  out <- vector("list", length(keys))
  names(out) <- keys
  for (k in keys) {
    out[[k]] <- graphics::par(k)
  }
  out
}

#' Diff two par snapshots
#'
#' @param before snapshot list from par_snapshot()
#' @param after snapshot list from par_snapshot()
#' @param tol numeric tolerance for numeric comparisons
#'
#' @return Named list of changed entries (before/after).
#' @keywords internal
#' @noRd
#'
#' @examples
#' grDevices::png(tempfile(), 800, 600)
#' on.exit(grDevices::dev.off(), add = TRUE)
#' b <- par_snapshot()
#' graphics::par(mar = c(2, 2, 0.5, 0) + 0.5)
#' a <- par_snapshot()
#' par_diff(b, a)
par_diff <- function(before, after, tol = 1e-10) {
  changed <- list()
  for (nm in intersect(names(before), names(after))) {
    b <- before[[nm]]
    a <- after[[nm]]
    same <- FALSE
    if (is.numeric(b) && is.numeric(a) && length(b) == length(a)) {
      same <- all(abs(b - a) < tol)
    } else {
      same <- identical(b, a)
    }
    if (!same) {
      changed[[nm]] <- list(before = b, after = a)
    }
  }
  changed
}
