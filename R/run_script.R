#' @title Predict MS^2 fragment spectra from SMILES code.
#'
#' @param x A data frame containing columns Name (compound names), SMILES (SMILES
#'     code of the compounds, Precursor_type (currently "[M-H]-" or "[M+H]+"), CE
#'     (Collision energy) and Instrument_type (i.e. HCD).
#' @param min_prob Minimum peak probability to be recorded in the spectrum.
#' @param annotation Return SMILES for fragments if TRUE.
#' @param fiora_script Path to python script fiora-predict.
#' @param fmt Set fmt to 'df' to simplify the return value to a data frame (named
#'     list otherwise).
#'
#' @description A wrapper around the python script `fiora-predict` using the
#'     fiora open source model to generate a MS^2 spectra for a compound with
#'     known SMILES code.
#'
#' @details This wrapper will generate a fiora ready input file (csv-format)
#'     based on the user parameters which is stored as a temp file. It will
#'     ensure that the current version of the fiora package is installed in a
#'     respective python environment. It will use `system2()` to run the python
#'     script `fiora-predict` and import its result back into R using the
#'     internal function `read_fiora()`.
#'     You can try different installed version of `fiora` by providing the path
#'     the the script explicitly.
#'
#' @return A list with the fiora results for the specified compound.
#'
#' @examples
#' \dontrun{
#'   td <- fioRa::test_data
#'   x <- setNames(data.frame(
#'     t(sapply(td[2:11], function(x) { strsplit(x, ",")[[1]] }))),
#'     strsplit(td[1], ",")[[1]]
#'    )
#'   foo <- run_script(x = x)
#'   foo[[1]][["spec"]]
#'   # modify parameters
#'   run_script(x = x[1,,drop=FALSE], min_prob = 0.05)
#'
#'   # use a different fiora environment/model
#'   s_pth <- "c:/Users/jlisec/AppData/Local/r-miniconda/envs/fiora-0.1.0/Scripts/fiora-predict"
#'   foo2 <- run_script(x = x, fiora_script = s_pth)
#'
#'   foo2[[1]][["spec"]]
#'
#'   for (i in 1:length(foo)) {
#'     cat("\n")
#'     print(names(foo)[i])#'
#'     print(foo[[i]][["spec"]])
#'     print(foo2[[i]][["spec"]])
#'   }
#' }
#'
#' @export
run_script <- function(
    x = data.frame(
      Name = "Example_0",
      SMILES = "CC1=CC(=O)OC2=CC(OS(O)(=O)=O)=CC=C12",
      Precursor_type = "[M-H]-",
      CE = 17,
      Instrument_type = "HCD"
    ),
    min_prob = 0.001,
    annotation = FALSE,
    fiora_script = NULL,
    fmt = c("list","df")
) {
  fmt <- match.arg(fmt)
  # get path of python.exe and fiora_predict script
  if (!is.null(fiora_script) && file.exists(fiora_script)) {
    default_path <- dirname(fiora_script)
    script_name <- basename(fiora_script)
  } else {
    # this should work on the shinyserver installation and is the default for 'find_fiora_predict_paths()'
    default_path <- "/home/shiny_test/miniforge3"
    script_name <- "fiora-predict"
  }

  fioRa_pth <- find_fiora_predict_paths(default_path = default_path, script_name = script_name)

  # get input/output files, write data
  temp_input_file <- tempfile(fileext = ".csv")
  temp_output_file <- gsub("csv$", "mgf", temp_input_file)
  stopifnot(is.data.frame(x))
  stopifnot(all(c("Name","SMILES","Precursor_type","CE","Instrument_type") %in% colnames(x)))
  tmp_data <- c(
    "Name,SMILES,Precursor_type,CE,Instrument_type",
    apply(x[,c("Name","SMILES","Precursor_type","CE","Instrument_type"),drop=FALSE], 1, paste, collapse=",")
  )
  cat(tmp_data, file = temp_input_file, append = FALSE, sep = "\n")

  args <- c(paste0('-i \"', temp_input_file, '\"'), paste0('-o \"', temp_output_file, '\"'))
  if (annotation) args <- c(args, "--annotation")
  if (is.numeric(min_prob)) args <- c(args, paste0('--min_prob ', min_prob))

  # process with args
  if (fioRa_pth$os == "Windows") {
    msg <- system2(command = fioRa_pth$python, args = c(fioRa_pth$script, args))
  } else {
    msg <- system2(command = fioRa_pth$script, args = args)
  }

  # read result
  if (msg==0) {
    read_fiora(fl = temp_output_file, fmt = fmt)
  } else {
    msg
  }
}

