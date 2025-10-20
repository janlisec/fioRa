#' @title Predict MS^2 fragment spectra from SMILES code.
#'
#' @param Name The compound name.
#' @param SMILES The SMILES code of the compound.
#' @param Precursor_type Precursor type (currently "[M-H]-" or "[M+H]+").
#' @param CE Collision energy.
#' @param Instrument_type Instrument type.
#' @param min_prob Minimum peak probability to be recorded in the spectrum.
#' @param annotation Return SMILES for fragments if TRUE.
#' @param fiora_script Path to python script fiora-predict.
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
#'   run_script()
#'   # modify parameters and store result
#'   foo <- run_script("Name" = "Test", min_prob = 0.05)
#'   # use a different fiora environment/model
#'   s_pth <- "c:/Users/jlisec/AppData/Local/r-miniconda/envs/fiora-0.1.0/Scripts/fiora-predict"
#'   foo <- run_script(fiora_script = s_pth)
#' }
#'
#' @export
run_script <- function(Name = "Example_0", SMILES = "CC1=CC(=O)OC2=CC(OS(O)(=O)=O)=CC=C12", Precursor_type = "[M-H]-", CE = 17, Instrument_type = "HCD", min_prob = 0.001, annotation = FALSE, fiora_script = NULL) {
  # get path of python.exe and fiora_predict script
  #fiora_script <- "c:/Users/jlisec/AppData/Local/r-miniconda/envs/fiora-conda/Scripts/fiora-predict"
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
  tmp_data <- c("Name,SMILES,Precursor_type,CE,Instrument_type", paste(c(Name,SMILES,Precursor_type,CE,Instrument_type), collapse=","))
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
  if (msg==0) read_fiora(fl = temp_output_file) else msg
}

