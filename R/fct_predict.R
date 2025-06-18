#' @title Predict MS^2 fragment spectra from SMILES code.
#'
#' @param Name The compound name.
#' @param SMILES The SMILES code of the compound.
#' @param Precursor_type Precursor type (currently "[M-H]-" or "[M+H]+").
#' @param CE Collision energy.
#' @param Instrument_type Instrument type.
#' @param min_prob Minimum peak probability to be recorded in the spectrum.
#' @param annotation Return SMILES for fragments if TRUE.
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
#'
#' @return A list with the fiora results for the specified compound.
#' @examples
#' \dontrun{
#'   predict()
#'   foo <- predict(annotation = TRUE, min_prob=0.01)
#' }#'
#' @export
predict <- function(Name = "Example_0", SMILES = "CC1=CC(=O)OC2=CC(OS(O)(=O)=O)=CC=C12", Precursor_type = "[M-H]-", CE = 17, Instrument_type = "HCD", min_prob = 0.001, annotation = FALSE) {
  temp_input_file <- tempfile(fileext = ".csv")
  temp_output_file <- gsub("csv$", "mgf", temp_input_file)
  tmp_data <- c("Name,SMILES,Precursor_type,CE,Instrument_type", paste(c(Name,SMILES,Precursor_type,CE,Instrument_type), collapse=","))
  cat(tmp_data, file = temp_input_file, append = FALSE, sep = "\n")

  # old version using py_require()
  #reticulate::py_require("git+https://github.com/BAMeScience/fiora.git")
  #fiora_script <- list.files(path=reticulate::py_config()$virtualenv, pattern="^fiora-predict$", recursive = TRUE, full.names = TRUE)
  #command <- reticulate::py_config()$python

  # new version using conda environment
  command <- reticulate::py_config()$python
  fiora_script <- list.files(path=reticulate::py_config()$pythonhome, pattern="^fiora-predict$", recursive = TRUE, full.names = TRUE)

  args <- c(fiora_script, paste0('-i \"', temp_input_file, '\"'), paste0('-o \"', temp_output_file, '\"'))
  if (annotation) args <- c(args, "--annotation")
  if (is.numeric(min_prob)) args <- c(args, paste0('--min_prob ', min_prob))
  msg <- system2(command = command, args = args)
  if (msg==0) read_fiora(fl = temp_output_file) else msg
}

