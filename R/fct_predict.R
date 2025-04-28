#' predict
#'
#' @param Name The compound name.
#' @param SMILES The SMILES code of the compound.
#' @param Precursor_type Precursor type.
#' @param CE Collision energy.
#' @param Instrument_type Instrument type.
#'
#' @description A wrapper around the python fiora model.
#' @return A list with the fiora results for the specified compound.
#' @examples
#' predict()
#'
#' @export
predict <- function(Name = "Example_0", SMILES = "CC1=CC(=O)OC2=CC(OS(O)(=O)=O)=CC=C12", Precursor_type = "[M-H]-", CE = 17, Instrument_type = "HCD") {
  temp_input_file <- tempfile(fileext = ".csv")
  temp_output_file <- gsub("csv$", "mgf", temp_input_file)
  tmp_data <- c("Name,SMILES,Precursor_type,CE,Instrument_type", paste(c(Name,SMILES,Precursor_type,CE,Instrument_type), collapse=","))
  reticulate::py_require("git+https://github.com/BAMeScience/fiora.git")
  cat(tmp_data, file = temp_input_file, append = FALSE, sep = "\n")
  fiora_script <- list.files(path=reticulate::py_config()$virtualenv, pattern="^fiora-predict$", recursive = TRUE, full.names = TRUE)
  command <- reticulate::py_config()$python
  args <- c(fiora_script, paste0('-i \"', temp_input_file, '\"'), paste0('-o \"', temp_output_file, '\"'))
  msg <- system2(command = command, args = args)
  if (msg==0) read_fiora(fl = temp_output_file) else msg
}

