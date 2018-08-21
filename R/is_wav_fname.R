#' Determine if file is a .wav
#' 
#' Checks to see if the filename supplied is a .wav file
#' @param fname filename of a .wav sound file
#' @importFrom stringr str_detect
#' @export
is_wav_fname <- function(fname) {
  str_detect(fname, "\\.wav$")
}
