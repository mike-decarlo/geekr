#' Play wave sound
#'
#' Uses Audio to play a sound effect if available
#' @param fname filename of a .wav sound file
#' @importFrom audio play
#' @importFrom audio load.wave
#' @export
play_audio <- function(fname) {
  sfx <- load.wave(fname)
  play(sfx)
}
