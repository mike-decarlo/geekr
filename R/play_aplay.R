#' Use Aplay to play sound
#' 
#' Uses Aplay player if available
#' @param fname filename of a .wav sound file
#' @export
play_aplay <- function(fname) {
  system(
    paste(
      "aplay --buffer-time=48000 -N -q"
      , fname
      )
    , ignore.stdout = TRUE
    , ignore.stderr = TRUE
    ,wait = FALSE
    )
  invisible(NULL)
}
