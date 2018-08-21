#' Use VLC to play sound
#' 
#' Uses VLC player if available
#' @param fname filename of a .wav sound file
#' @export
play_vlc <- function(fname) {
  system(
    paste0(
      "vlc -Idummy --no-loop --no-repeat --playlist"
      , "-autostart --no-media-library --play-and-exit"
      , fname
      )
    , ignore.stdout = TRUE
    , ignore.stderr = TRUE
    , wait = FALSE
    )
  invisible(NULL)
}
