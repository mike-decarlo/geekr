#' Use Paplay to play sound
#' 
#' Uses Paplay player if available
#' @param fname filename of a .wav sound file
#' @export
play_paplay <- function(fname) {
  system(
    paste(
      "paplay "
      , fname
      )
    , ignore.stdout = TRUE
    , ignore.stderr = TRUE
    , wait = FALSE
    )
  invisible(NULL)
}
