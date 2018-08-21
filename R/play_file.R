#' Controller to play sound
#' 
#' Uses system appropriate player if available
#' @param fname filename of a .wav sound file
#' @export
play_file <- function(fname) {
  if(Sys.info()["sysname"] == "Linux") {
    if(is_wav_fname(fname) && nchar(Sys.which("paplay")) >= 1) {
      play_paplay(fname)
    } else if(is_wav_fname(fname) && nchar(Sys.which("aplay")) >= 1) {
      play_aplay(fname)
    } else if(nchar(Sys.which("vlc")) >= 1) {
      play_vlc(fname)
    } else {
      play_audio(fname)
    }
  } else {
    play_audio(fname)
  }
}
