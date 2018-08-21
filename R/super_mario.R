#' Play a short sound from Super Mario Bros.
#'
#' \code{super_mario} plays a short sound which is useful if you want to get
#'  notified, for example, when a script has finished. As an added bonus there
#'  are a number of different sounds to choose from.
#'
#' If \code{super_mario} is not able to play the sound a warning is issued
#'  rather than an error. This is in order to not risk aborting or stopping
#'  the process that you wanted to get notified about.
#' @param sound character string or number specifying what sound to be played by 
#'  either specifying one of the built in sounds, specifying the path to a wav 
#'  file or specifying an url. The default is 1. Possible sounds are:
#'  \enumerate{ \item \code{"coin"} \item \code{"gameover"} \item
#'  \code{"mariodie"} \item \code{"warning"} \item \code{"stage_clear"}
#'  \item \code{"world_clear"} \item \code{"one_up"} \item 
#'  \code{"bowser_fall"} \item \code{"breakblock"} \item \code{"bump"} \item
#'  \code{"flagpole"} \item \code{"fireball"} \item \code{"jump_small"} \item
#'  \code{"jump_super"} \item \code{"kick"} \item \code{"pause"} \item
#'  \code{"pipe"} \item \code{"powerup"} \item
#'  \code{"powerup_appears"} \item \code{"stomp"} }
#'  If \code{sound} does not match any of the sounds above, or is a valid path
#'  or url, a random sound will be played. Currently \code{super_mario} can 
#'  only handle http urls, https is not supported.
#' @param expr An optional expression to be excecuted before the sound.
#' @return NULL
#' @examples
#' # Play a "ping" sound
#' super_mario()
#'
#' \dontrun{
#' # Play "bump" instead of a "coin".
#' super_mario("bump")
#' # or
#' super_mario(11)
#'
#' # Play a random sound
#' super_mario(0)
#'
#' # Update all packages and "ping" when it's ready
#' update.packages(ask=FALSE); super_mario()
#' }
#' @importFrom utils download.file
#' @export
super_mario <- function(sound = 1, expr = NULL) {
  expr
  sounds <- c(
    coin = "smb_coin.wav"
    , gameover = "smb_gameover.wav"
    , mariodie = "smb_mariodie.wav"
    , warning = "smb_warning.wav"
    , stage_clear = "smb_stage_clear.wav"
    , world_clear = "smb_world_clear.wav"
    , one_up = "smb_1-up.wav"
    , bowser_fall = "smb_bowserfalls.wav"
    , breakblock = "smb_breakblock.wav"
    , bump = "smb_bump.wav"
    , flagpole = "smb_flagpole.wav"
    , fireball = "smb_fireball.wav"
    , jump_small = "smb_jump-small.wav"
    , jump_super = "smb_jump-super.wav"
    , kick = "smb_kick.wav"
    , pause = "smb_pause.wav"
    , pipe = "smb_pipe.wav"
    , powerup = "smb_powerup.wav"
    , powerup_appears = "smb_powerup_appears.wav"
    , stomp = "smb_stomp.wav"
  )
  sound_path <- NULL
  if (is.na(sounds[sound]) || length(sounds[sound]) != 1) {
    if (is.character(sound)) {
      sound <- trimws(sound)
      if (file.exists(sound)) {
        sound_path <- sound
      } else if (grepl("^https://", sound)) {
        warning("Can't currently use https urls, only http.")
      } else if (grepl("^http://", sound)) {
        temp_file <- tempfile(pattern="")
        if (download.file(sound, destfile = temp_file, quiet = TRUE) == 0) {
          sound_path <- temp_file
        } else {
          warning(paste("Tried but could not download", sound))
        }
      } else {
        warning(
          paste0(
            '"'
            , sound
            , '" is not a valid sound nor path, playing a random sound instead.'
            )
          )
      }
    }
  } else {
    sound_path <- system.file(
      paste(
        "sounds/SMB/"
        , sounds[sound]
        , sep = ""
        )
      , package = "geekr"
      )
  }
  if (is.null(sound_path)) {
    sound_path <- system.file(
      paste(
        "sounds/SMB/"
        , sample(sounds, size=1)
        , sep = ""
        )
      , package = "geekr"
      )
  }
  tryCatch(play_file(sound_path), error = function(ex) {
    warning(
      paste0(
        "super_mario() could not play the sound due to the following error:\n"
        , ex
        )
      )
  })
}
