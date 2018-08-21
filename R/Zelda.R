#' Play a short sound from zelda
#'
#' \code{zelda} plays a short sound which is useful if you want to get
#'   notified, for example, when a script has finished. As an added bonus there
#'   are a number of different sounds to choose from.
#'
#' If \code{zelda} is not able to play the sound a warning is issued rather
#'   than an error. This is in order to not risk aborting or stopping the
#'   process that you wanted to get notified about.
#'
#' @param sound character string or number specifying what sound to be played
#'   by either specifying one of the built in sounds, specifying the path to a
#'   wav file or specifying an url. The default is 1. Possible sounds are:
#'   \enumerate{ \item \code{"fanfare"} \item \code{"agahnim"} \item
#'   \code{"mudora"} \item \code{"chest"} \item \code{"flute"}
#'   \item \code{"heartcontainer"} \item \code{"mushroom"} \item \code{"warp"}
#'   }
#'   If \code{sound} does not match any of the sounds above, or is a valid path
#'   or url, a random sound will be played. Currently \code{zelda} can only
#'   handle http urls, https is not supported.
#' @param expr An optional expression to be excecuted before the sound.
#' @return NULL
#' @examples
#' # Play a "fanfare" sound
#' zelda()
#'
#' \dontrun{
#' # Play "warp" instead of a "fanfare".
#' zelda("warp")
#' # or
#' zelda(8)
#'
#' # Play a random sound
#' zelda(0)
#'
#' # Update all packages and "ping" when it's ready
#' update.packages(ask=FALSE); zelda()
#' }
#' @importFrom utils download.file
#' @export
zelda <- function(sound = 1, expr = NULL) {
  expr
  sounds <- c(
    fanfare = "LTTP_ItemFanfare.wav"
    , agahnim = "LTTP_Agahnim_Dash.wav"
    , mudora = "LTTP_BookOfMudora.wav"
    , chest = "LTTP_Chest_Appears.wav"
    , flute = "LTTP_Flute.wav"
    , heartcontainer = "LTTP_Get_HeartContainer.wav"
    , mushroom = "LTTP_Mushroom.wav"
    , warp = "LTTP_Warp.wav"
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
        temp_file <- tempfile(pattern = "")
        if(download.file(sound, destfile = temp_file, quiet = TRUE) == 0) {
          sound_path <- temp_file
        } else {
          warning(paste("Tried but could not download", sound))
        }
      } else {
        warning(
          paste(
            '"'
            , sound
            , '" is not a valid sound nor path, '
            , 'playing a random sound instead.'
            , sep = ""
            )
          )
      }
    }
  } else {
    sound_path <- system.file(
      paste(
        "sounds/LTTP/"
        , sounds[sound]
        , sep = ""
        )
      , package = "geekr"
      )
  }
  if (is.null(sound_path)) { # play a random sound
    sound_path <- system.file(
      paste(
        "sounds/LTTP/"
        , sample(sounds, size = 1)
        , sep = ""
        )
      , package = "geekr"
      )
  }
  tryCatch(play_file(sound_path), error = function(ex) {
    warning(
      "zelda() could not play the sound due to the following error:\n"
      , ex
      )
  })
}
