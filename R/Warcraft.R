#' Play a short sound from Warcraft
#'
#' \code{warcraft} plays a short sound which is useful if you want to get
#'   notified, for example, when a script has finished. As an added bonus
#'   there are a number of different sounds to choose from.
#' If \code{warcraft} is not able to play the sound a warning is issued rather
#'   than an error. This is in order to not risk aborting or stopping the
#'   process that you wanted to get notified about.
#' @param sound character string or number specifying what sound to be played
#'   by either specifying one of the built in sounds, specifying the path to a
#'   wav file or specifying an url. The default is 1. Possible sounds are:
#'   \enumerate{ \item \code{"jobsdone"} \item \code{"hello"} \item
#'   \code{"peasant"} \item \code{"err"} \item \code{"peon"} \item
#'   \code{"notready"} \item \code{"complete"} \item \code{"gold"} \item
#'   \code{"blacksmith"} \item \code{"lumber"} }
#'   If \code{sound} does not match any of the sounds above, or is a valid path
#'   or url, a random sound will be played. Currently \code{warcraft} can only
#'   handle http urls, https is not supported.
#' @param expr An optional expression to be excecuted before the sound.
#' @return NULL
#' @examples
#' # Play an "jobsdone" sound
#' warcraft()
#'
#' \dontrun{
#' # Play "hello" instead of a "jobsdone".
#' warcraft("hello")
#' # or
#' warcraft(2)
#'
#' # Play a random sound
#' warcraft(0)
#'
#' # Update all packages and "ping" when it's ready
#' update.packages(ask=FALSE); warcraft()
#' }
#' @importFrom utils download.file
#' @export
warcraft <- function(sound = 1, expr = NULL) {
  expr
  sounds <- c(
    jobsdone = "ally_jobsdone.wav"
    , hello = "ally_hello.wav"
    , peasant = "ally_ready.wav"
    , err = "horde_...err.wav"
    , peon = "horde_ready.wav"
    , notready = "horde_readynotready.wav"
    , complete = "horde_work-complete.wav"
    , gold = "gold-mine.wav"
    , blacksmith = "blacksmith.wav"
    , lumber = "lumber-mill.wav"
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
        if (download.file(sound, destfile = temp_file, quiet = TRUE) == 0) {
          sound_path <- temp_file
        } else {
          warning(paste("Tried but could not download", sound))
        }
      } else {
        warning(
          paste0(
            "'"
            , sound
            , "' is not a valid sound nor path,"
            , " playing a random sound instead."
            )
          )
      }
    }
  } else {
    sound_path <- system.file(
      paste(
        "sounds/warcraft/"
        , sounds[sound]
        , sep = ""
        )
      , package = "geekr"
      )
  }
  if (is.null(sound_path)) {
    sound_path <- system.file(
      paste(
        "sounds/warcraft/"
        , sample(sounds, size = 1)
        , sep = ""
        )
      , package = "geekr"
      )
  }
  tryCatch(play_file(sound_path), error = function(ex) {
    warning(
      "warcraft() could not play the sound due to the following error:\n"
      , ex
      )
  })
}
