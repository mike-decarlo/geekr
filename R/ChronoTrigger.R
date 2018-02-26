#' Play a short sound from Chrono Trigger
#' 
#'\code{ChronoTrigger} plays a short sound which is useful if you want to get notified, 
#'for example, when a script has finished. As an added bonus there are a number
#'of different sounds to choose from.
#'
#'If \code{ChronoTrigger} is not able to play the sound a warning is issued rather than 
#'an error. This is in order to not risk aborting or stopping the process that
#'you wanted to get notified about.
#'
#'@param sound character string or number specifying what sound to be played by 
#'  either specifying one of the built in sounds, specifying the path to a wav 
#'  file or specifying an url. The default is 1. Possible sounds are:
#'  \enumerate{ \item \code{"save"} \item \code{"draw"} \item
#'  \code{"leenes_bell"} \item \code{"croak"} \item \code{"chest"}
#'  \item \code{"epoch"} \item \code{"strike"} \item \code{"temporal"}
#'  \item \code{"timegate"} \item \code{"wrong"} \item
#'  \code{"black_tyrano"} \item \code{"continuum"} \item \code{"destruction"} \item
#'  \code{"water_drip"} \item \code{"grunt"} \item \code{"gulls"} \item
#'  \code{"lavos"} \item \code{"robo"} }
#'  If \code{sound} does not match any of the sounds above, or is a valid path
#'  or url, a random sound will be played. Currently \code{ChronoTrigger} can only handle
#'  http urls, https is not supported.
#'@param expr An optional expression to be excecuted before the sound.
#'
#'@return NULL
#'
#'@examples
#' # Play a "ping" sound
#' ChronoTrigger()
#' 
#' \dontrun{
#' # Play \code{"gulls"} instead of a \code{"save"}.
#' ChronoTrigger("gulls")
#' # or
#' ChronoTrigger(16)
#' 
#' # Play a random sound
#' ChronoTrigger(0)
#' 
#' # Update all packages and "ping" when it's ready
#' update.packages(ask=FALSE); ChronoTrigger()
#' }
#'@export
#'@import audio
#'@import stringr

ChronoTrigger <- function(sound = 1, expr = NULL) {
  
  expr
  sounds <- c(
    save = "savepnt.wav"
    , draw = "drawpunk.wav"
    , leenes_bell = "leene.wav"
    , croak = "croak.wav"
    , chest = "chest.wav"
    , epoch = "engage.wav"
    , strike = "strike.wav"
    , temporal = "tempforc.wav"
    , timegate = "timegate.wav"
    , wrong = "wrongo.wav"
    , black_tyrano = "blacktyr.wav"
    , continuum = "continuum.wav"
    , destruction = "destruct.wav"
    , water_drip = "drip.wav"
    , grunt = "grunt.wav"
    , gulls = "gulls.wav"
    , lavos = "lavos.wav"
    , robo = "roboredy.wav"
  )
  
  sound_path <- NULL
  if(is.na(sounds[sound]) || length(sounds[sound]) != 1) {
    if(is.character(sound)) {
      sound <- str_trim(sound)
      if(file.exists(sound)) {
        sound_path <- sound
      } else if(str_detect(sound, "^https://")) {
        warning("Can't currently use https urls, only http.")
      } else if(str_detect(sound, "^http://")) {
        temp_file <- tempfile(pattern="")
        if(download.file(sound, destfile = temp_file, quiet = TRUE) == 0) { # The file was successfully downloaded
          sound_path <- temp_file
        } else {
          warning(paste("Tried but could not download", sound))
        }
      } else {
        warning(paste('"', sound, '" is not a valid sound nor path, playing a random sound instead.', sep = ""))
      }
    }
  } else {
    sound_path <- system.file(paste("sounds/CT/", sounds[sound], sep=""), package="geekr")
  }
  
  if(is.null(sound_path)) { # play a random sound
    sound_path <- system.file(paste("sounds/CT/", sample(sounds, size=1), sep=""), package="geekr")
  }
  
  tryCatch(play_file(sound_path), error = function(ex) {
    warning("ChronoTrigger() could not play the sound due to the following error:\n", ex)
  })
}

is_wav_fname <- function(fname) {
  str_detect(fname, regex("\\.wav$", ignore_case = TRUE))
}

play_vlc <- function(fname) {
  system(paste("vlc -Idummy --no-loop --no-repeat --playlist-autostart --no-media-library --play-and-exit", fname), 
         ignore.stdout = TRUE, ignore.stderr=TRUE,wait = FALSE)
  invisible(NULL)
}

play_paplay <- function(fname) {
  system(paste("paplay ", fname), ignore.stdout = TRUE, ignore.stderr=TRUE,wait = FALSE)
  invisible(NULL)
}

play_aplay <- function(fname) {
  system(paste("aplay --buffer-time=48000 -N -q", fname), ignore.stdout = TRUE, ignore.stderr=TRUE,wait = FALSE)
  invisible(NULL)
}

play_audio <- function(fname) {
  sfx <- load.wave(fname)
  play(sfx)
}

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
