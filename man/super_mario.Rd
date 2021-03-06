% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/super_mario.R
\name{super_mario}
\alias{super_mario}
\title{Play a short sound from Super Mario Bros}
\usage{
super_mario(sound = 1, expr = NULL)
}
\arguments{
\item{sound}{character string or number specifying what sound to be played by
either specifying one of the built in sounds, specifying the path to a wav
file or specifying an url. The default is 1. Possible sounds are:
\enumerate{ \item \code{"coin"} \item \code{"gameover"} \item
\code{"mariodie"} \item \code{"warning"} \item \code{"stage_clear"}
\item \code{"world_clear"} \item \code{"one_up"} \item
\code{"bowser_fall"} \item \code{"breakblock"} \item \code{"bump"} \item
\code{"flagpole"} \item \code{"fireball"} \item \code{"jump_small"} \item
\code{"jump_super"} \item \code{"kick"} \item \code{"pause"} \item
\code{"pipe"} \item \code{"powerup"} \item
\code{"powerup_appears"} \item \code{"stomp"} }
If \code{sound} does not match any of the sounds above, or is a valid path
or url, a random sound will be played. Currently \code{super_mario} can
only handle http urls, https is not supported.}

\item{expr}{An optional expression to be excecuted before the sound.}
}
\description{
\code{super_mario} plays a short sound which is useful if you want to get
 notified, for example, when a script has finished. As an added bonus there
 are a number of different sounds to choose from.
}
\details{
If \code{super_mario} is not able to play the sound a warning is issued
 rather than an error. This is in order to not risk aborting or stopping
 the process that you wanted to get notified about.
}
\examples{
# Play a "ping" sound
super_mario()

\dontrun{
# Play "bump" instead of a "coin".
super_mario("bump")
# or
super_mario(11)

# Play a random sound
super_mario(0)

# Update all packages and "ping" when it's ready
update.packages(ask=FALSE); super_mario()
}
}
