% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stopwatch.R
\name{stopwatch}
\alias{stopwatch}
\title{Start a stopwatch}
\usage{
stopwatch(
  target_time = NULL,
  start_color = NULL,
  end_color = NULL,
  sound = NULL,
  units = "minutes"
)
}
\arguments{
\item{target_time}{\verb{[numeric(1)]}\cr
If \code{target_time} is not \code{\link{NULL}}, the stopwatch will change colors from
\code{start_color} to \code{end_color} and play \code{sound} when \code{target_time} is reached.
Defaults to \code{\link{NULL}}.}

\item{start_color, end_color}{\verb{[character(1)]}\cr
Colors used to display the time.
The time is displayed in \code{start_color} when there is time remaining,
and in \code{end_color} when the timer has expired.
Passed to \code{\link[cli:make_ansi_style]{cli::make_ansi_style()}}.
Defaults to \code{"green"} and \code{"red"}, respectively.}

\item{sound}{\verb{[numeric(1)]} or \verb{[character(1)]}
The sound played by \code{\link[beepr:beep]{beepr::beep()}} when a timer ends.
See \code{\link[beepr:beep]{beepr::beep()}} for possible values.

If \code{\link{NULL}}, sound is disabled.

If a timer has expired and hasn't been ended,
the sound will play again each time one-fifth of the target time passes
(for example, if a session lasts 25 minutes,
a reminder sound will play every 5 minutes after the session ends).}

\item{units}{\verb{[character(1)]}\cr
Units used when interpreting numeric time period arguments.
Defaults to \code{"minutes"}.
Accepted units are
\code{"seconds"}, \code{"minutes"}, \code{"hours"}, \code{"days"}, \code{"weeks"}, \code{"fortnights"}, \code{"months"}, \code{"years"}, \code{"decades"}, \code{"centuries"}, or \code{"millennia"}
(or abbreviations thereof).}
}
\value{
Invisibly returns an \link[R6:R6Class]{R6} object of class \code{Stopwatch}.
}
\description{
Creates an interactive stopwatch in your R session.
I recommend you don't call this function in your current R session,
as it will block the console.
You may consider running it from the command line in a new terminal window with

\if{html}{\out{<div class="sourceCode">}}\preformatted{R -e "breaktime::stopwatch()"
}\if{html}{\out{</div>}}
}
