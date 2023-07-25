breaktime_env <- new.env()

#' Start a breaktime timer
#'
#' Creates an interactive timer in your R session.
#' I recommend you don't call this function in your current R session,
#' as it will block the console.
#' You may consider running it from the command line in a new terminal window with
#' ```
#' R -e "breaktime::breaktime()"
#' ```
#'
#' @source
#' Inspired by the [Flowtime Technique](https://medium.com/@UrgentPigeon/the-flowtime-technique-7685101bd191)
#' by [Urgent Pigeon](https://medium.com/@UrgentPigeon)
#' and the [Pomodoro Technique](https://francescocirillo.com/pages/pomodoro-technique)
#' by [Francesco Cirillo](https://francescocirillo.com/pages/francesco-cirillo).
#'
#' @param work_time `[numeric(1)]`\cr
#'   The length of time for each working session.
#'   Defaults to 25 minutes.
#' @param short_break_multiplier `[numeric(1)]`\cr
#'   The length of each short break compared to the preceding working session.
#'   Defaults to one fifth (for example, a 25 minute working session will be
#'   followed by a 5 minute short break).
#' @param long_break_frequency `[numeric(1)]`\cr
#'   The number of working sessions between each long break.
#'   Defaults to 4.
#' @param sound `[numeric(1)]` or `[character(1)]`
#'   The sound played by [beepr::beep()] when a timer ends.
#'   See [beepr::beep()] for possible values.
#'
#'   If [`NULL`], sound is disabled.
#'
#'   If a timer has expired and hasn't been ended,
#'   the sound will play again each time one-fifth of the target time passes
#'   (for example, if a session lasts 25 minutes,
#'   a reminder sound will play every 5 minutes after the session ends).
#' @param start_color,end_color `[character(1)]`\cr
#'   Colors used to display the time.
#'   The time is displayed in `start_color` when there is time remaining,
#'   and in `end_color` when the timer has expired.
#'   Passed to [cli::make_ansi_style()].
#'   Defaults to `"green"` and `"red"`, respectively.
#' @param units `[character(1)]`\cr
#'   Units used when interpreting numeric time period arguments.
#'   Defaults to `"minutes"`.
#'   Accepted units are
#'   `r knitr::combine_words(backtick(quote(breaktime_units)), and = " or ")`
#'   (or abbreviations thereof).
#'
#' @return Invisibly returns an [R6][R6::R6] object of class `BreakTime`.
#' @export
breaktime <- function(
  work_time = 25,
  short_break_multiplier = 1/5,
  long_break_frequency = 4,
  start_color = "green",
  end_color = "red",
  sound = "ping",
  units = "minutes"
) {
  try_resume("breaktime")

  breaktime_env$breaktime <- BreakTime$new(
    work_time = work_time,
    short_break_multiplier = short_break_multiplier,
    long_break_frequency = long_break_frequency,
    start_color = start_color,
    end_color = end_color,
    sound = sound,
    units = units
  )

  breaktime_env$breaktime
}

#' @importFrom R6 R6Class
#' @importFrom utils tail
BreakTime <- R6::R6Class(
  "BreakTime",
  inherit = Pomodoro,

  public = list(
    short_break_multiplier = numeric(1),

    initialize = function(
      work_time = 25,
      short_break_multiplier = 1/5,
      long_break_frequency = 4,
      start_color = "green",
      end_color = "red",
      sound = "ping",
      units = "minutes"
    ) {
      self$work_time <- in_seconds(work_time, units)
      self$short_break_multiplier <- short_break_multiplier
      self$long_break_frequency <- long_break_frequency
      self$start_color <- start_color
      self$end_color <- end_color
      self$sound <- sound

      self$working <- TRUE

      self$initialize_timepiece()
    }
  ),

  active = list(
    break_time = function() {
      short_break_time <- self$timepiece$elapsed_time * self$short_break_multiplier

      if (self$on_long_break) {
        sum(
          short_break_time,
          utils::tail(self$break_list, self$long_break_frequency - 1)
        )
      } else {
        short_break_time
      }
    }
  ),

  private = list(
    headline = "{if (cli::is_utf8_output()) '\U1F36B '}breaktime"
  )
)
