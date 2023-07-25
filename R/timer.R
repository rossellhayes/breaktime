#' Start a timer
#'
#' Creates an interactive timer in your R session.
#' I recommend you don't call this function in your current R session,
#' as it will block the console.
#' You may consider running it from the command line in a new terminal window with
#' ```
#' R -e "breaktime::timer()"
#' ```
#'
#' @inheritParams breaktime
#' @param duration `[numeric(1)]`\cr
#'   The length of the timer.
#'
#' @return Invisibly returns an [R6][R6::R6] object of class `Timer`.
#' @export
timer <- function(
  duration,
  start_color = "green",
  end_color = "red",
  sound = "ping",
  units = "minutes"
) {
  try_resume("timer")

  breaktime_env$timer <- Timer$new(
    duration = duration,
    start_color = start_color,
    end_color = end_color,
    sound = sound,
    units = units
  )

  breaktime_env$timer
}

#' @importFrom R6 R6Class
Timer <- R6::R6Class(
  "Timer",
  inherit = Stopwatch,

  public = list(
    timer_duration = NULL,

    initialize = function(
      duration,
      start_color = "green",
      end_color = "red",
      sound = "ping",
      units = "minutes"
    ) {
      self$start_time <- Sys.time()
      self$timer_duration <- in_seconds(duration, units)
      private$initialize_colors(start_color, end_color)
      self$sound <- sound
    },

    format = function() {
      if (
        self$complete &&
          self$elapsed_time - private$last_chime > self$timer_duration / 5
      ) {
        private$chime()
      }

      self$color(self$remaining_time)
    }
  ),

  active = list(
    remaining_time = function() {
      as.breaktime_duration(self$timer_duration - self$elapsed_time)
    },

    complete = function() {
      self$elapsed_time >= self$timer_duration
    }
  ),

  private = list(
    headline = "{if (cli::is_utf8_output()) '\U23F2\UFE0F '}timer"
  )
)
