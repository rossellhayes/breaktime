#' Start a stopwatch
#'
#' Creates an interactive stopwatch in your R session.
#' I recommend you don't call this function in your current R session,
#' as it will block the console.
#' You may consider running it from the command line in a new terminal window with
#' ```
#' R -e "breaktime::stopwatch()"
#' ```
#'
#' @inheritParams breaktime
#' @param target_time `[numeric(1)]`\cr
#'   If `target_time` is not [`NULL`], the stopwatch will change colors from
#'   `start_color` to `end_color` and play `sound` when `target_time` is reached.
#'   Defaults to [`NULL`].
#'
#' @return Invisibly returns an [R6][R6::R6] object of class `Stopwatch`.
#' @export
stopwatch <- function(
  target_time = NULL,
  start_color = NULL,
  end_color = NULL,
  sound = NULL,
  units = "minutes"
) {
  try_resume("stopwatch")

  breaktime_env$stopwatch <- Stopwatch$new(
    target_time = target_time,
    start_color = start_color,
    end_color = end_color,
    sound = sound,
    units = units
  )

  breaktime_env$stopwatch
}

Stopwatch <- R6::R6Class(
  "Stopwatch",
  inherit = Timepiece,

  public = list(
    target_time = numeric(1),

    initialize = function(
      target_time = NULL,
      start_color = NULL,
      end_color = NULL,
      sound = NULL,
      units = "minutes"
    ) {
      self$start_time <- Sys.time()
      self$target_time <- in_seconds(target_time, units) %||% Inf
      private$initialize_colors(start_color, end_color)
      self$sound <- sound
    },

    format = function() {
      if (
        self$complete &&
          self$elapsed_time - private$last_chime > self$target_time / 5
      ) {
        private$chime()
      }

      self$color(self$elapsed_time)
    }
  ),

  active = list(
    complete = function() {
      self$elapsed_time >= self$target_time
    }
  ),

  private = list(
    headline = "{if (cli::is_utf8_output()) '\U23F1\UFE0F '}stopwatch"
  )
)
