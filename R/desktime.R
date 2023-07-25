#' Start a DeskTime timer
#'
#' Creates an interactive timer in your R session.
#' I recommend you don't call this function in your current R session,
#' as it will block the console.
#' You may consider running it from the command line in a new terminal window with
#' ```
#' R -e "breaktime::desktime()"
#' ```
#'
#' @source
#' The [52/17 Rule](https://www.themuse.com/advice/the-rule-of-52-and-17-its-random-but-it-ups-your-productivity)
#' by [Julia Gifford](https://www.themuse.com/author/julia-gifford).
#'
#' @inheritParams breaktime
#' @param work_time `[numeric(1)]`\cr
#'   The length of time for each work session.
#'   Defaults to 52 minutes.
#' @param break_time `[numeric(1)]`\cr
#'   The length of time for each break.
#'   Defaults to 17 minutes.
#'
#' @return Invisibly returns an [R6][R6::R6] object of class `DeskTime`.
#' @export
desktime <- function(
  work_time = 52,
  break_time = 17,
  start_color = "green",
  end_color = "red",
  sound = "ping",
  units = "minutes"
) {
  try_resume("desktime")

  breaktime_env$desktime <- DeskTime$new(
    work_time = work_time,
    break_time = break_time,
    start_color = start_color,
    end_color = end_color,
    sound = sound,
    units = units
  )

  breaktime_env$desktime
}

#' @importFrom R6 R6Class
DeskTime <- R6::R6Class(
  "DeskTime",
  inherit = Pomodoro,

  public = list(
    short_break_multiplier = numeric(1),

    initialize = function(
      work_time = 52,
      break_time = 17,
      start_color = "green",
      end_color = "red",
      sound = "ping",
      units = "minutes"
    ) {
      self$work_time <- in_seconds(work_time, units)
      self$short_break_time <- in_seconds(break_time, units)
      self$start_color <- start_color
      self$end_color <- end_color
      self$sound <- sound

      self$working <- TRUE

      self$initialize_timepiece()
    },

    next_session = function() {
      if (self$working) {
        self$working <- FALSE
        self$on_break <- TRUE
      } else {
        self$working <- TRUE
        self$on_break <- FALSE
      }

      self$initialize_timepiece()
    }
  ),

  active = list(
    break_time = function() {
      self$short_break_time
    }
  ),

  private = list(
    headline = "{if (cli::is_utf8_output()) '\U1F5C4\UFE0F '}desktime"
  )
)
