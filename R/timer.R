#' Start a timer
#'
#' @description
#' `timer()` creates an interactive timer in your R session.
#' I recommend you don't use `timer()` in your current R session,
#' as it will block the console.
#' You may consider running it from the command line in a new terminal window with
#' ```
#' R -e "breaktime::timer()"
#' ```
#'
#' The default arguments to `timer()` match those recommended by the
#'
#' `pomodoro()` is an alias with defaults that match the recommended fixed times
#' of the [Pomodoro Technique](https://francescocirillo.com/pages/pomodoro-technique):
#' 25 minute working sessions, 5 minute breaks and 20 minute long breaks.
#'
#' @source
#' Inspired by the [Flowtime Technique](https://medium.com/@UrgentPigeon/the-flowtime-technique-7685101bd191)
#' by [Urgent Pigeon](https://medium.com/@UrgentPigeon/the-flowtime-technique-7685101bd191)
#' and the [Pomodoro Technique](https://francescocirillo.com/pages/pomodoro-technique)
#' by [Francesco Cirillo](https://francescocirillo.com/pages/francesco-cirillo).
#'
#' @param work_time `[numeric(1)]`\cr
#'   The length of time for each working session in minutes.
#'   Defaults to 25 minutes.
#' @param break_time `[numeric(1)]`\cr
#'   The length of time for each short break in minutes.
#'   If [`NULL`], the break time will be one-fifth of the length of the previous
#'   working session.
#' @param long_break_time `[numeric(1)]`\cr
#'   The length of time for each long break in minutes.
#'   Long breaks occur every fourth break.
#'   If [`NULL`], the long break time will be one-fifth of the length of the
#'   previous working session, plus the length of the previous three short breaks.
#' @param start_color,end_color `[character(1)]`\cr
#'   Colors used to display the time.
#'   The time is displayed in `start_color` when there is time remaining in the
#'   current session, and in `end_color` when the current session has expired.
#'   Color specifications are handled by [cli::make_ansi_style()].
#'
#' @return Invisibly returns an [R6][R6::R6] object of class `Timer`
#' @export
timer <- function(
  work_time = 25,
  break_time = NULL,
  long_break_time = NULL,
  start_color = "green",
  end_color = "red"
) {
  new_timer(
    work_time = work_time,
    break_time = break_time,
    long_break_time = long_break_time,
    start_color = start_color,
    end_color = end_color
  )
}

#' @rdname timer
#' @export
pomodoro <- function(
  work_time = 25,
  break_time = 5,
  long_break_time = 20,
  start_color = "green",
  end_color = "red"
) {
  new_timer(
    work_time = work_time,
    break_time = break_time,
    long_break_time = long_break_time,
    start_color = start_color,
    end_color = end_color
  )
}

new_timer <- function(...) {
  invisible(Timer$new(...))
}
