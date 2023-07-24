#' Start a Pomodoro timer
#'
#' Creates an interactive timer in your R session.
#' I recommend you don't call this function in your current R session,
#' as it will block the console.
#' You may consider running it from the command line in a new terminal window with
#' ```
#' R -e "breaktime::pomodoro()"
#' ```
#'
#' @source
#' The [Pomodoro Technique](https://francescocirillo.com/pages/pomodoro-technique)
#' by [Francesco Cirillo](https://francescocirillo.com/pages/francesco-cirillo).
#'
#' @inheritParams breaktime
#' @param short_break_time `[numeric(1)]`\cr
#'   The length of time for each short break.
#'   Defaults to 5 minutes.
#' @param long_break_time `[numeric(1)]`\cr
#'   The length of time for each long break in minutes.
#'   Defaults to 20 minutes.
#'
#' @return Invisibly returns an [R6][R6::R6] object of class `Pomodoro`.
#' @export
pomodoro <- function(
  work_time = 25,
  short_break_time = work_time / 5,
  long_break_time = short_break_time * long_break_frequency,
  long_break_frequency = 4,
  start_color = "green",
  end_color = "red",
  sound = "ping",
  units = "minutes"
) {
  try_resume("pomodoro")

  breaktime_env$pomodoro <- Pomodoro$new(
    work_time = work_time,
    short_break_time = short_break_time,
    long_break_time = long_break_time,
    long_break_frequency = long_break_frequency,
    start_color = start_color,
    end_color = end_color,
    sound = sound,
    units = units
  )

  breaktime_env$pomodoro
}


# @staticimports pkg:stringstatic
#  str_pad str_width

Pomodoro <- R6::R6Class(
  "Pomodoro",

  public = list(
    working = FALSE,
    on_break = FALSE,
    on_long_break = FALSE,

    timepiece = NULL,

    start_color = NULL,
    end_color = NULL,
    sound = NULL,

    work_time = numeric(1),
    short_break_time = numeric(1),
    long_break_time = numeric(1),
    long_break_frequency = numeric(1),

    break_list = numeric(0),

    initialize = function(
      work_time = 25,
      short_break_time = 5,
      long_break_time = 20,
      long_break_frequency = 4,
      start_color = "green",
      end_color = "red",
      sound = "ping",
      units = "minutes"
    ) {
      self$work_time <- in_seconds(work_time, units)
      self$short_break_time <- in_seconds(short_break_time, units)
      self$long_break_time <- in_seconds(long_break_time, units)
      self$long_break_frequency <- long_break_frequency
      self$sound <- sound

      private$initialize_colors(start_color, end_color)

      self$working <- TRUE

      self$initialize_timepiece()
    },

    initialize_timepiece = function() {
      if (self$working) {
        self$timepiece <- Stopwatch$new(
          target_time = self$work_time,
          units = self$units,
          start_color = self$start_color,
          end_color = self$end_color,
          sound = self$sound
        )
      } else {
        self$break_list <- append(self$break_list, self$break_time)

        self$timepiece <- Timer$new(
          duration = self$break_time,
          units = self$units,
          start_color = self$start_color,
          end_color = self$end_color,
          sound = self$sound
        )
      }
    },

    next_session = function() {
      if (self$working) {
        self$working <- FALSE
        self$on_break <- TRUE

        if (
          length(self$break_list) %% self$long_break_frequency ==
            self$long_break_frequency - 1
        ) {
          self$on_long_break <- TRUE
        }
      } else {
        self$working <- TRUE
        self$on_break <- FALSE
        self$on_long_break <- FALSE
      }

      self$initialize_timepiece()
    },

    print = function() {
      private$print_header()

      old_option <- options("cli.progress_show_after" = 0)
      on.exit(options(old_option), add = TRUE)

      private$get_key <- determine_get_key_method()

      cli::cli_progress_message(
        "{self$status} {self$timepiece$format()}"
      )

      repeat {
        cli::cli_progress_update()

        key <- private$get_key(block = self$timepiece$is_paused)

        if (nzchar(key)) {
          switch(
            key,
            p = self$timepiece$pause(),
            b = self$next_session(),
            r = self$timepiece$reset()
          )

          cli::cli_progress_update()
        }

        Sys.sleep(1 - (self$timepiece$elapsed_time %% 1))
      }
    }
  ),

  active = list(
    status = function() {
      status <- if (self$working) {
        "Working"
      } else if (self$on_long_break) {
        "Long break"
      } else {
        "On break"
      }

      if (self$timepiece$is_paused) {
        status <- str_pad("Paused", str_width(status), side = "right", pad = ".")
      }

      ellipsis <- strrep(".", floor(self$timepiece$elapsed_time) %% 3 + 1)
      ellipsis <- str_pad(ellipsis, width = 3, side = "right")

      paste0(status, ellipsis)
    },

    break_time = function() {
      if (self$on_long_break) {
        self$long_break_time
      } else {
        self$short_break_time
      }
    }
  ),

  private = list(
    get_key = NULL,

    headline = "{if (cli::is_utf8_output()) '\U1F345 '}pomodoro",

    print_header = function() {
      cli::cli_h1(private$headline)
      cli::cli_text(
        "Press {.key b} to start or end a break, ",
        "{.key p} to pause, ",
        "{.key r} to restart this session, ",
        "or {.key esc} to quit."
      )
    },

    initialize_colors = function(start_color, end_color) {
      self$start_color <- cli::make_ansi_style(
        start_color %||% cli::style_no_color
      )
      self$end_color <- cli::make_ansi_style(
        end_color %||% cli::style_no_color
      )
    }
  )
)
