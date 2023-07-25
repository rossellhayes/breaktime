#' @importFrom R6 R6Class
#' @importFrom beepr beep
Timepiece <- R6::R6Class(
  "Timepiece",

  public = list(
    start_time = NULL,
    pause_time = NULL,
    is_paused = FALSE,

    start_color = NULL,
    end_color = NULL,
    sound = character(0),

    initialize = function() {
      self$start_time <- Sys.time()
    },

    pause = function() {
      if (self$is_paused) {
        private$last_pause_duration <- self$pause_duration
        self$is_paused <- FALSE
      } else {
        self$pause_time <- Sys.time()
        private$last_elapsed_time <- self$elapsed_time
        self$is_paused <- TRUE
      }
    },

    reset = function() {
      self$start_time <- Sys.time()
      self$pause_time <- NULL
      private$last_elapsed_time <- 0
      private$last_pause_duration <- 0
      self$is_paused <- FALSE
    },

    format = function() {
      self$elapsed_time
    },

    print = function() {
      if (!rlang::is_interactive()) {
        return(cli::cli_text("{self$format()}"))
      }

      old_option <- options("cli.progress_show_after" = 0)
      on.exit(options(old_option), add = TRUE)

      private$get_key <- determine_get_key_method()

      private$print_header()

      cli::cli_progress_message("{self$format()}")

      repeat {
        cli::cli_progress_update()

        key <- private$get_key(block = self$is_paused)

        if (nzchar(key)) {
          switch(
            key,
            escape = break,
            p = self$pause(),
            r = self$reset()
          )

          cli::cli_progress_update()
        }

        Sys.sleep(1 - (self$elapsed_time %% 1))
      }
    }
  ),

  active = list(
    elapsed_time = function() {
      if (self$is_paused) {
        return(private$last_elapsed_time)
      }

      breaktime_duration(self$start_time, Sys.time()) - self$pause_duration
    },

    pause_duration = function() {
      if (!self$is_paused) {
        return(private$last_pause_duration)
      }

      breaktime_duration(self$pause_time, Sys.time()) +
        private$last_pause_duration
    },

    complete = function() {
      FALSE
    },

    color = function() {
      if (self$complete) {
        self$end_color
      } else {
        self$start_color
      }
    }
  ),

  private = list(
    last_elapsed_time = 0,
    last_pause_duration = 0,
    last_chime = 0,

    get_key = NULL,

    initialize_colors = function(start_color, end_color) {
      self$start_color <- cli::make_ansi_style(
        start_color %||% cli::style_no_color
      )
      self$end_color <- cli::make_ansi_style(
        end_color %||% cli::style_no_color
      )
    },

    chime = function() {
      private$last_chime <- self$elapsed_time

      if (length(self$sound) != 0) {
        beepr::beep()
      }
    },

    print_header = function() {
      cli::cli_h1(private$headline)
      cli::cli_text(
        "Press {.key p} to pause, ",
        "{.key r} to restart, ",
        "or {.key esc} to quit."
      )
    }
  )
)
