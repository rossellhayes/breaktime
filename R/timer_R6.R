Timer <- R6::R6Class(
  "Timer",
  public = list(
    start_time      = NULL,
    work_time       = NULL,
    break_time      = NULL,
    long_break_time = NULL,
    sound           = NULL,
    sound_enabled   = NULL,
    start_color     = NULL,
    end_color       = NULL,
    break_time_list = numeric(),
    target_time     = NULL,
    elapsed_time    = -1,
    time_expired    = FALSE,
    status          = NULL,
    color           = NULL,
    cli_id          = NULL,
    message         = "",
    key             = "",
    command_keys    = c("b", "p", "r", "q", "escape"),

    initialize = function(
      work_time = NULL,
      break_time = NULL,
      long_break_time = NULL,
      sound = "ping",
      start_color = "green",
      end_color = "red"
    ) {
      check_keypress_support()

      self$work_time       <- as.timer_duration((work_time * 60) %||% Inf)
      self$break_time      <- as.timer_duration(break_time * 60)
      self$long_break_time <- as.timer_duration(long_break_time * 60)

      self$sound         <- sound
      self$sound_enabled <- !is.null(sound)

      self$start_color <- cli::make_ansi_style(start_color)
      self$end_color   <- cli::make_ansi_style(end_color)

      self$print_text()
      self$cli_id <- cli::cli_progress_message("{self$message}")

      self$start("work")
    },

    start = function(status = c("work", "break")) {
      self$reset_timer()

      if (status == "work") {
        self$status <- "Working"
        self$set_target_time(self$work_time)
      } else {
        self$status <- "On break"
        self$set_target_time(self$break_time %||% round(self$elapsed_time / 5))
        self$break_time_list <- append(self$break_time_list, self$target_time, 0)

        if (length(self$break_time_list) %% 4 == 0) {
          self$status <- "Long break"
          self$set_target_time(
            self$long_break_time %||% sum(self$break_time_list[1:4])
          )
        }
      }

      self$count()
    },

    set_target_time = function(time) {
      # Target time is never less than one second
      self$target_time <- max(time, 1)
    },

    count = function() {
      while (TRUE) {
        elapsed_time <- as.timer_duration(
          floor(difftime(Sys.time(), self$start_time, units = "secs"))
        )

        if (elapsed_time != self$elapsed_time) {
          self$elapsed_time <- elapsed_time

          if (self$elapsed_time >= self$target_time) {
            if (!self$time_expired) {
              self$time_expired <- TRUE
              # After time has expired, turn timer red and ding
              self$color <- self$end_color
              if (self$sound_enabled) beepr::beep(self$sound)
            } else if (
              self$sound_enabled &&
              (self$elapsed_time - self$target_time) %%
              ceiling(self$target_time / 5) == 0
            ) {
              # After time has expired, ding again each time 1/5 of the target
              # time has passed (e.g. after 25 min, ding every subsequent 5 min)
              beepr::beep(self$sound)
            }
          }

          self$message <- cli::format_inline(
            cli::format_bullets_raw(c("*" = "")),
            self$status,
            str_pad(strrep(".", self$elapsed_time %% 4), width = 4),
            if (self$status == "Working") {
              "{self$color(self$elapsed_time)} ({self$target_time}) "
            } else {
              "{self$color(self$target_time - self$elapsed_time)} "
            }
          )
          cli::cli_progress_update(id = self$cli_id)
        }

        Sys.sleep(0.1)

        self$key <- get_key()

        if (identical(self$key, "p")) {
          self$pause()
        }

        switch(
          self$key,
          b = if (identical(self$status, "Working")) {
            return(self$start("break"))
          } else {
            return(self$start("work"))
          },
          r = self$reset_timer(),
          escape = ,
          q = self$quit()
        )
      }
    },

    pause = function(parent.env = parent.env()) {
      pause_time <- Sys.time()

      self$message <- cli::format_inline(
        gsub(
          pattern = paste0(self$status, "\\.+\\s+"),
          replacement = paste0(
            "Paused", strrep(".", nchar(self$status) - nchar("Paused")),
            str_pad(strrep(".", self$elapsed_time %% 4), width = 4)
          ),
          x = self$message
        ),
        " Press {.key p} to resume."
      )
      cli::cli_progress_update(id = self$cli_id)

      while (TRUE) {
        self$key <- get_key(block = TRUE)

        if (self$key %in% self$command_keys) {
          self$start_time <- self$start_time + (Sys.time() - pause_time)

          # Force timer to immediately reprint
          self$elapsed_time <- -1

          break
        }
      }
    },

    reset_timer = function() {
      self$start_time   <- Sys.time()
      self$time_expired <- FALSE
      self$color        <- self$start_color
    },

    quit = function(env = rlang::caller_env()) {
      rlang::return_from(env, invisible(self$elapsed_time))
    },

    print = function() {
      self$print_text()
      self$cli_id <- cli::cli_progress_message("{self$message}")
      self$count()
    },

    print_text = function() {
      cli::cli_h1("breaktime")
      cli::cli_text(
        "Press {.key b} to start or end a break, ",
        "{.key p} to pause, ",
        "{.key r} to restart this session, ",
        "or {.key q} to quit."
      )
    }
  )
)
