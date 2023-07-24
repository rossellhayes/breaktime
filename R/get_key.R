get_key_rstudio <- function(block = TRUE) {
  key <-  ""

  repeat {
    key <- substr(
      trimws(rstudioapi::getConsoleEditorContext()$contents),
      1,
      1
    )

    if (nzchar(key)) {
      rstudioapi::sendToConsole("", execute = FALSE)
      return(tolower(key))
    }

    if (!block) {
      return(NA_character_)
    }

    Sys.sleep(0.1)
  }
}

determine_get_key_method <- function() {
  if (identical(.Platform$GUI, "RStudio")) {
    rlang::check_installed(
      "rstudioapi",
      reason = "to use `breaktime` in RStudio."
    )
  }

  has_rstudioapi <- isTRUE(try(rstudioapi::isAvailable(), silent = TRUE))

  if (has_rstudioapi) {
    return(get_key_rstudio)
  }

  rlang::check_installed(
    "keypress",
    reason = "to use `breaktime` from the command line."
  )

  has_keypress <- isTRUE(try(keypress::has_keypress_support(), silent = TRUE))

  if (has_keypress) {
    return(keypress::keypress)
  }

  cli::cli_abort(
    c(
      "It looks like your R session does not support capturing keypresses.",
      "*" = "Try running R from the command line or in RStudio to use
        {.pkg breaktime}."
    )
  )
}
