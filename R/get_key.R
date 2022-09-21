get_key <- function(block = FALSE, case_sensitive = FALSE) {
  case <- if (case_sensitive) identity else tolower

  if (rlang::is_installed("keypress") && keypress::has_keypress_support()) {
    return(case(keypress::keypress(block = block)))
  }

  if (rlang::is_installed("rstudioapi") && rstudioapi::isAvailable()) {
    key <-  ""

    while (key == "") {
      key <- substr(
        trimws(rstudioapi::getConsoleEditorContext()$contents), 1, 1
      )

      if (key != "") {rstudioapi::sendToConsole("", execute = FALSE)}
      if (!block) {break}
      Sys.sleep(0.1)
    }

    return(case(key))
  }
}

check_keypress_support <- function() {
  if (identical(.Platform$GUI, "RStudio")) {
    rlang::check_installed(
      "rstudioapi",
      reason = "to use `breaktime` in RStudio."
    )
  }

  rstudioapi <- isTRUE(try(rstudioapi::isAvailable(), silent = TRUE))

  if (!rstudioapi) {
    rlang::check_installed(
      "keypress",
      reason = "to use `breaktime` from the command line."
    )
  }

  keypress <- isTRUE(try(keypress::has_keypress_support(), silent = TRUE))

  if (!rstudioapi && !keypress) {
    cli::cli_abort(
      c(
        "It looks like your R session does not support capturing keypresses.",
        "*" = "Try running R from the command line or in RStudio to use {.pkg breaktime}."
      )
    )
  }
}
