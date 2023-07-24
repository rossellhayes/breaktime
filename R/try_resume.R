try_resume <- function(object, call = rlang::caller_env()) {
  if (
    !rlang::is_interactive() || !exists(object, breaktime_env, inherits = FALSE)
  ) {
    return(invisible())
  }

  msg <- c("i" = "There is an ongoing {.pkg {object}} timer.")

  repeat {
    cli::cli_bullets(msg)

    response <- readline("Do you want to resume? (y/n) ")
    response <- tolower(substr(trimws(response), 1, 1))
    switch(
      response,
      "y" = rlang::return_from(call, get(object, breaktime_env)),
      "n" = break
    )

    msg <- c("x" = "Sorry, I didn't understand that input.")
  }

  invisible()
}
