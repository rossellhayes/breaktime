as.timer_duration <- function(x) {
  x <- as.numeric(x)
  class(x) <- "timer_duration"
  x
}

#' @export
as.character.timer_duration <- function(x, ...) {
  x <- as.numeric(x)

  negative <- character(length(x))
  negative[x < 0] <- "-"

  x <- abs(x)

  remainder <- x %% 1
  # Remove leading zeroes from remainder
  # If remainder == 0, it will become an empty string
  remainder <- sub("^0", "", format(remainder))

  # Create a matrix with a column for hours, minutes and seconds
  x <- matrix(
    data = c(
      hours   = x %/% 3600,
      minutes = x %/% 60 %% 60,
      seconds = x %/% 1 %% 60
    ),
    ncol = 3
  )

  # Pad single-digit minutes and seconds with zeroes
  x[, -1] <- formatC(x[, -1], width = 2, flag = "0")

  # Collapse each row of the matrix into a single character string
  x <- apply(x, 1, paste, collapse = ":")

  # If duration is less than an hour, don't print hours
  x <- sub("^0:", "", x)

  x <- paste0(negative, x, remainder)

  x
}

#' @export
print.timer_duration <- function(x, ...) {
  if (length(x) > 0) x <- as.character(x)

  attributes(x) <- NULL

  NextMethod(quote = FALSE, right = TRUE)
}

#' @export
Summary.timer_duration <- function(x, ...) {
  as.timer_duration(NextMethod())
}
