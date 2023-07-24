breaktime_duration <- function(start, end) {
  as.breaktime_duration(difftime(end, start, units = "secs"))
}

as.breaktime_duration <- function(x) {
  structure(as.numeric(x), class = "breaktime_duration")
}

# @staticimports pkg:stringstatic
#  str_pad str_replace

#' @export
as.character.breaktime_duration <- function(x, digits = 0, ...) {
  x <- as.numeric(x)

  negative <- character(length(x))
  negative[x < 0] <- "-"

  x <- abs(x)

  if (digits > 0) {
    remainder <- x %% 1
    remainder <- trunc(remainder, digits = digits)
    remainder <- str_replace(remainder, "^0", "")
    remainder <- str_pad(remainder, digits + 1, side = "right", pad = "0")
  } else {
    remainder <- rep("", length(x))
  }

  x <- matrix(
    c(
      x %/% 3600,     # hours
      x %/% 60 %% 60, # minutes
      x %/% 1 %% 60   # seconds
    ),
    ncol = 3
  )

  # Pad single digits with zeroes
  x[] <- str_pad(x, 2, side = "left", pad = "0")

  x <- apply(x, 1, paste, collapse = ":")

  # Don't print leading zeroes for minutes and hours
  x <- str_replace(x, "^[0:]+([0-9:]+:[0-9]{2}$)", "\\1")

  x <- paste0(negative, x, remainder)
  x
}

trunc <- function(x, digits = 0, ...) {
  base::trunc(x * 10 ^ digits) / 10 ^ digits
}

#' @export
print.breaktime_duration <- function(x, digits = 0, ...) {
  x <- as.character(x, digits = digits, ...)
  NextMethod(quote = FALSE, right = TRUE)
}

#' @export
Summary.breaktime_duration <- function(x, ...) {
  as.breaktime_duration(NextMethod())
}
