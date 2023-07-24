`%||%` <- function(lhs, rhs) {
  if (is.null(lhs)) rhs else lhs
}

backtick <- function(x) {
  paste0("`", x, "`")
}

quote <- function(x) {
  paste0('"', x, '"')
}
