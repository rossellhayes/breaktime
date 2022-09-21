`%||%` <- function(lhs, rhs) {
  if (length(lhs) == 0) rhs else lhs
}

str_pad <- function(x, width) {
  gsub(" ", "\u00a0", format(x, justify = "left", width = width))
}
