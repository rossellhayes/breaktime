in_seconds <- function(x, units = NULL) {
  normalized_units <- normalize_units(units) %||% "seconds"

  multiplier <- rep(1, length(x))
  multiplier[normalized_units == "minutes"] <- 60
  multiplier[normalized_units == "hours"] <- 3600 # 60 * 60
  multiplier[normalized_units == "days"] <- 86400 # 3600 * 24
  multiplier[normalized_units == "weeks"] <- 604800 # 86400 * 7
  multiplier[normalized_units == "fortnights"] <- 1209600 # 86400 * 14
  multiplier[normalized_units == "years"] <- 31556952 # 86400 * (365 + 97 / 400)
  multiplier[normalized_units == "months"] <- 2629746 # 31556952 / 12
  multiplier[normalized_units == "decades"] <- 315569520 # 31556952 * 10
  multiplier[normalized_units == "centuries"] <- 3155695200 # 31556952 * 100
  multiplier[normalized_units == "millennia"] <- 31556952000 # 31556952 * 1000

  x * multiplier
}

breaktime_units <- c(
  "seconds",
  "minutes",
  "hours",
  "days",
  "weeks",
  "fortnights",
  "months",
  "years",
  "decades",
  "centuries",
  "millennia"
)

# @staticimports pkg:stringstatic
#  str_remove

normalize_units <- function(units) {
  normalized_units <- str_remove(units, "s$")

  aliases <- c(
    "minutes" = "m",
    "minutes" = "mi",
    "hours" = "hrs",
    "days" = "d",
    "weeks" = "wks",
    "years" = "yrs",
    "centuries" = "century",
    "millennia" = "millennium"
  )

  valid_units <- c(breaktime_units, aliases)

  normalized_units <- valid_units[
    pmatch(normalized_units, valid_units, duplicates.ok = TRUE)
  ]
  normalized_units <- unname(normalized_units)

  normalized_units[normalized_units %in% aliases] <- names(aliases)[
    match(normalized_units, aliases)
  ]

  if (anyNA(normalized_units)) {
    cli::cli_abort(c(
      "{.arg units} should be one of {.or {.val {breaktime_units}}}.",
      "x" = "{.val {units[is.na(normalized_units)]}}
        could not be unambiguously matched to a valid unit."
    ))
  }

  normalized_units
}
