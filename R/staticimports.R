# Generated by staticimports; do not edit by hand.
# ======================================================================
# Imported from pkg:stringstatic
# ======================================================================

#' Duplicate and concatenate strings within a character vector
#'
#' Dependency-free drop-in alternative for `stringr::str_pad()`.
#'
#' @author Eli Pousson \email{eli.pousson@gmail.com}
#'   ([ORCID](https://orcid.org/0000-0001-8280-1706))
#'
#'   Alexander Rossell Hayes \email{alexander@rossellhayes.com}
#'   ([ORCID](https://orcid.org/0000-0001-9412-0457))
#'
#' @source Adapted from the [stringr](https://stringr.tidyverse.org/) package.
#'
#' @param string Input vector.
#'   Either a character vector, or something coercible to one.
#' @param width Minimum width of padded strings.
#' @param side Side on which padding character is added (left, right or both).
#' @param pad Single padding character (default is a space).
#' @param use_width If `FALSE`,
#'   use the length of the string instead of the width;
#'   see [str_width()]/[str_length()] for the difference.
#'
#' @return A character vector.
#' @noRd
str_pad <- function(
	string, width, side = c("left", "right", "both"), pad = " ", use_width = TRUE
) {
	if (!is.numeric(width)) {
		return(string[NA])
	}

	if (any(nchar(pad, type = "width") != 1)) {
		stop("each string in `pad` should consist of code points of total width 1")
	}

	side <- match.arg(side)

	nchar_type <- if (isTRUE(use_width)) "width" else "chars"
	string_width <- nchar(string, nchar_type)
	pad_width <- width - string_width
	pad_width[pad_width < 0] <- 0

	switch(
		side,
		"left" = paste0(strrep(pad, pad_width), string),
		"right" = paste0(string, strrep(pad, pad_width)),
		"both" = paste0(
			strrep(pad, floor(pad_width / 2)),
			string,
			strrep(pad, ceiling(pad_width / 2))
		)
	)
}

#' Remove matched patterns in a string
#'
#' Dependency-free drop-in alternative for `stringr::str_remove()`.
#'
#' @source Adapted from the [stringr](https://stringr.tidyverse.org/) package.
#'
#' @param string Input vector.
#'   Either a character vector, or something coercible to one.
#'
#' @param pattern Pattern to look for.
#'
#'   The default interpretation is a regular expression,
#'   as described in [base::regex].
#'   Control options with [regex()].
#'
#'   Match a fixed string (i.e. by comparing only bytes), using [fixed()].
#'   This is fast, but approximate.
#'
#' @return A character vector.
#' @noRd
str_remove <- function(string, pattern) {
	if (length(string) == 0 || length(pattern) == 0) return(character(0))
	is_fixed <- inherits(pattern, "stringr_fixed")
	Vectorize(sub, c("pattern", "x"), USE.NAMES = FALSE)(
		pattern, replacement = "", x = string, perl = !is_fixed, fixed = is_fixed
	)
}

#' Replace matched patterns in a string
#'
#' Dependency-free drop-in alternative for `stringr::str_replace()`.
#'
#' @source Adapted from the [stringr](https://stringr.tidyverse.org/) package.
#'
#' @param string Input vector.
#'   Either a character vector, or something coercible to one.
#'
#' @param pattern Pattern to look for.
#'
#'   The default interpretation is a regular expression,
#'   as described in [base::regex].
#'   Control options with [regex()].
#'
#'   Match a fixed string (i.e. by comparing only bytes), using [fixed()].
#'   This is fast, but approximate.
#'
#' @param replacement A character vector of replacements.
#'   Should be either length one, or the same length as `string` or `pattern`.
#'   References of the form `\1`, `\2`, etc. will be replaced with the contents
#'   of the respective matched group (created by `()`).
#'
#'   To replace the complete string with `NA`,
#'   use `replacement = NA_character_`.
#'
#'   Using a function for `replacement` is not yet supported.
#'
#' @return A character vector.
#' @noRd
str_replace <- function(string, pattern, replacement) {
	if (length(string) == 0 || length(pattern) == 0 || length(replacement) == 0) {
		return(character(0))
	}

	is_fixed <- inherits(pattern, "stringr_fixed")

	Vectorize(sub, c("pattern", "replacement", "x"), USE.NAMES = FALSE)(
		pattern, replacement, x = string, perl = !is_fixed, fixed = is_fixed
	)
}

#' Compute the width of a string
#'
#' Dependency-free drop-in alternative for `stringr::str_width()`.
#' Results for non-ASCII characters may be inaccurate in R < 4.0.
#'
#' @source Adapted from the [stringr](https://stringr.tidyverse.org/) package.
#'
#' @param string Input vector.
#'   Either a character vector, or something coercible to one.
#'
#' @return A numeric vector the same length as string.
#' @noRd
str_width <- function(string) {
	nchar(as.character(string), type = "width", keepNA = TRUE)
}
