# scribblr
# Copyright (C) 2021  Valerio Gherardi
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

warn_missing_note <- function()
	cat("This {scribblr} note does not exist.", file = stderr())

warn_missing_scribblr_dir <- function()
	cat("No {scribblr} directory found.\n", file = stderr())

assert_is_string <- function(
	x, can_be_null = FALSE, name = deparse(substitute(x))
	)
{
	p <- can_be_null && is.null(x)
	q <- is.character(x) && length(x) == 1 && !is.na(x)
	if (p || q) return()

	msg <- paste0("'", name, "' ",
		"must be", ifelse(can_be_null, " either NULL or ", " "),
		"a length one character (not NA)."
		)

	stop(msg)
}

assert_is_note_name <- function(x, name = deparse(substitute(x)))
{
	assert_is_string(x, can_be_null = TRUE, name = name)

	if (is.null(x) || !grepl("[^[:alnum:]_.]", x))
		return()

	msg <- paste0("'", name, "' ",
				  "can contain only alphanumeric characters, ",
				  "underscores (_) and dots (.)"
	)

	stop(msg)
}

assert_rs_is_available <- function(
	msg = paste0("`", deparse(sys.calls()[[sys.nframe() - 1]]), "`", # function name
				 "can only be used inside RStudio."
				 )
)
{
	if (!rstudioapi::isAvailable())
		rlang::abort(msg, class = "rs_not_available_error")
}

ask_yesno_qn <- function(qn) {
	prompt <- paste(qn, "(y/n)> ")
	while (!is_valid_ans(ans <- readline(prompt)))
		next;
	return(ans == "y")
}

is_valid_ans <- function(ans)
	identical(ans, "y") || identical(ans, "n")
