assert_is_string <- function(x, can_be_null)
{
	p <- can_be_null && is.null(x)
	q <- is.character(x) && length(x) == 1 && !is.na(x)
	if (p || q) return()

	what <- paste0("'", deparse(substitute(x)), "'")
	msg <- paste0(
		what, " must be", ifelse(can_be_null, " either NULL or ", " "),
		"a length one character (not NA)."
		)

	stop(msg)
}

ask_yesno_qn <- function(qn) {
	prompt <- paste(qn, "(y/n)> ")
	while (!is_valid_ans(ans <- readline(prompt)))
		next;
	return(ans == "y")
}

is_valid_ans <- function(ans)
	identical(ans, "y") || identical(ans, "n")
