ask_yesno_qn <- function(qn) {
	prompt <- paste(qn, "(y/n)> ")
	while (!is_valid_ans(ans <- readline(prompt)))
		next;
	return(ans == "y")
}

is_valid_ans <- function(ans)
	identical(ans, "y") || identical(ans, "n")
