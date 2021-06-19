#' @export
erase <- function() {
	paths <- get_scribble_paths()
	filepath <- paths[["filepath"]]
	if (!file.exists(filepath))
		return(invisible(NULL))
	# Warn about erasing notes
	title <- "Erase scribblr notes"
	msg <- paste0("This operation will erase your notes located at ", filepath,
				  ".", " Do you wish to continue? (y/n)"
				  )
	ans <- "?"
	while (!is_valid_ans(ans))
		ans <- tolower( rstudioapi::showPrompt(title, msg, default = "n") )
	if (ans == "y")
		file.remove(filepath)
	return(invisible(NULL))
}

is_valid_ans <- function(ans)
	identical(ans, "y") || identical(ans, "n")
