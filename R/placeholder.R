scribblr_placeholder <- function() {
	path <- get_scribblr_path(); filepath <- scribblr_filepath(path[["dir"]])
	paste0(
		"Welcome to {scribblr}!\n\n"
		,"You can use this space to take quick notes about the current project."
		," The content of this text area will be automatically saved in ",
		filepath, " upon closing this window.\n\n"
		,"If you encounter a bug or want to suggest an improvement, ",
		"please follow the GitHub link at the bottom of this page and file ",
		"an issue in the {scribblr} repository."
	)
}
