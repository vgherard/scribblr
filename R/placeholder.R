scribblr_placeholder <- function() {
	proj <- get_cur_proj();
	paste0(
		"Welcome to {scribblr}!\n\n"
		,"You can use this space to take quick notes about the current project."
		," The content of this text area will be automatically saved"
		," upon closing this window.\n\n"
		,"If you encounter a bug or want to suggest an improvement, ",
		"please follow the GitHub link at the bottom of this window and file ",
		"an issue in the {scribblr} repository."
	)
}
