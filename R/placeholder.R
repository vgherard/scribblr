scribblr_placeholder <- function(note) {
	welcome <- "Welcome to {scribblr}!"

	msg <- ifelse(
		is.null(note),
		paste0(
			"You can use this space to take quick notes about the current "
			,"project. The content of this text area will be automatically "
			,"saved upon closing this window."
			,"\n\n"
			,"If you want to organize your notes in separate files, try "
			,"`scribblr::scribble(note_name)`, where `note_name` is the title "
			,"of your note."),
		paste0(
			"The content of this text area will be automatically "
			,"saved upon closing this window."
			)
		)

	bugs <- paste0(
		"If you encounter a bug or want to suggest an improvement, "
		,"please follow the GitHub link at the bottom of this window and file "
		,"an issue in the {scribblr} repository."
		)

	paste0(welcome, "\n\n", msg, "\n\n", bugs)
}
