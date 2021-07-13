get_cur_proj <- function()
{
	dir <- rstudioapi::getActiveProject()
	is_r_project <- !is.null(dir)
	# If there is no active project, use the R Home directory as base
	if (!is_r_project) {
		dir <- fs::path_home_r()
	}
	list(dir = dir, is_r_project = is_r_project)
}

scribblr_dir_name <- function() ".scribblr"

scribblr_dir <- function(proj_dir) file.path(proj_dir, scribblr_dir_name())

scribblr_dir_exists <- function(proj_dir)
	dir.exists(scribblr_dir(proj_dir))

scribblr_dir_create <- function(check = TRUE)
{
	proj_dir <- get_cur_proj()[["dir"]]
	scribblr_dir <- scribblr_dir(proj_dir)

	if (check)
	{
		if (dir.exists(scribblr_dir))
			return()
		cat("{scribblr} directory", scribblr_dir, "does not exist.\n"
			,file = stderr()
			)
		if (!ask_yesno_qn("Should I create one?"))
			stop("Execution aborted by user.")
	}

	dir.create(scribblr_dir)
	cat("A new {scribblr} note file was created at:\n", scribblr_dir, "\n")

	if (file.exists( file.path(proj_dir, ".Rbuildignore") ))
		usethis::use_build_ignore(scribblr_dir_name())
}

get_note_name <- function(note)
{
	if (missing(note))
		return("main")

	if (!is.character(note) && length(note) == 1 && !is.na(note))
			stop("'note' must be a character of length one (not NA).")
	paste0("00", note)
}

ask_yesno_qn <- function(qn) {
	prompt <- paste(qn, "(y/n)> ")
	while (!is_valid_ans(ans <- readline(prompt)))
		next;
	return(ans == "y")
}

is_valid_ans <- function(ans)
	identical(ans, "y") || identical(ans, "n")
