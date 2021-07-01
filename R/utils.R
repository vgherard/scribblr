get_scribble_paths <- function()
{
	dir <- rstudioapi::getActiveProject()
	is_r_project <- !is.null(dir)
	# If there is no active project, use the R Home directory as base
	if (!is_r_project) {
		dir <- fs::path_home_r()
	}
	filepath <- paste0(dir, "/", ".scribblr")
	list(dir = dir, filepath = filepath, is_r_project = is_r_project)
}

create_scribble_file <- function(dir, filepath)
{
	file.create(filepath)
	cat("A new {scribblr} note file was created at:\n", filepath, "\n")
	if (file.exists(paste0(dir, "/.Rbuildignore")))
		usethis::use_build_ignore(".scribblr")
}

ask_yesno_qn <- function(qn) {
	prompt <- paste(qn, "(y/n)> ")
	while (!is_valid_ans(ans <- readline(prompt)))
		next;
	return(ans == "y")
}

is_valid_ans <- function(ans)
	identical(ans, "y") || identical(ans, "n")
