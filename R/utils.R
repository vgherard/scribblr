get_scribblr_path <- function()
{
	dir <- rstudioapi::getActiveProject()
	is_r_project <- !is.null(dir)
	# If there is no active project, use the R Home directory as base
	if (!is_r_project) {
		dir <- fs::path_home_r()
	}
	list(dir = dir, is_r_project = is_r_project)
}

scribblr_filepath <- function(dir)
	paste0(dir, "/", ".scribblr")

check_scribblr_file <- function(dir)
{
	filepath <- scribblr_filepath(dir)
	if (file.exists(filepath))
		return(TRUE)
	cat("No {scribblr} file found at:\n", filepath, "\n")
	if ( !ask_yesno_qn("Do you want to create one?") )
		return(FALSE)
	file.create(filepath)
	cat("A new {scribblr} note file was created at:\n", filepath, "\n")
	if (file.exists(paste0(dir, "/.Rbuildignore")))
		usethis::use_build_ignore(".scribblr")
	return(TRUE)
}

ask_yesno_qn <- function(qn) {
	prompt <- paste(qn, "(y/n)> ")
	while (!is_valid_ans(ans <- readline(prompt)))
		next;
	return(ans == "y")
}

is_valid_ans <- function(ans)
	identical(ans, "y") || identical(ans, "n")
