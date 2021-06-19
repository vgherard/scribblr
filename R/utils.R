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
