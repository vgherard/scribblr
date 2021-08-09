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

scribblr_dir_name <- function()
	".scribblr"

main_note_name <- function()
	"main"

scribblr_dir <- function(where = get_cur_proj()[["dir"]])
	file.path(where, scribblr_dir_name())

scribblr_dir_exists <- function(where = get_cur_proj()[["dir"]])
	dir.exists(scribblr_dir(where))

scribblr_dir_create <- function(where = get_cur_proj()[["dir"]], check = TRUE)
{
	scribblr_dir <- scribblr_dir(where)

	if (check)
	{
		if (dir.exists(scribblr_dir))
			return()
		warn_missing_scribblr_dir()
		if (!ask_yesno_qn("Should I create one?"))
			stop("Execution aborted by user.")
	}

	dir.create(scribblr_dir)
	dir.create(file.path(scribblr_dir, "notes"))
	file.create(file.path(scribblr_dir, "notes", main_note_name()))
	cat("A new {scribblr} directory was created at:\n", scribblr_dir, "\n")

	if ( file.exists(file.path(where, ".Rbuildignore")) )
		usethis::use_build_ignore(scribblr_dir_name())
}

scribblr_note_path <- function(note, where = get_cur_proj()[["dir"]])
{
	assert_is_note_name(note)

	if (is.null(note))
		return(file.path(scribblr_dir(where), "notes", main_note_name()))

	return(file.path(scribblr_dir(where), "notes", note, note))
}

scribblr_note_create <- function(
	where = get_cur_proj()[["dir"]], note, check = TRUE
)
{
	assert_is_note_name(note)
	path <- scribblr_note_path(where = where, note = note)

	if (check)
	{
		if (file.exists(path))
			return()
		warn_missing_note()
		if (!ask_yesno_qn("Should I create it?"))
			stop("Execution aborted by user.")
	}

	dir <- dirname(path)
	if (!exists(dir))
		dir.create(dir)
	file.create(path)
}
