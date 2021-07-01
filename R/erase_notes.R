# scribblr
# Copyright (C) 2021  Valerio Gherardi
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

#' @title Erase \code{scribblr} notes
#'
#' @author Valerio Gherardi
#'
#' @description Deletes the \code{scribblr} note file associated to the active
#' RStudio project (or to the global RStudio session, if no project is active).
#' @examples
#' \dontrun{
#' erase()
#' }
#' @seealso \link{scribble}
#' @export
erase_notes <- function() {
	dir <- get_scribblr_path()[["dir"]]
	filepath <- scribblr_filepath(dir)
	if (!file.exists(filepath)) {
		cat("No {scribblr} note file to be deleted.")
		return(invisible(NULL))
	}

	# Warn about erasing notes
	cat("This operation will erase your {scribblr} notes.")
	if (ask_yesno_qn("Do you want to continue?"))
		file.remove(filepath)

	return(invisible(NULL))
}

