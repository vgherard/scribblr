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

#' @title Delete a \code{scribblr} note
#'
#' @author Valerio Gherardi
#'
#' @description Delete a \code{scribblr} note.
#' @param note a length one character (not NA). Note to be deleted.
#' @return returns \code{NULL}, invisibly. Used for side-effects.
#' @examples
#' \dontrun{
#' erase()
#' }
#' @seealso \link{scribble}
#' @export
scribblr_delete <- function(note) {
	path <- scribblr_note_path(note)

	if (!file.exists(path)) {
		warn_missing_note()
		return(invisible(NULL))
	}

	# Warn about erasing notes
	cat("This operation will erase your {scribblr} note and cannot be undone.")
	if (ask_yesno_qn("Do you want to proceed?"))
		file.remove(filepath)

	return(invisible(NULL))
}

