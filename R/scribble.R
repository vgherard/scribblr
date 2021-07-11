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

#' @title Open \code{scribblr} note editor
#'
#' @author Valerio Gherardi
#'
#' @description Opens the \code{scribblr} note editor in a new window.
#' @details
#' \code{scribblr} integrates a minimalist note editor within RStudio,
#' useful for taking quick project-related notes without distractions.
#' \code{scribblr} notes are RStudio project aware: each
#' project has associated its own note file, which can be
#' read and modified by calling \code{scribble()} from an RStudio session with
#' the active project. If \code{scribble()} is called during a session without
#' any active project, a global note file (located at the R home directory) is
#' accessed.
#'
#' Calling \code{scribble()} opens the \code{scribblr} project notes editor
#' in a new window. Notes are autosaved when the editor is closed; until that
#' moment, the R session will remain busy.
#'
#' \code{scribble()} can also be called (and, in particular, associated a
#' custom keystroke) via the RStudio Addin "Open scribblr note editor".
#' @examples
#' \dontrun{
#' scribble()
#' }
#' @export
scribble <- function() {
	path <- get_scribblr_path()
	dir <- path[["dir"]]
	if (!check_scribblr_file(dir)) {
		message("Execution aborted by user.")
		return(invisible(NULL))
	}
	filepath <- scribblr_filepath(dir)

	txt <- paste0(readLines(filepath), collapse = "\n")

	title <- "Notes for RStudio"
	if (path[["is_r_project"]])
		title <- paste(title, "project at", dir)

	ver <- packageVersion("scribblr")

	#------------------------------------------------------------ User Interface
	ui <- miniPage(
		tags$head(tags$script(HTML('
			$(document).on("shiny:connected", function(){
				Shiny.setInputValue("loaded", 1);
				Shiny.addCustomMessageHandler("focus",
					function(NULL) {
						document.getElementById("noteIO").focus();
						})
				});
			')))

		,gadgetTitleBar(
			title,
			left = NULL,
			right = miniTitleBarCancelButton(
				inputId = "close", label = "Close (Esc)"
				)
		)
		,miniContentPanel(
			textAreaInput(
				inputId = "noteIO",
				label = NULL,
				value = txt,
				placeholder = scribblr_placeholder(),
				width = "100%",
				height = "325px"
			)
		)
		,a(
			align = "center",
			paste0("{scribblr} v", ver),
			icon("github fa-1x"),
			href = "https://github.com/vgherard/scribblr"
		)
	)

	#-------------------------------------------------------------------- Server
	server <- function(input, output, session) {
		observeEvent(input$loaded, {
			session$sendCustomMessage("focus", list(NULL))
		})
		observeEvent(input$close, {
			write(input$noteIO, filepath, append = F)
			invisible(stopApp())
		}, ignoreInit = TRUE)
	}


	#------------------------------------------------------------------- Run App
	viewer <- dialogViewer(dialogName = "scribblr", width = 800, height = 600)
	runGadget(ui, server, viewer = viewer, stopOnCancel = FALSE)
}
