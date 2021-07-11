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
		keys::useKeys()
		,shinyjs::useShinyjs()

		,tags$head(tags$script(HTML('
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
				inputId = "closeButton", label = "Close (Esc)"
				)
		)

		,miniContentPanel(

			conditionalPanel(
				condition = "input.previewButton % 2 == 0",
				textAreaInput(
					inputId = "noteIO",
					label = NULL,
					value = txt,
					placeholder = scribblr_placeholder(),
					width = "100%",
					height = "280px"
				)
			)

			,conditionalPanel(
				condition = "input.previewButton % 2 == 1",
				uiOutput("preview")
			)

		)

		,miniButtonBlock(

			keys::keysInput("previewKeys",
				c("ctrl+p", "command+p"),
				global = TRUE
			)
			,actionButton(
				inputId = "previewButton",
				label = "Toggle preview (Ctrl+P)",
				icon = icon("markdown")
			)

			,keys::keysInput("saveToFileKeys",
					   c("ctrl+s", "command+s"),
					   global = TRUE
					   )
			,actionButton(
				inputId = "saveToFileButton",
				label = "Save to file (Ctrl+S)",
				icon = icon("save")
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
		### c.f. https://stackoverflow.com/questions/6140632/
		observe(shinyjs::runjs("
			document.getElementById('noteIO').addEventListener('keydown',
			function(e) {
				if (e.key == 'Tab') {
    				e.preventDefault();
    				var start = this.selectionStart;
    				var end = this.selectionEnd;

    				this.value = this.value.substring(0, start) +
    					\"\t\" + this.value.substring(end);

    				this.selectionStart = this.selectionEnd = start + 1;
			}
		});"))

		observeEvent(input$loaded, {
			session$sendCustomMessage("focus", list(NULL))
		})

		observeEvent(input$closeButton, {
			write(input$noteIO, filepath, append = F)
			invisible(stopApp())
		}, ignoreInit = TRUE)

		output$preview <- renderUI(markdown(input$noteIO))

		observeEvent(
			list(input$saveToFileKeys, input$saveToFileButton),
		{
			try({
				write(input$noteIO, file.choose(new = T), append = F)
			}, silent = TRUE)
			session$sendCustomMessage("focus", list(NULL))
		}, ignoreInit = TRUE)


		observeEvent(
			input$previewKeys,
			shinyjs::click("previewButton"),
			ignoreInit = TRUE
			)
		observeEvent(input$previewButton, {
			session$sendCustomMessage("focus", list(NULL))
		}, ignoreInit = TRUE)
	}


	#------------------------------------------------------------------- Run App
	viewer <- dialogViewer(dialogName = "scribblr", width = 800, height = 600)
	runGadget(ui, server, viewer = viewer, stopOnCancel = FALSE)
}
