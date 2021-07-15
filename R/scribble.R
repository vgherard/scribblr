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
#' @param note either \code{NULL}, or a length one character (not \code{NA}).
#' See details.
#' @return returns \code{NULL}, invisibly. Called for side-effects.
#' @details
#' \code{scribblr} integrates a minimalist note editor within RStudio,
#' useful for taking quick project-related notes without distractions.
#' \code{scribblr} notes are RStudio project aware: each project has associated
#' its own notes, which can be accessed by calling \code{scribble()}, and are
#' stored into the project's root directory. Using \code{scribble()} without
#' any active project will take the R home directory as root.
#'
#' Calling \code{scribble()} with the default \code{note = NULL} gives access to
#' the main project notes. Otherwise, \code{note} must be a string specifying a
#' valid filename.
#'
#' \code{scribblr} notes and settings for the active project are stored in the
#' \code{".scribblr"} directory, under the project's root. If this, or the note
#' specified by \code{note}, do not exist, the user will be prompted for
#' permission to create the required files/directories.
#'
#' Notes are autosaved when the editor is closed; until that
#' moment, the R session will remain busy.
#'
#' \code{scribble()} can also be called (and, in particular, associated a
#' custom keystroke) via the RStudio Addin "Open scribblr note editor".
#' @examples
#' \dontrun{
#' scribble()
#' }
#' @export
scribble <- function(note = NULL) {
	data <- scribble_init(note)

	#------------------------------------------------------------ User Interface
	ui <- miniPage(
		keys::useKeys()
		,shinyjs::useShinyjs()

		# Set focus on text area input
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
			data[["title"]],
			left = img(src = "img/logo.png", width = 39),

			right = miniTitleBarCancelButton(
				inputId = "doneButton", label = "Done (Esc)"
				)
		)

		,miniContentPanel(

			conditionalPanel(
				condition = "input.previewButton % 2 == 0",
				textAreaInput(
					inputId = "noteIO",
					label = NULL,
					value = data[["txt"]],
					placeholder = scribblr_placeholder(note),
					width = "100%",
					height = "280px"
				)
			)

			,conditionalPanel(
				condition = "input.previewButton % 2 == 1",
				uiOutput("preview")
			)

		)

		,fillRow(height = "10%"
			,keys::keysInput("previewKeys",
				c("ctrl+p", "command+p"),
				global = TRUE
			)
			,materialSwitch(
				inputId = "previewButton",
				label = "Preview (Ctrl+P)",
				value = FALSE,
				status = "primary",
				inline = T
			)



			# ,switchInput(
			# 	inputId = "previewButton"
			# 	,label = icon("markdown")
			# 	#,size = "mini"
			# 	)
			# ,actionButton(
			# 	inputId = "previewButton",
			# 	label = "Toggle preview (Ctrl+P)",
			# 	icon = icon("markdown")
			# )

			,keys::keysInput("exportKeys",
					   c("ctrl+e", "command+e"),
					   global = TRUE
					   )
			# ,keys::keysInput("saveToFileKeys",
			# 				 c("ctrl+e+f", "command+e+f"),
			# 				 global = TRUE
			# )
			# ,actionButton(
			# 	inputId = "saveToFileButton",
			# 	label = "Save to file (Ctrl+S)",
			# 	icon = icon("save")
			# )
			,dropdownButton(
				inputId = "exportDropdown",
				label = "Export...",
				icon = icon("sliders"),
				status = "primary",
				circle = FALSE,
				up = TRUE,
				right = TRUE,
				actionBttn(
					inputId = "saveToFileButton",
					label = "...to file",
					icon = icon("save"),
					style = "fill",
					size = "xs"
				),
				actionBttn(
					inputId = "ghIssueButton",
					label = "...to GitHub Issues",
					icon = icon("github"),
					style = "fill",
					size = "xs"
				)
			)
		)

		,a(
			align = "center",
			paste0("{scribblr} v", data[["ver"]]),
			icon("github fa-1x"),
			href = "https://github.com/vgherard/scribblr"
		)
	)

	#-------------------------------------------------------------------- Server
	server <- function(input, output, session) {
		# Set focus on text area on load
		observeEvent(input$loaded, {
			session$sendCustomMessage("focus", list(NULL))
		})

		# Use tab to indent in text area
		# See https://stackoverflow.com/questions/6140632/
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


		# Markdown preview
		output$preview <- renderUI(markdown(input$noteIO))
		observeEvent(
			input$previewKeys,
			shinyjs::click("previewButton"),
			ignoreInit = TRUE
		)
		observeEvent(input$previewButton, {
			session$sendCustomMessage("focus", list(NULL))  # refocus text area
		}, ignoreInit = TRUE)


		# Export dropdown menu
		observeEvent(input$exportKeys, {
			toggleDropdownButton(inputId = "exportDropdown", session = session)
			}, ignoreInit = TRUE)

		observeEvent(input$exportDropdown_state, {
			if (!input$exportDropdown_state)
				session$sendCustomMessage("focus", list(NULL))
			}, ignoreInit = TRUE)

		observeEvent(input$saveToFileButton, {
			try({
				write(input$noteIO, file.choose(new = T), append = F)
			}, silent = TRUE)
			session$sendCustomMessage("focus", list(NULL)) # refocus text
		}, ignoreInit = TRUE)

		observeEvent(input$ghIssueButton, {
			title <- ifelse(!is.null(note), note, "")
			post_github_issue(title = title, body = input$noteIO)
			session$sendCustomMessage("focus", list(NULL)) # refocus text
		}, ignoreInit = TRUE)

		# Done
		observeEvent(input$doneButton, {
			write(input$noteIO, data[["note_path"]], append = F)
			stopApp()
		}, ignoreInit = TRUE)

	}


	#------------------------------------------------------------------- Run App
	viewer <- dialogViewer(dialogName = "scribblr", width = 800, height = 600)
	runGadget(ui, server, viewer = viewer, stopOnCancel = FALSE)
}

scribble_init <- function(note) {
	scribblr_dir_create(check = TRUE)
	scribblr_note_create(note = note, check = TRUE)

	proj <- get_cur_proj()
	note_path <- scribblr_note_path(note = note)
	txt <- paste0(readLines(note_path), collapse = "\n")


	note_name <- basename(note_path)
	title <- paste0("'", note_name, "'", " @ RStudio")
	if (proj[["is_r_project"]])
		title <- paste0(title, " project '", basename(proj[["dir"]]), "'")

	ver <- packageVersion("scribblr")

	addResourcePath("img", system.file("img", package = "scribblr"))

	list(
		note = note, note_path = note_path, txt = txt, title = title, ver = ver
		)
}
