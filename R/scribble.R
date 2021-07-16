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
	assert_rs_is_available()

	data <- scribble_init(note)

	#------------------------------------------------------------ User Interface
	ui <- miniPage(
		NULL

		# Plugins
		,shinyjs::useShinyjs()
		,keys::useKeys()

		# Set focus on text area input
		,tags$head(tags$script(HTML('
			$(document).on("shiny:connected", function(){
				Shiny.setInputValue("loaded", 1);
				Shiny.addCustomMessageHandler("focus",
					function(el) {
						document.getElementById(el).focus();
						})
				});
			')))

		# Hotkeys
		,keys::keysInput("sendKeys",
						 c("ctrl+s", "command+s"),
						 global = TRUE
		)
		,keys::keysInput("previewKeys",
						c("ctrl+p", "command+p"),
						global = TRUE
		)
		,keys::keysInput("editKeys",
						 c("ctrl+e", "command+e"),
						 global = TRUE
		)


		# Title bar
		,gadgetTitleBar(
			data[["title"]],
			left = dropdownButton(
				inputId = "sendDropdown",
				label = "Send... (Ctrl+S[+Tab])",
				icon = icon("share"),
				status = "primary",
				circle = FALSE,
				up = FALSE,
				right = FALSE,
				actionBttn(
					inputId = "saveToFileButton",
					label = "...to File",
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
			),
			right = miniTitleBarCancelButton(
				inputId = "doneButton", label = "Done (Esc)"
				)
		) # gadgetTitleBar

		# Text/preview area
		,miniTabstripPanel(
			id = "textAreaTabPanel",
			miniTabPanel(
				"Edit (Ctrl+E)", icon = icon("edit"),
				textAreaInput(
					inputId = "noteIO",
					label = NULL,
					value = data[["txt"]],
					placeholder = scribblr_placeholder(note),
					width = "100%",
					height = "310px"
				)
			),
			miniTabPanel(
				"Preview (Ctrl+P)", icon = icon("markdown"),
				uiOutput("preview")
			),
			miniTabPanel(
				"About", icon = icon("info-circle"),
				sidebarLayout(
					sidebarPanel = sidebarPanel(
						NULL
						,br()
						,img(src = "img/logo.png", width = 200)
					),
					mainPanel = mainPanel(
						NULL
						,h2(paste0("{scribblr} v", data[["ver"]]),
							align = "center"
							)
						,p("Author: Valerio Gherardi",
						   a("vgherard@sissa.it",
						     href = "mailto:vgherard@sissa.it"
						     ),
						   align = "center"
						   )

						# Links and share
						,br()
						,fluidRow(
							NULL
							,column(
								5, offset = 1
								,h4(icon("link"), "Links")
								,a(icon("github fa-1x"), "GitHub",
								   href = "https://github.com/vgherard/scribblr"
								)
								,br()
								,a(icon("bug fa-1x"), "Bug Reports",
								   href = "https://github.com/vgherard/scribblr/issues"
								)
							)
							,column(
								5, offset = 1
								,h4(icon("share-alt"), "Share")
								,a(icon("twitter fa-1x"), "Twitter",
								   href = "https://twitter.com/intent/tweet?text={scribblr}:%20A%20Minimalist%20Notepad%20Inside%20RStudio&url=https://github.com/vgherard/scribblr&via=ValerioGherardi&hashtags=rstats,rstudio,productivity"
								)
								,br()
								,a(icon("linkedin fa-1x"), "LinkedIn",
								   href = "https://www.linkedin.com/sharing/share-offsite/?url=https://github.com/vgherard/scribblr"
								)
							)
						)

					)
				)

			) # About miniTabPanel

		) # miniTabstripPanel
	)

	#-------------------------------------------------------------------- Server
	server <- function(input, output, session) {
		# Set focus on text area on load
		observeEvent(input$loaded, {
			session$sendCustomMessage("focus", list("noteIO"))
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

					const TAB_SIZE = 4;

        			// The one-liner that does the magic
        			document.execCommand('insertText', false, ' '.repeat(TAB_SIZE));
					return;
			}
		});"))

		# Edit note
		observeEvent(
			input$editKeys, {
			updateTabsetPanel(
				session, "textAreaTabPanel", selected = "Edit (Ctrl+E)"
			)
			}, ignoreInit = TRUE, priority = 1
		)
		observeEvent(
			input$textAreaTabPanel, {
				if (input$textAreaTabPanel == "Edit (Ctrl+E)")
					session$sendCustomMessage("focus", list("noteIO"))
			}, ignoreInit = TRUE, priority = 0
		)


		# Markdown preview
		output$preview <- renderUI(markdown(input$noteIO))
		observeEvent(
			input$previewKeys, {
				updateTabsetPanel(
					session, "textAreaTabPanel", selected = "Preview (Ctrl+P)"
					)
			}, ignoreInit = TRUE)


		# Export dropdown menu
		observeEvent(input$sendKeys, {
			toggleDropdownButton(inputId = "sendDropdown", session = session)
			}, ignoreInit = TRUE)

		observeEvent(input$sendDropdown_state, {
			if (input$sendDropdown_state)
				session$sendCustomMessage("focus", list("saveToFileButton"))
			else
				session$sendCustomMessage("focus", list("noteIO"))
			}, ignoreInit = TRUE)

		observeEvent(input$saveToFileButton, {
			try({
				write(input$noteIO, file.choose(new = T), append = F)
			}, silent = TRUE)
			session$sendCustomMessage("focus", list("noteIO")) # refocus text
		}, ignoreInit = TRUE)

		observeEvent(input$ghIssueButton, {
			title <- ifelse(!is.null(note), note, "")
			post_github_issue(title = title, body = input$noteIO)
			session$sendCustomMessage("focus", list("noteIO")) # refocus text
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
