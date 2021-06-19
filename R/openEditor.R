#' @export
openEditor <- function() {
	base_dir <- rstudioapi::getActiveProject()
	# If there is no active project, use the R Home directory as base
	if (is.null(base_dir)) {
		base_dir <- fs::path_home_r()
	}

	note_dir <- paste0(base_dir, "/", ".rstudionotes")
	if (!dir.exists(note_dir)) {
		msg <- paste0("No notes directory found at ", note_dir, "; ")
		msg <- paste0(msg, "Should I automatically create one?")
		qn_create_dir <- rstudioapi::showQuestion(
			"Notes directory not found", msg, ok = "Yes", cancel = "No"
			)
		if (!qn_create_dir)
			return(invisible(NULL))
		dir.create(note_dir)
	}

	get_file_list <- function()
		as.list(list.files(note_dir))

	#------------------------------------------------------------ User Interface
	ui <- miniPage(
		shinyjs::useShinyjs()
		,gadgetTitleBar(
			paste("Notes for R project at ", base_dir),
			left = NULL,
			right = miniTitleBarCancelButton(inputId = "close", label = "Close (Esc)")
			)
		,sidebarLayout(
			div(id = "sidebar",
				sidebarPanel(
					uiOutput("currentNoteSelector")
					,keys::useKeys()
					,actionButton("newNoteButton", "New note (Ctrl+N)")
					,keys::keysInput("newNoteKeys", c("ctrl+n", "command+n"))
					,keys::keysInput("nextNoteKeys", c("ctrl+down", "command+down"))
					,keys::keysInput("previousNoteKeys", c("ctrl+up", "command+up"))
					,br()
					,actionButton("discardNoteButton", "Discard note (Ctrl + D)")
					,keys::keysInput("discardNoteKeys", c("ctrl+d", "command+d"))
					,br()
					,p("Navigate notes: Ctrl+up/down")
					# Work-in-progress
					# ,br()
					# ,checkboxInput("previewCheckbox", "Preview", value = F)
					)
				)
			,mainPanel(
				conditionalPanel(
					condition = "input.currentNoteSelector != \"\""
					,uiOutput("noteInputOutput")
					#,uiOutput("notePreview") # work-in-progress
					)
				)
			)
		)


	#-------------------------------------------------------------------- Server
	server <- function(input, output, session) {
		notes <- reactiveVal(get_file_list())
		current_note <- reactiveVal("")
		current_note_path <- function()
			paste0(note_dir, "/", current_note())
		save_current_note <- function()
			if (current_note() != "")
				write(input$noteInputOutput, current_note_path(), append = F)

		output$currentNoteSelector <- renderUI({
			selectInput("currentNoteSelector",
				    label = NULL,
				    choices = c("Select note" = "", notes())
				    )
		})

		output$noteInputOutput <- renderUI({
			txt <- ""
			if (current_note() != "") {
				note_lines <- readLines(current_note_path())
				txt <- paste0(txt, note_lines, collapse = "\n")
			}
			textAreaInput("noteInputOutput",
						  label = NULL,
						  value = txt,
						  width = "100%",
						  height = "300px"
						  )
		})

		# Work-in-progress
		# output$notePreview <- renderUI({
		# 	if (length(input$noteInputOutput) > 0)
		# 		HTML(markdown::markdownToHTML(text = input$noteInputOutput))
		# })

		observeEvent({
			paste(input$newNoteButton, input$newNoteKeys)
		}, {
			save_current_note()
			name <- rstudioapi::showPrompt(
				"New note",
				"Insert note title",
				default = "New note"
				)
			if (file.exists(paste0(note_dir, "/", name))) {
				n <- 1
				while (file.exists(paste0(note_dir, "/", name, " (", n, ")")))
					n <- n + 1
				name <- paste0(name, " (", n, ")")
			}
			file.create(paste0(note_dir, "/", name))
			current_note(name)
			notes(get_file_list())
			updateSelectInput(session,
					  "currentNoteSelector",
					  selected = name
					  )
		}, ignoreInit = TRUE
		)

		observeEvent(
			paste(input$discardNoteButton, input$discardNoteKeys),
			{
			if (current_note() != "") {
				file.remove(current_note_path())
				current_note("")
				notes(get_file_list())
			}
		})

		observeEvent(
			input$nextNoteKeys, {
				if (length(notes()) != 0) {
					names <- c("", notes())
					len <- length(names)
					i <- match(current_note(), names)
					i <- ifelse(i == len, 1, i + 1)
					updateSelectInput(session,
									  "currentNoteSelector",
									  selected = names[i]
					)
				}
			}
		)

		observeEvent(
			input$previousNoteKeys, {
				if (length(notes()) != 0) {
					names <- c("", notes())
					len <- length(names)
					i <- match(current_note(), names)
					i <- ifelse(i == 1, len, i - 1)
					updateSelectInput(session,
									  "currentNoteSelector",
									  selected = names[i]
					)
				}
			}
		)


		observeEvent({
			input$currentNoteSelector
			}, {
			save_current_note()
			current_note(input$currentNoteSelector)
			},
			priority = 1,
			ignoreInit = TRUE
			)

		observeEvent(input$currentNoteSelector, {
			if (current_note() != "")
				action <- shinyjs::enable
			else
				action <- shinyjs::disable
			action("discardNoteButton")
		})

		# observeEvent(input$previewCheckbox, {
		# 	if (input$previewCheckbox) {
		# 		shinyjs::show("notePreview")
		# 		shinyjs::hide("noteInputOutput")
		# 	} else {
		# 		shinyjs::hide("notePreview")
		# 		shinyjs::show("noteInputOutput")
		# 	}
		#
		# })

		observeEvent(input$close, {
			save_current_note()
			invisible(stopApp())
		}, ignoreInit = TRUE)
	}

	runGadget(ui, server,
		  viewer = dialogViewer(dialogName = "rstudionotes", width = 800, height = 600),
		  stopOnCancel = FALSE
		  )

}
