#' @export
openNoteEditorAddin <- function() {

	# Get the document context.
	context <- rstudioapi::getActiveDocumentContext()

	# Set the default data to use based on the selection.
	text <- context$selection[[1]]$text
	defaultData <- text

	# Generate UI for the gadget.
	ui <- miniPage(
		gadgetTitleBar("Notes"),
		miniContentPanel(
			fluidRow(
				uiOutput("currentNoteSelector")
				,actionButton("newNoteButton", "New note")
			)
			,hr()
			,conditionalPanel(
				condition = "input.currentNoteSelector != \"\""
				,fluidRow(
					actionButton("deleteNoteButton", "Delete note")
					,checkboxInput("previewCheckbox", "Preview Markdown")
					,uiOutput("textOutput")
					)
				,conditionalPanel(
					condition = "input.previewCheckbox"
					,uiOutput("previewOutput")
					)
				)
			)
		)

	# Server code for the gadget.
	server <- function(input, output, session) {

		# Get list of notes
		cur_proj <- getActiveProject()
		note_dir <- paste0(cur_proj, "/", ".rstudiomemo")
		if (!dir.exists(note_dir))
			dir.create(note_dir)

		get_file_list <- function()
			lapply(list.files(note_dir),
			       function(filename) gsub(".md$", "", filename)
			       )

		notes <- reactiveVal(get_file_list())

		# Render note selector
		output$currentNoteSelector <- renderUI({
			selectInput("currentNoteSelector",
				    label = p("Current note"),
				    choices = c("Select note" = "", notes())
				    )
		})

		output$textOutput <- renderUI({
			name <- input$currentNoteSelector
			if (name != "") {
				path <- paste0(note_dir, "/", name, ".md")
				txt <- readLines(path)
				textAreaInput("textOutput", label = "Note", value = txt)
			}
		})

		output$previewOutput <- renderUI({
			name <- input$currentNoteSelector
			if (name != "" && input$previewCheckbox) {
				path <- paste0(note_dir, "/", name, ".md")
				includeMarkdown(path)
			}
		})

		observeEvent(input$newNoteButton, {
			name <- rstudioapi::showPrompt(
				"New note",
				"Insert note title"
				)
			file.create(paste0(note_dir, "/", name, ".md"))
			notes(get_file_list())
			updateSelectInput(session,
					  "currentNoteSelector",
					  selected = name
					  )
		})

		observeEvent(input$deleteNoteButton, {
			name <- input$currentNoteSelector
			file.remove(paste0(note_dir, "/", name, ".md"))
			notes(get_file_list())
			updateSelectInput(session,
					  "currentNoteSelector",
					  selected = ""
			)
		})

		# Render note markdown
		#includeMarkdown(.......)

	# 	reactiveData <- reactive({
	#
	# 		# Collect inputs.
	# 		dataString <- input$data
	# 		subsetString <- input$subset
	#
	# 		# Check to see if there is data called 'data',
	# 		# and access it if possible.
	# 		if (!nzchar(dataString))
	# 			return(errorMessage("data", "No dataset available."))
	#
	# 		if (!exists(dataString, envir = .GlobalEnv))
	# 			return(errorMessage("data", paste("No dataset named '", dataString, "' available.")))
	#
	# 		data <- get(dataString, envir = .GlobalEnv)
	#
	# 		if (!nzchar(subsetString))
	# 			return(data)
	#
	# 		# Try evaluating the subset expression within the data.
	# 		condition <- try(parse(text = subsetString)[[1]], silent = TRUE)
	# 		if (inherits(condition, "try-error"))
	# 			return(errorMessage("expression", paste("Failed to parse expression '", subsetString, "'.")))
	#
	# 		call <- as.call(list(
	# 			as.name("subset.data.frame"),
	# 			data,
	# 			condition
	# 		))
	#
	# 		eval(call, envir = .GlobalEnv)
	# 	})
	#
	# 	output$pending <- renderUI({
	# 		data <- reactiveData()
	# 		if (isErrorMessage(data))
	# 			h4(style = "color: #AA7732;", data$message)
	# 	})
	#
	# 	output$output <- renderDataTable({
	# 		data <- reactiveData()
	# 		if (isErrorMessage(data))
	# 			return(NULL)
	# 		data
	# 	})
	#
	# 	# Listen for 'done'.
	# 	observeEvent(input$done, {
	#
	# 		# Emit a subset call if a dataset has been specified.
	# 		if (nzchar(input$data) && nzchar(input$subset)) {
	# 			code <- paste("subset(", input$data, ", ", input$subset, ")", sep = "")
	# 			rstudioapi::insertText(text = code)
	# 		}
	#
	# 		invisible(stopApp())
	# 	})
	}

	# Use a modal dialog as a viewr.
	viewer <- dialogViewer("Subset", width = 1000, height = 800)
	runGadget(ui, server, viewer = viewer)

}
