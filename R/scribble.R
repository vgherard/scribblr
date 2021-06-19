#'
#' @export
scribble <- function() {
	paths <- get_scribble_paths()
	dir <- paths[["dir"]]
	filepath <- paths[["filepath"]]

	if (!file.exists(filepath)) {
		file.create(filepath)
	}

	txt <- paste0("", readLines(filepath), collapse = "\n")

	title <- "Notes for RStudio"
	if (paths[["is_r_project"]])
		title <- paste(title, "project at", dir)

	js <- '
		$(document).on("shiny:connected", function(){
			Shiny.setInputValue("loaded", 1);
			Shiny.addCustomMessageHandler("focus",
				function(NULL) {
					document.getElementById("noteIO").focus();
					})
		});
	'

	#------------------------------------------------------------ User Interface
	ui <- miniPage(
		tags$head(tags$script(HTML(js)))

		,gadgetTitleBar(
			title,
			left = NULL,
			right = miniTitleBarCancelButton(inputId = "close", label = "Close (Esc)")
			)
		,miniContentPanel(
			textAreaInput(
				inputId = "noteIO",
				label = NULL,
				value = txt,
				width = "100%",
				height = "250px"
				)
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
