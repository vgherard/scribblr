test_that("Throws error if rstudio is not available", {
	skip_if(rstudioapi::isAvailable(), message = "RStudio is running")
	expect_error(scribble(), class = "rs_not_available_error")
})
