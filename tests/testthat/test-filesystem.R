test_that("get_cur_proj() returns a two-component list", {
	skip_if_not(rstudioapi::isAvailable(), message = "RStudio not running")

	res <- get_cur_proj()
	expect_vector(res, ptype = list(), size = 2L)
})
