test_that("assert_is_string works correctly", {
	expect_error(assert_is_string("string"), NA)
	expect_error(assert_is_string(NULL, can_be_null = TRUE), NA)

	expect_error(assert_is_string(1))
	expect_error(assert_is_string(NULL, can_be_null = FALSE))
	expect_error(assert_is_string(letters))
})

test_that("assert_is_note_name works correctly", {
	expect_error(assert_is_note_name("name"), NA)
	expect_error(assert_is_note_name(NULL), NA)
	expect_error(assert_is_note_name("1st_name.2nd_name"), NA)

	expect_error(assert_is_note_name(1))
	expect_error(assert_is_note_name("x/y"))
})
