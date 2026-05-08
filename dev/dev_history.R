run_dev_housekeeping <- function() {
	usethis::use_build_ignore("dev")
	attachment::att_amend_desc(extra.suggests = c("pkgdown", "covr"), update.config = TRUE)
	invisible(NULL)
}

run_validation_example_tests <- function(
	files = c(
		"tests/testthat/test-methods-print-plot-summary.R",
		"tests/testthat/test-book-examples.R"
	),
	quiet = TRUE
) {
	devtools::load_all(quiet = quiet)

	results <- purrr::map(files, testthat::test_file)
	invisible(results)
}
