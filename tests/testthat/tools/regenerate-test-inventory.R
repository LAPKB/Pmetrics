extract_testthat_calls <- function(file) {
  lines <- readLines(file, warn = FALSE)

  call_lines <- grep(
    "\\b(?:test_that|testthat::test_that)\\s*\\(",
    lines,
    perl = TRUE
  )
  call_lines <- call_lines[!grepl("^\\s*#", lines[call_lines])]

  if (length(call_lines) == 0) {
    return(data.frame(
      file = character(),
      line = integer(),
      test = character(),
      stringsAsFactors = FALSE
    ))
  }

  extract_description <- function(line_index) {
    end_index <- min(line_index + 20L, length(lines))
    snippet <- paste(lines[line_index:end_index], collapse = " ")

    match <- regexec(
      "(?s)\\b(?:test_that|testthat::test_that)\\s*\\((.*?),\\s*\\{",
      snippet,
      perl = TRUE
    )
    capture <- regmatches(snippet, match)[[1]]

    if (length(capture) >= 2) {
      first_arg <- trimws(capture[2])

      if (grepl('^\".*\"$', first_arg)) {
        return(sub('^\"(.*)\"$', '\\1', first_arg))
      }

      return(first_arg)
    }

    NA_character_
  }

  descriptions <- vapply(call_lines, extract_description, character(1))

  data.frame(
    file = rep(file, length(call_lines)),
    line = as.integer(call_lines),
    test = descriptions,
    stringsAsFactors = FALSE
  )
}

generate_test_inventory <- function(
    pkg_root = ".",
    tests_path = file.path("tests", "testthat"),
    output_path = file.path("tests", "testthat", "test-inventory.csv"),
    write_csv = TRUE
) {
  pkg_root <- normalizePath(pkg_root, winslash = "/", mustWork = TRUE)
  tests_dir <- file.path(pkg_root, tests_path)

  test_files <- list.files(
    path = tests_dir,
    pattern = "^test.*\\.[Rr]$",
    full.names = TRUE,
    recursive = TRUE
  )

  rows <- lapply(test_files, function(path) {
    extract_testthat_calls(path)
  })

  inventory <- do.call(rbind, rows)

  if (nrow(inventory) == 0) {
    inventory <- data.frame(
      file = character(),
      line = integer(),
      test = character(),
      stringsAsFactors = FALSE
    )
  }

  inventory$file <- sub(
    paste0("^", gsub("([.|()\\^{}+$*?]|\\[|\\])", "\\\\\\1", pkg_root), "/"),
    "",
    inventory$file
  )

  inventory <- inventory[order(inventory$file, inventory$line), , drop = FALSE]
  rownames(inventory) <- NULL

  if (isTRUE(write_csv)) {
    output_file <- file.path(pkg_root, output_path)
    dir.create(dirname(output_file), recursive = TRUE, showWarnings = FALSE)
    utils::write.csv(inventory, output_file, row.names = FALSE)
  }

  inventory
}

if (sys.nframe() == 0) {
  invisible(generate_test_inventory())
}
