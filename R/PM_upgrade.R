#' Upgrade legacy Pmetrics objects
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' Upgrades data created by older Pmetrics versions to the current schema.
#' In particular, the former observation-occasion column, `block`, is renamed
#' to `occasion` throughout data frames embedded in the supplied object.
#'
#' `PM_upgrade()` is called automatically by Pmetrics object constructors, so
#' most users only need it when working directly with an older saved data frame
#' or list. Re-save an upgraded object to make the conversion permanent.
#'
#' @param x A data frame, list, or Pmetrics R6 object to upgrade.
#' @param warn Whether to warn when a legacy `block` column is converted.
#'
#' @return An upgraded copy of `x`.
#' @export
#'
#' @examples
#' old_data <- data.frame(id = 1, block = 1)
#' PM_upgrade(old_data)
PM_upgrade <- function(x, warn = TRUE) {
  upgraded <- FALSE

  pm_upgrade_occasion_column <- function(data) {
    if (!is.data.frame(data) || !"block" %in% names(data)) {
      return(data)
    }

    if ("occasion" %in% names(data)) {
      columns_match <- isTRUE(all.equal(
        data$block,
        data$occasion,
        check.attributes = FALSE
      ))
      if (!columns_match) {
        cli::cli_abort(c(
          "x" = "Cannot upgrade data containing conflicting {.field block} and {.field occasion} columns.",
          "i" = "Remove one column or make their values identical before calling {.fn PM_upgrade}."
        ))
      }
      data$block <- NULL
    } else {
      names(data)[names(data) == "block"] <- "occasion"
    }

    upgraded <<- TRUE
    data
  }

  upgrade_value <- function(value) {
    if (is.data.frame(value)) {
      return(pm_upgrade_occasion_column(value))
    }

    # Rebuild PM_op objects so the legacy public `$block` field is replaced by
    # the current `$occasion` field as well as upgrading the underlying data.
    if (inherits(value, "PM_op")) {
      rebuild <- function() PM_op$new(value, path = file.path(tempdir(), "PM_upgrade"))
      return(if (isTRUE(warn)) rebuild() else suppressWarnings(rebuild()))
    }

    if (inherits(value, "PM_data")) {
      rebuild <- function() PM_data$new(value$data, quiet = TRUE)
      rebuilt <- if (isTRUE(warn)) rebuild() else suppressWarnings(rebuild())
      rebuilt$pop <- upgrade_value(value$pop)
      rebuilt$post <- upgrade_value(value$post)
      return(rebuilt)
    }

    if (is.list(value) && !inherits(value, "R6")) {
      attributes_value <- attributes(value)
      value <- lapply(value, upgrade_value)
      attributes(value) <- attributes_value
      return(value)
    }

    if (inherits(value, "R6")) {
      value <- value$clone(deep = TRUE)
      upgrade_fields <- intersect(
        c("data", "standard_data", "pop", "post", "op", "cov", "cycle", "final", "valid"),
        ls(envir = value, all.names = TRUE)
      )
      for (field in upgrade_fields) {
        current <- value[[field]]
        if (!is.null(current) && !is.function(current)) {
          value[[field]] <- upgrade_value(current)
        }
      }
      return(value)
    }

    value
  }

  result <- upgrade_value(x)
  if (upgraded && isTRUE(warn)) {
    lifecycle::deprecate_warn(
      "3.3.0",
      what = I("The `block` column in Pmetrics data"),
      with = I("the `occasion` column"),
      details = "The object was upgraded automatically in memory. Re-save it to make this change permanent."
    )
  }

  result
}
