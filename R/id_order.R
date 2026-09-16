# Ordering of subject identifiers
#
# Pmetrics identifiers are text whenever a data file labels subjects with
# anything other than plain integers, and sorting text is byte order: "10"
# precedes "2". Any ordering that follows id order - simulated regimens, PTA
# rows - then follows that order instead of the one the data was written in,
# which silently associates results with the wrong subject.
#
# `pm_id_rank()` returns an integer rank per element: numeric ids sort
# numerically, anything else keeps byte order, and equal ids share a rank so
# they stay together. Use it wherever data are ordered or grouped by id.

pm_id_rank <- function(id) {
  id <- as.character(id)
  unique_ids <- unique(id)
  if (length(unique_ids) == 0) {
    return(integer(0))
  }

  numeric_ids <- suppressWarnings(as.numeric(unique_ids))
  ordered <- if (all(!is.na(numeric_ids))) {
    unique_ids[order(numeric_ids)]
  } else {
    sort(unique_ids, na.last = TRUE)
  }

  match(id, ordered)
}
