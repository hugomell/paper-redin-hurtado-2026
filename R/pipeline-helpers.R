# R/pipeline_helpers.R
clean_names <- function(x, prefix) {
  names(x) <- sub(paste0("^", prefix, "_"), "", names(x))
  x
}

combine_tables <- function(tabs, prefix) {
  tabs <- clean_names(tabs, prefix)
  out  <- dplyr::bind_rows(tabs, .id = "model")
  attr(out, "ci_label")     <- attr(tabs[[1]], "ci_label")
  out
}
