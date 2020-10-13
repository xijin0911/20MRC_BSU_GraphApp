# Functions ---------------------------------------------------------------

DEBUG <- getOption("shinydag.debug", FALSE)

debug_input <- function(x, x_name = NULL) {
  if (!isTRUE(DEBUG)) return()
  
  if (is.null(x)) {
    cat(if (!is.null(x_name)) paste0(x_name, ":"), "NULL", "\n")
  } else if (inherits(x, "igraph")) {
    cat(capture.output(print(x)), "", sep = "\n")
  } else if (length(x) == 1 && !is.list(x)) {
    cat(if (!is.null(x_name)) paste0(x_name, ":"), if (length(names(x))) names(x), "-", x, "\n")
  } else if (is.list(x) && length(x) == 0) {
    cat(if (!is.null(x_name)) paste0(x_name, ":"), "list()", "\n")
  } else {
    if (!inherits(x, "data.frame")) x <- tibble::enframe(x)
    cat(if (!is.null(x_name)) paste0(x_name, ":"), knitr::kable(x), "", sep = "\n")
  }
}

debug_line <- function(...) {
  if (!isTRUE(DEBUG)) return()
  cli::cat_line(...)
}

# use y if x is.null
`%||%` <- function(x, y) if (is.null(x)) y else x
# use y if x is not null(ish) (otherwise NULL)
`%??%` <- function(x, y) if (!is.null(x) && x != "") y