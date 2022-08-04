#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
#' @param lhs A value or the magrittr placeholder.
#' @param rhs A function call using the magrittr semantics.
#' @return The result of calling `rhs(lhs)`.
NULL

#' Available datasets
#'
#' Named list, with names being concisely named human-readable descriptions,
#' and values being short programming-friendly labels.
#' @export

data_options <- list(
  "Marx's Capital" = "marx_cap",
  "Lenin's collected works (English)" = "lenin_en",
  "Lenin's collected works (Russian)" = "lenin_ru"
)
