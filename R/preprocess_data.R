#' Load preprocessed data for word counting
#'
#' Some data sources have metadata etc about the text (table of contents, etc).
#' This function removes unnecessary rows.
#'
#' @param datasource string indicating which data source to use

load_data <- function(datasource = c("marx_cap", "lenin_en", "lenin_ru")) {
  datasource <- match.arg(datasource)
  if (datasource == "marx_cap") {
    keep_sections <- c("body", "footnotes")
    marxmywords::capital_vol1 %>%
      filter(section %in% keep_sections)
  } else if (datasource == "lenin_en"){
    leninature::lenin
  } else {
    keep_sections <- "main"
    leninism::leninru %>%
      filter(section %in% keep_sections)
  }
}

#' Available datasets
#'
#' Named list, with names being concisely named human-readable descriptions,
#' and values being short programming-friendly labels.

data_options <- list(
  "Marx's Capital" = "marx_cap",
  "Lenin's collected works (English)" = "lenin_en",
  "Lenin's collected works (Russian)" = "lenin_ru"
)
