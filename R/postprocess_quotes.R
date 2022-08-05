#' Data source-specific parsing for display
#' @param counts data frame after counting word mentions
#' @param datasource shortname of data source
#' @export

post_process_quotes <- function(
  counts,
  datasource = c("marx_cap", "lenin_en", "lenin_ru")
){
  datasource <- match.arg(datasource)
  if (nrow(counts) == 0) return(data.frame())
  if (datasource == "marx_cap") {
    counts %>%
      filter(mentions > 0) %>%
      mutate(link = I(html_link(url_marx_cap(chapter)))) %>%
      select(word, chapter, link, text)
  } else if (datasource == "lenin_en"){
    counts %>%
      filter(mentions > 0) %>%
      mutate(link = I(html_link(url_leninature(url)))) %>%
      select(word, year, title, link, text)

  } else {
    counts %>%
      filter(mentions > 0) %>%
      select(word, vol, text) %>%
      tidytext::unnest_tokens(excerpt, text, token = "sentences") %>%
      rowwise() %>%
      filter(grepl(word, excerpt, ignore.case = TRUE))
  }
}

#' Create html formatted url
#' @export
#' @param url url
#' @param display_text text to display, optionally HTML formatted
#' @export

html_link <- function(url, display_text = "link") {
  paste0('<a href="', url, '">', display_text, "</a>")
}

#' url to Marxists.org given chapter of Capital
#' @param chapter chapter number (integer)
#' @export
url_marx_cap <- function(chapter) {
  ch <- ifelse(chapter < 10, paste0("0", chapter), as.character(chapter))
  paste0("https://www.marxists.org/archive/marx/works/1867-c1/ch", ch, ".htm")
}

#' Full url to Marxists.org given leninature url
#' @param url abbreviated url requiring pre-fix
#' @export
url_leninature <- function(url) {
  paste0("https://www.marxists.org/archive/lenin/", url)
}
