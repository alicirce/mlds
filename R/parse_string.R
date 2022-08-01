#' Parse comma-separated string input into keyword vector
#'
#' @param word_input string of comma separated key words

parse_string <- function(word_input) {
  if(is.null(word_input) || word_input == "") return(NULL)
  lines <- strsplit(word_input, "\n")[[1]]
  lapply(
    strsplit(lines, ","),
    trimws
  )
}

#' returns a data frame of word: keyword pairs
#'
#' @param list_of_words list, with each element being a vector of words.
#'   see example output of `parse_string`
group_same_words <- function(list_of_words) {
  mapper <- list_of_words
  names(mapper) <- lapply(list_of_words, `[`, 1)
  mapper <- stack(mapper)
  names(mapper) <- c("word", "keyword")
  mapper
}
