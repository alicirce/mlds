#' UI
#' @noRd
ui <- fluidPage(

  # Application title
  titlePanel("Word counts in Marxist-Leninist texts"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "datasource",
        "Data source:",
        list(
          "Marx's Capital" = "marx_cap",
          "Lenin's collected works (English)" = "lenin_en",
          "Lenin's collected works (Russian)" = "lenin_ru"
        )
      ),
      checkboxInput('ignore_case', 'Ignore case sensitivity', TRUE),
      checkboxInput('word_boundary', 'Require word boundaries', TRUE),
      textAreaInput(
        "word_input",
        "Enter key words: separate key words by new lines.
        Group similar words on one line separated by commas",
        "surplus value,surplus-value,\nlabour power,labour-power",
        rows = 3
      )
    ),

    # Show a plot of the generated distribution
    mainPanel(tableOutput("tab"))
  )
)

#' Server
#' @noRd
server <- function(input, output) {
  df <- reactive({load_data(input$datasource)})
  words <- reactive({parse_string(input$word_input)})

  output$tab <- renderTable({
    mapper <- group_same_words(words())
    counts <- count_mentions_in_dataframe(
      df(),
      unlist(words()),
      unlist(input$ignore_case),
      input$word_boundary
    )
    if (nrow(counts) > 0) {
      counts %>%
        left_join(mapper, by = "word") %>%
        group_by(keyword) %>%
        summarize(n = as.integer(sum(mentions))) %>%
        arrange(desc(n))
    } else {
      data.frame(
        words = unique(mapper$keyword),
        n = 0L
      )
    }


  })
}

#' Run the application
#' @export
run_app <- function() shiny::shinyApp(ui = ui, server = server)
