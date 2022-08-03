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
        data_options
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
    mainPanel(
      tabsetPanel(
        type = "pills",
        tabPanel(
          "Counts",
          fluidRow(
            column(3, tableOutput("tab")),
            column(7, plotOutput("plot"))
          )
        ),
        tabPanel(
          "Quotes",
          DT::DTOutput("quotes")
        )
      )
    )
  )
)

#' Server
#' @noRd
server <- function(input, output) {
  datasource <- reactive({input$datasource})
  df <- reactive({load_data(datasource())})
  words <- reactive({parse_string(input$word_input)})
  raw_counts <- reactive({
    count_mentions_in_dataframe(
      df(),
      unlist(words()),
      unlist(input$ignore_case),
      input$word_boundary
    )
  })
  counts <- reactive({
    mapper <- group_same_words(words())
    if (nrow(raw_counts()) > 0) {
      raw_counts() %>%
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

  output$tab <- renderTable({
    counts()
  })
  observe({
    output$plot <- renderPlot({
      pretty_name <- names(data_options[data_options == datasource()])[1]
      counts() %>%
        mutate(keyword = reorder(keyword, n)) %>%
        ggplot() +
        aes(y = keyword, x = n, color = keyword) +
        geom_h_lollipop() +
        theme_mlds() +
        theme(
          axis.title = element_blank(),
          axis.ticks.y = element_blank(),
          legend.position = "none"
        ) +
        labs(
          y = "number of mentions",
          title = paste0("Word frequencies in\n", pretty_name)
        ) +
        scale_color_manual(
          values = c(rep("black", length(words()) - 1), "red")
        )
    },
    width = "auto",
    height = 60 + as.numeric(length(words())) * 20
    )
  })
  output$quotes <- DT::renderDT({
    post_process_quotes(raw_counts(), datasource())
  },
  escape = FALSE
  )
}

#' Run the application
#' @export
run_app <- function() shiny::shinyApp(ui = ui, server = server)
