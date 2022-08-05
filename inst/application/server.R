library(ggplot2)
library(dplyr)
library(ggtextcounts)
library(mlds)

#' Server
#' @noRd
server <- function(input, output) {
  datasource <- reactive({input$datasource})
  df <- reactive({load_data(datasource())})
  words <- reactive({
    req(input$word_input)
    parse_string(input$word_input)
  })
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
        keyword = unique(mapper$keyword),
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
      counts_to_plot <- counts()
      if (max(counts_to_plot$n, na.rm = TRUE) > 0) {
        counts_to_plot <- counts_to_plot %>%
          mutate(keyword = reorder(keyword, n))
      }
      counts_to_plot %>%
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


