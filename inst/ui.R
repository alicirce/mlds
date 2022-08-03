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
  ),
  hr(),
  HTML("<strong>Data Source</strong>:"),
  HTML(paste0(
    "<li>Marx's <i>Capital</i>: ",
    html_link(
      'https://www.marxists.org/archive/marx/works/1867-c1/',
      "Marx/Engels Internet Archive"
    ),
    " via ",
    html_link(
      'https://alicirce.github.io/marxmywords/',
      "marxmywords"
    ),
    "</li>"
  )),
  HTML(paste0(
    "<li>Lenin's Collected Works (English): ",
    html_link(
      'https://www.marxists.org/archive/lenin/index.htm',
      "Lenin Internet Archive"
    ),
    " via ",
    html_link(
      'https://alicirce.github.io/leninature/',
      "leninature"
    ),
    "</li>"
  )),
  HTML(paste0(
    "<li>Lenin's Collected Works (Russian): ",
    html_link(
      'https://leninism.su/works',
      "leninism.su"
    ),
    " via ",
    html_link(
      'https://github.com/alicirce/leninism',
      "leninism"
    ),
    "</li>"
  )),
  HTML(paste0(
    "<br><strong>Contact</strong>: ",
    html_link("https://twitter.com/alicirce", "@alicirce")
  ))
)
