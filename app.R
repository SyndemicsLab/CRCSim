library(shiny)
library(shinythemes)
library(data.table)
library(ggplot2)
#devtools::install("SyndemicsLab/Syndemics")
#devtools::install(".")
library(CRCSim)
library(Syndemics)

ui <- fluidPage(
  theme = shinytheme("flatly"),

  # Application title
  titlePanel("Capture Re-Capture Interactive Simulation"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      actionButton("sim", "Estimate"),
      numericInput("n", "Total Population Count:", min = 1, max = NA, value = 3e5),
      numericInput("sup", "Suppress numbers under:", min = 0, max = NA, value = 10),
      textInput("p_capture", "Capture Probabilities (comma-separated):"),
      textInput("p_stratif", "Stratification Probabilities (comma-separated):")
    ),

    mainPanel(
      tabsetPanel(
        tabPanel("Graphical Results", plotOutput("plot")),
        tabPanel("Tabled Results", tableOutput("table"))
      )
    )
  )
)

server <- function(input, output) {

  data <- eventReactive(input$sim, {
    n <- input$n
    suppression <- input$sup
    p_captures <- c(as.numeric(unlist(strsplit(gsub("\\s", "", input$p_capture), ","))))
    p_stratif <- c(as.numeric(unlist(strsplit(gsub("\\s", "", input$p_stratif), ","))))
    n_captures <- length(p_captures)
    n_stratif <- length(p_stratif)

    data <- CRCSim::analyze(n, n_captures, n_stratif, p_captures, p_stratif, suppress = suppression)
  })

  output$table <- renderTable({
    data()
    })

  output$plot <- renderPlot({
    plotData <- data()
    p_stratif <- c(as.numeric(unlist(strsplit(gsub("\\s", "", input$p_stratif), ","))))
    plotData <- plotData[, group := factor(group, labels = p_stratif)]

    subtitleStr <- paste0("Capture Probabilities: ", input$p_capture, "\n",
                          "Stratification Probabilities: ", input$p_stratif)

    ggplot(plotData, aes(y = estimate, x = group, fill = group)) +
      geom_col() +
      labs(x = "Stratification Probability", y = "Estimate\n X marks Ground Truth",
           title = "CRC: Estimated vs Ground Truth",
           subtitle = subtitleStr) +
      guides(fill = "none") +
      geom_point(aes(y = ground), shape = 4, size = 2) +
      geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci)) +
      theme_bw()
  })
}

shinyApp(ui = ui, server = server)

