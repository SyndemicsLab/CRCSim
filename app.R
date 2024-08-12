library(shiny)
library(shinythemes)
library(data.table)
library(ggplot2)
#devtools::install_github("SyndemicsLab/Syndemics", subdir = "Syndemics")
devtools::install(".")
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
      textInput("p_stratif", "Stratification Probabilities (comma-separated):"),
      p("Notes:"),
      p("Please be patient! This app simulates a population for each model which can be computationally expensive"),
      p("Confidence intervals are derived from a confint() estimate on model intercept
        when non-bootstrapped (as is the case in this applet)")
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

    config <- list(
      f0.05 = list(direction = "forward", threshold = 0.05),
      f0.1 = list(direction = "forward", threshold = 0.1),
      b0.05 = list(direction = "backward", threshold = 0.05),
      b0.1 = list(direction = "backward", threshold = 0.1),
      fb0.05 = list(direction = "both", threshold = 0.05),
      fb0.1 = list(direction = "both", threshold = 0.1)
    )

    pois <- lapply(config, function(x){
      CRCSim::analyze(n, n_captures, n_stratif, p_captures, p_stratif, suppress = suppression,
                      method = "poisson", formula.selection = "stepwise", opts.stepwise = c(x, verbose = FALSE))
    })
    pois_data <- rbindlist(pois, idcol = c("Model", names(pois)))
    pois_data <- pois_data[, Model := paste0(gsub("b", "Backward-", gsub("f", "Forward-", gsub("fb", "Both-", Model))))]

    nb <- lapply(config, function(x){
      CRCSim::analyze(n, n_captures, n_stratif, p_captures, p_stratif, suppress = suppression,
                      method = "negbin", formula.selection = "stepwise", opts.stepwise = c(x, verbose = FALSE))
    })
    nb_data <- rbindlist(nb, idcol = c("Model", names(nb)))
    nb_data <- nb_data[, Model := paste0(gsub("b", "Backward-", gsub("f", "Forward-", gsub("fb", "Both-", Model))))]

    data <- rbind(nb_data[, Method := "NB"], pois_data[, Method := "Poisson"])[, AbsDifference := abs(GroundTruth - Estimate)
                                                                               ][, tmp := mean(AbsDifference), by = c("Model", "Method")]
    setorderv(data, "tmp", order = 1)
    data <- data[, tmp := NULL]
    return(data)
  })

  output$table <- renderTable({
    data()
    })

  output$plot <- renderPlot({
    plotData <- data()
    p_stratif <- c(as.numeric(unlist(strsplit(gsub("\\s", "", input$p_stratif), ","))))
    plotData <- plotData[, Group := factor(Group, labels = p_stratif)]

    subtitleStr <- paste0("Capture Probabilities: ", input$p_capture, "\n",
                          "Stratification Probabilities: ", input$p_stratif)

    ggplot(plotData, aes(y = Estimate, x = Group, fill = Group)) +
      labs(x = "Stratification Probability", y = "Estimate (Red)\nGround Truth (Blue)",
           title = "CRC: Estimated vs Ground Truth",
           subtitle = subtitleStr) +
      guides(fill = "none") +
      geom_point(aes(y = GroundTruth), color = "blue") +
      geom_errorbar(aes(ymin = LowerCI, ymax = UpperCI)) +
      theme_bw() +
      facet_grid(Method~Model) +
      geom_point(col = "red")
  })
}

shinyApp(ui = ui, server = server)

