# app.R — Enhanced version with uncertainty bounds
library(shiny)
library(readxl)

# ---- Load data ----
DATA_PATH <- "by_city.xlsx"
df <- read_xlsx(DATA_PATH)

# Map for metrics and bounds
metric_map <- list(
  "Rate (ED visits per 100,000 population)" = list(
    est = "mean_annual_attr_ED_rate_est",
    lb  = "mean_annual_attr_ED_rate_lb",
    ub  = "mean_annual_attr_ED_rate_ub"
  ),
  "Number of ED visits (annual mean)" = list(
    est = "mean_annual_attr_ED_visit_est",
    lb  = "mean_annual_attr_ED_visit_lb",
    ub  = "mean_annual_attr_ED_visit_ub"
  )
)

# Basic checks
needed <- unique(unlist(lapply(metric_map, unname)))
stopifnot(all(c("region", "POP2020") %in% names(df)))
df <- df[complete.cases(df[, c("region", "POP2020", needed)]), ]

# ---- UI ----
ui <- fluidPage(
  tabsetPanel(
    # BY TOWN
    tabPanel(title = "Town",
             titlePanel("Heat-attributable ED visits by MA Town"),
        sidebarLayout(
          sidebarPanel(
            selectInput("metric", "Metric", choices = names(metric_map)),
            sliderInput("pop_range", "Town population range (2020)",
                        min = min(df$POP2020), max = max(df$POP2020),
                        value = range(df$POP2020), step = 1, sep = ","),
            numericInput("top_n", "Show top N towns", 
                         value = 15, min = 5, max = 100)
          ),
          mainPanel(
            plotOutput("barplot", height = "450px"),
            tags$hr(),
            h4("Filtered data"),
            tableOutput("table")
          )
        )
    ),
    # BY DEMOGRAPHICS
    # Age-Group, 
    tabPanel(title = "Demographics",
             titlePanel("Heat-attributable ED visits by Demographic variables")),
    # BY GEOSPATIAL 
    tabPanel(title = "GeoSpatial",
             titlePanel("Heat-attributable ED visits by Geospatial variables")),
  )
)

# ---- SERVER ----
server <- function(input, output, session) {
  
  filtered <- reactive({
    rng <- input$pop_range
    df[df$POP2020 >= rng[1] & df$POP2020 <= rng[2], ]
  })
  
  output$barplot <- renderPlot({
    m <- metric_map[[input$metric]]
    d <- filtered()[order(filtered()[[m$est]], decreasing = TRUE), ]
    d <- head(d, min(nrow(d), input$top_n))
    
    vals <- d[[m$est]]
    lb   <- d[[m$lb]]
    ub   <- d[[m$ub]]
    names(vals) <- d$region
    
    op <- par(mar = c(8, 4, 3, 1) + 0.1)
    on.exit(par(op), add = TRUE)
    
    bp <- barplot(vals,
                  las = 2,
                  ylim = c(0, max(ub, na.rm = TRUE) * 1.05),
                  ylab = input$metric,
                  main = paste("Top", nrow(d), "towns by", input$metric),
                  cex.names = 0.8)
    
    # Add error bars
    arrows(x0 = bp, y0 = lb, x1 = bp, y1 = ub, angle = 90, code = 3, length = 0.05)
  })
  
  output$table <- renderTable({
    m <- metric_map[[input$metric]]
    d <- filtered()
    out <- d[, c("region", "POP2020", m$est, m$lb, m$ub)]
    names(out) <- c("Town", "Population (2020)", "Estimate",
                    "Lower bound", "Upper bound")
    head(out[order(out$Estimate, decreasing = TRUE), ], 100)
  })
}

shinyApp(ui, server)
