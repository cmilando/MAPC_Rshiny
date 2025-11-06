# app.R — Enhanced version with uncertainty bounds
library(shiny)
library(readxl)
library(tidyverse)
library(ggpubr)
library(shinythemes)

# ---- Load data ----
by_city_df <- read_xlsx("by_city.xlsx")
by_demo_df <- read_xlsx("county_by_demographic.xlsx")

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

demographic_variable_map <- list(
  "Age-group" = list(
    levels = c( '18-39', '40-64', '65+')
  ),
  "Gender" = list(
    levels = c('Female', 'Male')
  )
)

# Basic checks
needed <- unique(unlist(lapply(metric_map, unname)))
stopifnot(all(c("region", "POP2020") %in% names(by_city_df)))
by_city_df <- by_city_df[complete.cases(by_city_df[, c("region", "POP2020", needed)]), ]


# ---- UI ----
ui <- fluidPage(
  # theme
  theme = shinytheme("flatly"),
  
  # max width
  style = "max-width: 900px;",
  
  tabsetPanel(
    ## Notes
    tabPanel(title = "Notes",
             titlePanel("Heat-attributable ED visits in MA"),
             helpText(HTML("
             <p>
Chad Milando, PhD (cmilando@bu.edu), Gregory Wellenius, ScD (wellenius@bu.edu)
The Center for Climate and Health, Boston University School of Public Health
</p>
<br>
             <p>Heat is a recognized public health hazard. More people die of heat than of any other weather-related disaster. We used Massachusetts data to evaluate the health impacts of heat in the Commonwealth. </p>
              <ul>
                <li><b>Time period:</b> Summers (May – Sept) of 2010 through 2023</li>
                <li><b>Population:</b> Massachusetts (MA) residents of all ages</li>
                <li><b>Health outcome:</b> All-cause Emergency Department (ED) visits in MA hospitals</li>
                <li><b>Exposure:</b> Daily maximum temperature for each MA Zipcode</li>
                Health impacts are calculated for summertime days where the daily maximum temperature > 75F.
              </ul>
              "))
    ),
    # BY TOWN
    tabPanel(title = "Town",
             titlePanel("Heat-attributable ED visits by MA Town"),
        sidebarLayout(
          sidebarPanel(
            selectInput("city_metric", "Metric", choices = names(metric_map)),
            sliderInput("city_pop_range", "Town population range (2020)",
                        min = min(by_city_df$POP2020), max = max(by_city_df$POP2020),
                        value = range(by_city_df$POP2020), step = 1, sep = ","),
            numericInput("city_top_n", "Show top N towns", 
                         value = 15, min = 5, max = 100)
          ),
          mainPanel(
            plotOutput("by_city_barplot", height = "450px"),
            tags$hr(),
            h4("by_city_filtered data"),
            tableOutput("by_city_table")
          )
        )
    ),
    # BY DEMOGRAPHICS
    tabPanel(title = "Demographics",
             titlePanel("Heat-attributable ED visits by Demographic variables"),
             
             sidebarLayout(
               sidebarPanel(
                 selectInput("demo_metric", "Metric", 
                             choices = names(metric_map)),
                 selectInput("demo_variable", "Variable", 
                             choices = names(demographic_variable_map)),
                 uiOutput("demo_level_ui")

               ),
               mainPanel(
                 plotOutput("by_demo_barplot", height = "450px"),
                 tags$hr(),
                 h4("by_demo_filtered data"),
                 tableOutput("by_demo_table")
               )
             )
             
             
             ),
    # BY GEOSPATIAL 
    tabPanel(title = "GeoSpatial",
             titlePanel("Heat-attributable ED visits by Geospatial variables")),
  )
)

# ---- SERVER ----
server <- function(input, output, session) {
  
  # ========================================================================
  
  by_city_filtered <- reactive({
    rng <- input$city_pop_range
    by_city_df[by_city_df$POP2020 >= rng[1] & by_city_df$POP2020 <= rng[2], ]
  })
  
  output$by_city_barplot <- renderPlot({
    m <- metric_map[[input$city_metric]]
    d <- by_city_filtered()[order(by_city_filtered()[[m$est]], decreasing = TRUE), ]
    d <- head(d, min(nrow(d), input$city_top_n))
    
    vals <- d[[m$est]]
    lb   <- d[[m$lb]]
    ub   <- d[[m$ub]]
    names(vals) <- d$region
    xarea <- d$region
    
    plot_df <- data.frame(vals, lb, ub, xarea)
    # print(head(plot_df))
    
    plot_df %>%
      ggplot(aes(x = reorder(xarea, -vals), y = vals)) +
      geom_hline(yintercept = 0) +
      geom_col(fill = "lightblue", width = 0.75, color = 'black', alpha = 0.75) +
      geom_errorbar(aes(ymin = lb, ymax = ub), width = 0.2) +
      labs(
        x = NULL,
        y = input$city_metric ,
        title = paste("Top", nrow(d), "towns by", input$city_metric)
      ) +
      theme_classic2(base_size = 12) +
      theme(
        axis.line.x = element_blank(),
        axis.text.y = element_text(size = 10),
        plot.title = element_text(hjust = 0.5),
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)
        # plot.margin = margin(t=5,r=150,b =5,l=5)
      )
  })
  
  output$by_city_table <- renderTable({
    m <- metric_map[[input$city_metric]]
    d <- by_city_filtered()
    out <- d[, c("region", "POP2020", m$est, m$lb, m$ub)]
    names(out) <- c("Town", "Population (2020)", "Estimate",
                    "Lower bound", "Upper bound")
    head(out[order(out$Estimate, decreasing = TRUE), ], 100)
  })
  
  
  # ========================================================================
  
  # Render the second selectInput based on the chosen category
  output$demo_level_ui <- renderUI({

    opts <- demographic_variable_map[[ input$demo_variable  ]]

    selectInput(
      inputId = "demo_level",
      label   = "Level:",
      choices = opts,
    )
  })
  
  by_demo_filtered <- reactive({
    print('here')
    print(input$demo_level)
    print('--')
    if(!is.null(input$demo_level)) {
      by_demo_df[by_demo_df$level == input$demo_level, ]
    }
  })
  
  output$by_demo_barplot <- renderPlot({
    m <- metric_map[[input$demo_metric]]
    d <- by_demo_filtered()[order(by_demo_filtered()[[m$est]], decreasing = TRUE), ]

    vals <- d[[m$est]] 
    lb   <- d[[m$lb]]
    ub   <- d[[m$ub]]
    xarea <- d$AREA
    xlevel <- d$level
    names(vals) <- paste0(d$AREA, "_", d$level)
    
    plot_df <- data.frame(vals, lb, ub, xarea, xlevel)
    # print(head(plot_df))
    
    plot_df %>%
      ggplot(aes(x = reorder(xarea, -vals), y = vals)) +
      geom_hline(yintercept = 0) +
      geom_col(fill = "lightblue", width = 0.75, color = 'black', alpha = 0.75) +
      geom_errorbar(aes(ymin = lb, ymax = ub), width = 0.2) +
      labs(
        x = NULL,
        y = input$demo_metric,
        title = paste("Top", nrow(d), "counties by", input$demo_metric)
      ) +
      theme_classic2(base_size = 12) +
      theme(
        axis.line.x = element_blank(),
        axis.text.y = element_text(size = 10),
        plot.title = element_text(hjust = 0.5),
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)
        # plot.margin = margin(t=5,r=150,b =5,l=5)
      )
  })
  
  output$by_demo_table <- renderTable({
    m <- metric_map[[input$demo_metric]]
    d <- by_demo_filtered()
    out <- d[, c("AREA",'level', "sum_population", m$est, m$lb, m$ub)]
    names(out) <- c("County",'Level', "Population (2020)", "Estimate",
                    "Lower bound", "Upper bound")
    head(out[order(out$Estimate, decreasing = TRUE), ], 100)
  })
  
  
  
}

shinyApp(ui, server)
