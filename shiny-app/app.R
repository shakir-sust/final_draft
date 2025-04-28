# app.R
# Shiny app for Corn‐Trial Explorer
# Assumes all objects (weather, weather_train, weather_test,
# EDA_yield, final_spec, final_fit, preds, metrics_tbl,
# pred_vs_obs_plot, var_imp_plot) are already present in the environment

library(shiny)
library(tidyverse)
library(ggridges)
library(viridis)

# Soil‐Sep and Weather‐Sep predictor lists
soil_vars <- c(
  "mean_soil.ph_Sep",
  "mean_soil.om.pct_Sep",
  "mean_soil.k.ppm_Sep",
  "mean_soil.p.ppm_Sep"
)
weather_vars <- c(
  "mean_srad.wm2_Sep",
  "mean_tmax.c_Sep",
  "mean_tmin.c_Sep",
  "mean_vp.pa_Sep",
  "sum_prcp.mm_Sep"
)

# UI
ui <- fluidPage(
  titlePanel("Corn‐Trial Explorer"),
  sidebarLayout(
    sidebarPanel(
      conditionalPanel(
        "input.tabSelected == 'Soil EDA'",
        selectInput("soil_var", "Select soil Sep predictor:", choices = soil_vars)
      ),
      conditionalPanel(
        "input.tabSelected == 'Weather EDA'",
        selectInput("weather_var", "Select weather Sep predictor:", choices = weather_vars)
      )
    ),
    mainPanel(
      tabsetPanel(
        id = "tabSelected",
        tabPanel(
          "Yield EDA",
          h3("Density plot to compare target variable 'yield' in the training and test set"),
          plotOutput("yieldPlot")
        ),
        tabPanel(
          "Soil EDA",
          h3("Distribution of soil‐related predictors in September"),
          plotOutput("soilPlot")
        ),
        tabPanel(
          "Weather EDA",
          h3("Distribution of weather‐related predictors in September"),
          plotOutput("weatherPlot")
        ),
        tabPanel(
          "Pred vs Observed",
          h3("Predicted vs Observed Plot with R² & RMSE"),
          plotOutput("predObsPlot")
        ),
        tabPanel(
          "Variable Importance",
          h3("Variable Importance Plot"),
          plotOutput("vipPlot")
        )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Yield EDA
  output$yieldPlot <- renderPlot({
    EDA_yield +
      labs(
        title = "Density plot to compare target variable 'yield' in the training and test set",
        x     = "Yield",
        y     = "Density"
      ) +
      theme_minimal()
  })
  
  # Soil EDA
  output$soilPlot <- renderPlot({
    var <- input$soil_var
    dens <- density(weather[[var]], na.rm = TRUE)
    scale_val <- 1 / max(dens$y, na.rm = TRUE)
    
    ggplot(weather, aes(x = .data[[var]], y = var, fill = stat(x))) +
      geom_density_ridges_gradient(
        scale          = scale_val,
        rel_min_height = 0.01
      ) +
      scale_fill_viridis_c(option = "C") +
      labs(x = "Value", y = NULL) +
      theme_ridges() +
      theme(legend.position = "none")
  })
  
  # Weather EDA
  output$weatherPlot <- renderPlot({
    var <- input$weather_var
    dens <- density(weather[[var]], na.rm = TRUE)
    scale_val <- 1 / max(dens$y, na.rm = TRUE)
    
    ggplot(weather, aes(x = .data[[var]], y = var, fill = stat(x))) +
      geom_density_ridges_gradient(
        scale          = scale_val,
        rel_min_height = 0.01
      ) +
      scale_fill_viridis_c(option = "C") +
      labs(x = "Value", y = NULL) +
      theme_ridges() +
      theme(legend.position = "none")
  })
  
  # Predicted vs Observed
  output$predObsPlot <- renderPlot({
    pred_vs_obs_plot + theme_minimal()
  })
  
  # Variable Importance
  output$vipPlot <- renderPlot({
    var_imp_plot + theme_minimal()
  })
}

# Run the app
shinyApp(ui, server)
