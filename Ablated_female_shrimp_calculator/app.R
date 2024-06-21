
library(shiny)

# Define UI for the app
ui <- fluidPage(
  titlePanel(HTML("<h1>Ablated Female Shrimp Calculator</h1><p>
                  This app calculates the estimated number of ablated female 
                  breeder shrimp that would be used to produce an inputted tonnage of shrimp.</p>")
  ),
  sidebarLayout(
    sidebarPanel(
      numericInput("tonnes", "Metric Tonnes of Shrimp Produced:", value = 500, min = 0),
      selectInput("species", "Species", c("P. vannamei",
                                          "P. monodon", "Other penaeid species")),
      sliderInput("mortality_rate", "Pre-slaughter Mortality Rate (%):", 
                  min = 0, max = 100, value = 50, step = 1),
      sliderInput("hatching_rate", "Hatching Rate (%):", 
                  min = 0, max = 100, value = 50, step = 1),
      sliderInput("ablation_rate", "What percentage of female shrimp are ablated?:", 
                  min = 0, max = 100, value = 100, step = 1),
      actionButton("calculate", "Calculate")
    ),
    mainPanel(
      HTML("<h3>Number of Female Breeder Shrimp Needed:</h3>"),
      tags$div(
        class = "output-summary",
        tags$p("Mean Estimate:", span(textOutput("mean_result", container = span),
                                      style = "font-size: 30px; font-weight: bold"),
               style = "font-size: 25px;", "shrimp"),
        tags$p("5th Percentile:", span(textOutput("p5_result", container = span),
                                       style = "font-size: 20px; font-weight: bold"),
                                       style = "font-size: 15px;", "shrimp"),
        tags$p("95th Percentile:", span(textOutput("p95_result", container = span), 
                                        style = "font-size: 20px; font-weight: bold;"),
               style = "font-size: 15px;", "shrimp")
      ),
      tags$footer(
        HTML("<p style='font-size: 12px; color: #666;'>
          <br><br><br>Info: The calculation first converts tonnages to number of individuals,
          then estimates how many females would be needed to produce that number of individuals
          while accounting for pre-slaughter mortality and hatching rate. 100,000 estimates
          based on possible weight and egg number ranges, then the mean, 5th percentile,
          and 95th percentile are reported.<br> <br>
          Egg numbers are estimated from the FAO factsheets for P. vannamei and P. monodon,
          and Table 4.6a in Wickins and Lee (2002). Average shrimp weight is based on
          estimates by fishcount.
        </p>")
    )
  )
))

# Define server logic for the app
server <- function(input, output) {
  
  calculate_breeders <- reactive({
    set.seed(123) # Set seed for reproducibility
    
    tonnes <- input$tonnes
    mortality_rate <- input$mortality_rate / 100
    hatching_rate <- input$hatching_rate / 100
    ablation_rate <- input$ablation_rate / 100
    num_samples <- 100000 # Number of samples for shrimp weight and eggs per female
    
    # Define average shrimp weight ranges for each species
    shrimp_weight_ranges <- list(
      "P. vannamei" = list(mean = 18, sd = (26 - 11) / 4),
      "P. monodon" = list(mean = 37, sd = (60 - 20) / 4),
      "Other penaeid species" = list(mean = 18, sd = (40 - 6) / 4)
    )
    
    # Random sampling of shrimp weights based on selected species
    species_data <- shrimp_weight_ranges[[input$species]]
    average_shrimp_weight <- rnorm(num_samples, mean = species_data$mean, sd = species_data$sd)
    average_shrimp_weight <- average_shrimp_weight[average_shrimp_weight > 0] # Ensure no negative weights
    
    # Convert tonnes to grams (assuming 1 tonne = 1,000,000 grams)
    total_shrimp_grams <- tonnes * 1e6
    
    # Total number of shrimp needed
    total_shrimp_needed <- total_shrimp_grams / average_shrimp_weight
    
    # Adjust for preslaughter mortality
    total_shrimp_needed_adjusted <- total_shrimp_needed / (1 - mortality_rate)
    
    # Adjust for hatching rate
    total_shrimp_needed_adjusted <- total_shrimp_needed_adjusted / hatching_rate
    
    # Define egg ranges for each species
    egg_ranges <- list(
      "P. vannamei" = c(100000, 250000),
      "P. monodon" = c(500000, 750000),
      "Other penaeid species" = c(200000, 1000000)
    )
    
    # Get the appropriate egg range for the selected species
    eggs_per_female_range <- egg_ranges[[input$species]]
    
    # Randomly sample egg numbers from the uniform range for the selected species
    eggs_per_female_samples <- runif(num_samples, min = eggs_per_female_range[1], max = eggs_per_female_range[2])
    
    # Number of female breeders needed
    breeders_needed <- total_shrimp_needed_adjusted / eggs_per_female_samples
    
    breeders_needed <- breeders_needed * ablation_rate
    
    # Calculate mean, 5th percentile, and 95th percentile
    mean_estimate <- mean(breeders_needed)
    p5_estimate <- quantile(breeders_needed, probs = 0.05)
    p95_estimate <- quantile(breeders_needed, probs = 0.95)
    
    list(
      mean_estimate = mean_estimate,
      p5_estimate = p5_estimate,
      p95_estimate = p95_estimate,
      shrimp_weights = average_shrimp_weight,
      num_samples = length(breeders_needed)
    )
  })
  
  output$mean_result <- renderText({
    res <- calculate_breeders()
    ceiling(res$mean_estimate)
  })
  
  output$p5_result <- renderText({
    res <- calculate_breeders()
    ceiling(res$p5_estimate)
  })
  
  output$p95_result <- renderText({
    res <- calculate_breeders()
    ceiling(res$p95_estimate)
  })
}

# Run the app
shinyApp(ui = ui, server = server)
