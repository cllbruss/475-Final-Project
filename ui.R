library(shiny)
library(bslib)
library(shinyWidgets)
library(shinyjs)

ui <- fluidPage(
  theme = bs_theme(version = 4, bootswatch = "minty"),
  sidebarLayout(
   sidebarPanel(
    selectInput(inputId = "selected_event",
              label = "Select Event:",
              choices = unique(interpolated$Event)
  ),
 
    selectInput(inputId = "selected_country",
              label = "Select Country (Only for Champions Table):",
              choices = unique(interpolated$Country)
  ),
  
    actionButton(
              inputId = "instruction_button",
              label = "App Instructions"
  )
 ),
  mainPanel(
    tabsetPanel(
      tabPanel("Full Time Series", plotOutput("main_plot"), "Displayed above is the full time series plot for each of the events included in the Boston Marathon."),
      tabPanel("Seasonality", plotOutput("season"), "Unfortunately, there is no seasonality in the Boston Marathon data as the event only happens once per year, but displayed above 
               is the time series for the selected event compared against the average completion time for the selected event displayed as the blue bar."),
      tabPanel("Correlation", plotOutput("correlation"), "Displayed above is the correlation between the historical completion times for the selected event, where for most of the events, looking one year
               back will tell us the most information for the current and future completion times."),
      tabPanel("Decomposition", plotOutput("decomp"), "Displayed above is the decompostion graph for the selected event that shows the time series graph, the trend of the data, 
               and the remainder of the data. The gray bar displayed on the left side of each graph tell us how the variation in the data changes. If the grey bar is large for the respected decompostion graph, that means the the variation for that graph is smaller 
               compared to the variation in the data. The trend graph shows the overall trend of the data, and the remainder graph shows what is left over when the seasonal and trend-cycle components have been subtracted from the data, but in our case we have no seasonality component."),
      tabPanel("Mean", plotOutput("Mean"), "Mean model with predictions for next 5 years."),
      tabPanel("Drift", plotOutput("Drift"), "Drift model with predictions for next 5 years."),
      tabPanel("Naive", plotOutput("Naive"), "Naive model with predictions for next 5 years."),
      tabPanel("Holts Method", plotOutput("holt"), "Holts method with graph and predictions for the next 5 years. Also included is the Holts damped method."),
      tabPanel("ARIMA", plotOutput("arima"), "Auto ARIMA model with predictions for next 5 years."),
      tabPanel("Champions Table", dataTableOutput("stats"), "Displayed above is the champions table that displays the specific winner and their completion times of the selected event and selected country.
               This can be used along with the time series graphs to see specific times that are displayed and which person and country are responsible for the times.")
    )
  )
 )
)

