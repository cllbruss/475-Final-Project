library(tsibble)
library(fpp3)
library(dplyr)
library(DT)
library(seasonal)

server <- function(input, output) {
  
  observe({
    if(
      input$instruction_button
    )
    showModal(modalDialog(
      title = "App Instructions",
      "This app looks at statistics from the Boston Marathon.
       The Full Time Series tab shows the full time series plot for the Boston Marathon. For the Seasonality tab, the Correlation tab, and the Decomposition tab, 
       the user can change what event will be reflected in each of the three tabs on the left side of the page under Select Event. Lastly, the Champions Table tab 
       can show specific statisitcs based on what country and event the user has selected in the left side of the page."))
  })
  
  
  output$stats <- renderDataTable({
    
    marathon_stats <- interpolated %>% 
      filter(Event %in% input$selected_event) %>% 
      filter(Country %in% input$selected_country)
    
    subset_stats <- marathon_stats[,c(2,3,5)]
    
    datatable(subset_stats, options = list(lengthMenu = c(5, 10), pageLength = 5, scrollX = TRUE), escape = FALSE, selection = 'none')
    
  })
  
  output$main_plot <- renderPlot({
    
  ts_df <- as_tsibble(
     interpolated,
     index = "Year",
     key = "Event"
    )
    
     autoplot(ts_df, Time) + 
       labs(title = "Boston Marathon Completion Time by Event")
    
  })
  
  plot_df <- reactive({
    
    filtered_marathon <- interpolated %>% 
      filter(Event %in% input$selected_event)
    
    my_df <- as_tsibble(
      filtered_marathon,
      index = "Year",
      key = "Event"
    ) 
    
  })
  
  output$season <- renderPlot({
    
    plot_df() %>% 
      gg_subseries() +
      labs(title = "Boston Marathon Completion Times")
      
  })
  
  output$correlation <- renderPlot({
    
    
    plot_df() %>% 
      ACF(Time) %>% 
      autoplot() +
      labs(title = "Boston Marathon Completion Times")
    
  
  })
  
  output$decomp <- renderPlot({
    
    plot_df() %>%
      model(
        STL(Time ~ trend(window = 7) +
              season(window = "periodic"),
            robust = TRUE)) %>%
      components() %>%
      autoplot()
  
  })
  
   output$Mean <- renderPlot({
    
     plot_df() %>% 
      model(Mean = MEAN(Time)) %>% 
      forecast(h = 5) %>% 
      autoplot(plot_df())
      
  })
   
   output$Drift <- renderPlot({
     
     plot_df() %>% 
       model(Drift = RW(Time ~ drift())) %>% 
       forecast(h = 5) %>% 
       autoplot(plot_df())
     
   })
   
   output$Naive <- renderPlot({
     
     plot_df() %>% 
       model(`Naïve` = NAIVE(Time)) %>% 
       forecast(h = 5) %>% 
       autoplot(plot_df())
     
   })
   
   output$holt <- renderPlot({
     
     plot_df() %>%
       model(
         `Holt's method` = ETS(Time ~ error("A") + trend("A") + season("N")),
         `Damped Holt's method` = ETS(Time ~ error("A") + trend("Ad", phi = 0.9) + season("N"))) %>% 
       forecast(h = 5) %>% 
       autoplot(plot_df())
   })
   
   output$arima <- renderPlot({
     
     plot_df() %>% 
       model(ARIMA(Time)) %>% 
       forecast(h = 5) %>% 
       autoplot(plot_df())
     
   })
  
  
}
