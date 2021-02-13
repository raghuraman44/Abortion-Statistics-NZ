# Author: Raghuraman Srinivasan
# Date: 13 February 2021

shinyServer(function(input, output, session) {
    
# ------------------------------------------------------------------------------------------------------------------------------------
    
    # Data Summary and Raw Data
    
# ------------------------------------------------------------------------------------------------------------------------------------
    
    output$Summary1 <- renderPrint({
        dfSummary(AbortionCR)
    })
    
    output$Summary2 <- renderPrint({
        dfSummary(AbortionCRAge)
    })
    
    output$Raw_data1 <- renderDataTable({
        datatable(data = as.data.frame(AbortionCR[,1:3]),
                  options = list(
                      initComplete = JS(
                          "function(settings, json) {",
                          "$(this.api().table().header()).css({'color': '#fff'});",
                          "}"))) # jquery to change the color of table header to match the theme
    })

    output$Raw_data2 <- renderDataTable({
        datatable(data = as.data.frame(AbortionCRAge[,1:4]),
                  options = list(
                      initComplete = JS(
                          "function(settings, json) {",
                          "$(this.api().table().header()).css({'color': '#fff'});",
                          "}"))) # jquery to change the color of table header to match the theme
    })
    
# ------------------------------------------------------------------------------------------------------------------------------------
    
    # Home 
    # Summary Statistics and Ridgeline Plot to Understand the Key Values

# ------------------------------------------------------------------------------------------------------------------------------------
    
    # Calculations: 8 Value boxes with icons are created to show a quick status on the statistical parameters of Abortion Rate 
    # and Number of Abortions 
    
    # Statistical Metrics like Sum, Mean, Standard Deviation, Maximum and Minimum are applied to get the key descriptive statistics
    
    output$TotalAbBox <- renderValueBox({
        x <- sum(AbortionCR$Induced_abortions)
        valueBox(subtitle = "Total Abortions", x, icon = icon("heart-broken", "fa-3x"))
    })
    
    output$SdAbBox <- renderValueBox({
        x <- round(sd(AbortionCR$Induced_abortions),2)
        valueBox(subtitle = "Abortion Yearly Deviation", x,  icon = icon("exchange-alt", "fa-3x"))
    })
    
    output$NetAbRBox <- renderValueBox({
        x <- mean(AbortionCR$General_abortion_rate)
        valueBox(subtitle = "Net Abortion Rate", x,  icon = icon("capsules", "fa-3x"))
    })
    
    output$SdAbRBox <- renderValueBox({
        v = x <- round(sd(AbortionCR$General_abortion_rate),2)
        valueBox(subtitle = "Abortion Rate Yearly Deviation", x,  icon = icon("exchange-alt", "fa-3x"))
    })
    
    output$HighestAbBox <- renderValueBox({
        x <- max(AbortionCR$Induced_abortions)
        valueBox(subtitle = "Highest Abortions in a Year", x, icon = icon("search-plus", "fa-3x"))
    })
    
    output$LowestAbBox <- renderValueBox({
        x <- min(AbortionCR$Induced_abortions)
        valueBox(subtitle = "Lowest Abortions in a Year", x,  icon = icon("search-minus", "fa-3x"))
    })
    
    output$HighestAbRBox <- renderValueBox({
        x <- round(max(AbortionCR$General_abortion_rate),2)
        valueBox(subtitle = "Highest Abortion Rate in a Year", x,  icon = icon("angle-double-up", "fa-3x"))
    })
    
    output$LowestAbRBox <- renderValueBox({
        v = x <- round(min(AbortionCR$General_abortion_rate),2)
        valueBox(subtitle = "Lowest Abortion Rate in a Year", x,  icon = icon("angle-double-down", "fa-3x"))
    })
    
# ------------------------------------------------------------------------------------------------------------------------------------
    
    # Ridgeline plots are used to show the comparison of variation among factorial fields with low cardinality 
    # Age Category is one such field which is compared with Abortion Rate 
    # This helps to get a quick overview on the Age Categories that requires more attention to look into
    
    output$RidgelineAbRRate <- renderPlot({
        ggplot(AbortionCRAge, aes(x = Abortion_rate, y = Age_of_woman, fill = Age_of_woman)) +
            geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
            labs(title = 'Abortion Rate for Different Age Groups') +
            theme_ipsum() +
            theme(
                legend.position="none",
                panel.spacing = unit(0.1, "lines"),
                strip.text.x = element_text(size = 15)
            )
    }) 
    
    output$RCScatter <- renderPlot({
        ggplot(AbortionCR, aes(x = Induced_abortions, y= General_abortion_rate)) +
            geom_point() +
            geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) +
            labs(title = 'Abortion Rate vs Number of Abortions') +
            xlab("Number of Abortions") +
            ylab("Abortion Rate") +
            theme_ipsum()
    })
    
    output$ScatterData <- renderTable({
        nearPoints(AbortionCR[,1:3], input$plot_click)
    })
    
    # Dplyr is used to filter, sort and present the requested data by applying appropriate conditions
    # These are represented directly as table since it helps to quickly know the stats
    
    output$T5Y <- renderTable(
        {
            TopYears <- AbortionCR %>% arrange(desc(General_abortion_rate))
            return(head(TopYears[,1:3], 5))
        })
    
    output$B5Y <- renderTable(
        {
            TopYears <- AbortionCR %>% arrange(General_abortion_rate)
            return(head(TopYears[,1:3], 5))
        })
    
# ------------------------------------------------------------------------------------------------------------------------------------   
    
    # Time Series Analysis
    # Get the change in trends in Abortion Rate and Number of Abortions over the past 4 decades 
    
# ------------------------------------------------------------------------------------------------------------------------------------
    
    # Line plots are used to visualize the trends over the chosen Timeline
    # These plots are also enhanced further to display the values individually for different Age Groups
       
    output$LineAb <- renderPlot({
        ggplot(AbortionCR, aes(x=year(Date), y=Induced_abortions)) +
            geom_line(color = "red") +
            geom_point() +
            theme_classic2() +
            theme(text = element_text(size=15)) +
            xlab("Year") +
            ylab("Number of Abortions") +
            ggtitle("Number of Abortions 1980 - 2019")
    }) 
    
    output$LineAbRAge <- renderPlot({
        ggplot(AbortionCRAge, aes(x=Period, y=Abortion_rate, group=Age_of_woman, color=Age_of_woman)) +
            geom_line() +
            ylab("Abortion Rate") +
            xlab("Year") +
            scale_color_viridis(discrete = TRUE) +
            ggtitle("Abortion Rate for Different Age Groups 2000 - 2019") +
            scale_colour_manual(values=rainbow(8)) +
            theme(text = element_text(size=15))
    })
    
# ------------------------------------------------------------------------------------------------------------------------------------
    
    # Future Forecast
    # ARIMA model is used to understand the current values and predict the future values till 2025 
    
# ------------------------------------------------------------------------------------------------------------------------------------
    
    # Forecast Plot is captured to show the possible trend in the near Future
    # QQ Plot helps to understand the Actual vs Predicted values 
    # A straight line or close to straight line graph indicates the good performance of the model 
    # ACF says the relation of residual value with past values
    # PACF says the relation of residual value with next values
    # RMSE explains the concentration/deviation of prediction around the best fit
    # 
    
    output$ForecastPlot <- renderPlot({
        plot(ArimaForecast)
    })
    
    output$QQResPlot <- renderPlot({
        qqnorm(ArimaForecast$residuals)
    })
    
    output$ACFResPlot <- renderPlot({
        acf(ArimaForecast$residuals)
    })
    
    output$PACFResPlot <- renderPlot({
        pacf(ArimaForecast$residuals)
    })
    
    output$ARIMAMetrics <- renderPrint({ 
        accuracy(ArimaModel)
    })
    
# ------------------------------------------------------------------------------------------------------------------------------------
    
})
