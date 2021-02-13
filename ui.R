# Author: Raghuraman Srinivasan
# Date: 13 February 2021

# ------------------------------------------------------------------------------------------------------------------------------------  

# This is the User Interface Code Section of the Shiny UI Application. 
# This application features Exploratory Data Analysis and Future Forecasting of Abortion Rates in New Zealand from 1980 to 2019.

# ------------------------------------------------------------------------------------------------------------------------------------  

shinyUI(
    
# ------------------------------------------------------------------------------------------------------------------------------------   
    
    fluidPage(
        
        theme = shinytheme("darkly"),
        useShinyjs(),
        titlePanel(
            fluidRow(
                
                column(2, img(src = "https://www.clipartkey.com/mpngs/m/156-1568900_pregnant-icon-green.png", width = 100, height = 90)),
                column(10, h1(strong("Abortion Statistics in New Zealand 1980 - 2019"), style="color:#33C484"))
                
            )
        ),
        
        add_busy_bar(color = "Yellow"),
        
# ------------------------------------------------------------------------------------------------------------------------------------        
        
        tabsetPanel(
            
# ------------------------------------------------------------------------------------------------------------------------------------   
            
            tabPanel(strong("Home"),

                     fluidRow(
                         valueBoxOutput(outputId = "TotalAbBox", width = 3),
                         valueBoxOutput(outputId = "SdAbBox", width = 3),
                         valueBoxOutput(outputId = "NetAbRBox", width = 3),
                         valueBoxOutput(outputId = "SdAbRBox", width = 3)
                     ),

                     br(),

                     fluidRow(
                         valueBoxOutput(outputId = "HighestAbBox", width = 3),
                         valueBoxOutput(outputId = "LowestAbBox", width = 3),
                         valueBoxOutput(outputId = "HighestAbRBox", width = 3),
                         valueBoxOutput(outputId = "LowestAbRBox", width = 3),
                     ),

                     br(),

                     fluidRow(
                         box(plotOutput(outputId = "RidgelineAbRRate"), width = 7),
                         box(tableOutput(outputId = "T5Y"), title = "Top 5 Years with Highest Abortion Rates", width = 5),
                     ),
                     
                     br(),
                     
                     fluidRow(
                         box(plotOutput(outputId = "RCScatter", click = "plot_click"), width = 7),
                         box(tableOutput(outputId = "B5Y"), title = "Top 5 Years with Lowest Abortion Rates", width = 5),
                         br(),
                         tableOutput("ScatterData")
                     ),
                     
                     br()
            
            ), #Home

# ------------------------------------------------------------------------------------------------------------------------------------

            tabPanel(strong("Time Series Analysis"),

                     fluidRow(
                         plotOutput(outputId = "LineAb"),
                         br(),
                         plotOutput(outputId = "LineAbRAge")
                     )

            ), # Time Series Analysis

# ------------------------------------------------------------------------------------------------------------------------------------

            tabPanel(strong("Future Forecast"),
                     
                     fluidRow(
                         box(plotOutput(outputId = "ForecastPlot")),
                         box(plotOutput(outputId = "QQResPlot"))
                     ),   
                     
                     br(),
                     
                     fluidRow(
                         box(plotOutput(outputId = "ACFResPlot")),
                         box(plotOutput(outputId = "PACFResPlot")),
                         
                     ),
                     
                     br(),
                     
                     fluidRow(
                         verbatimTextOutput(outputId = "ARIMAMetrics")
                     ),
                     
                     br()
                     
            ), # Future Forecast

# ------------------------------------------------------------------------------------------------------------------------------------    

            tabPanel(strong("Data Summary"),
                     verbatimTextOutput(outputId = "Summary1"),
                     verbatimTextOutput(outputId = "Summary2")

            ), # Data Summary

# ------------------------------------------------------------------------------------------------------------------------------------    

            tabPanel(strong("Raw Data"),
                     tabsetPanel(
                         tabPanel(strong("AbortionCR"),
                                  br(),
                                  p(h5(strong("Search and Sort the Dataset"),align = "center")),
                                  dataTableOutput(outputId = "Raw_data1")
                         ),
                         tabPanel(strong("AbortionCRAge"),
                                  br(),
                                  p(h5(strong("Search and Sort the Dataset"),align = "center")),
                                  dataTableOutput(outputId = "Raw_data2")
                         )
                     )
            
            ) # Raw Data
            
# ------------------------------------------------------------------------------------------------------------------------------------    
            
        ) # Tabset Panel
        
# ------------------------------------------------------------------------------------------------------------------------------------    
        
    ) # Fluid Page
    
# ------------------------------------------------------------------------------------------------------------------------------------   
    
) # Shiny UI
