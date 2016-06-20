#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
    
    # Application title
    titlePanel("Resultados - Resolução de Entidades Incremental"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("experimento", "Experimento:",
                        list("Experimento 1" = "experiment1", 
                             "Experimento 2" = "experiment2", 
                             "Experimento 3" = "experiment3",
                             "Experimento 4" = "experiment4")
                        
            ),
            br(),
            radioButtons("dataset", "Base de Dados:",
                         c("Printers" = "printers",
                           "Uol-Electonic" = "electronic",
                           "Uol-Non-Electronic" = "nonElectronic",
                           "Cora Ref" = "cora",
                           "Cora" = "cora2",
                           "PVAF" = "pvaf"
                          # "Books" = "books"
                         )
            ),
            
            br(),
            
            sliderInput("percentagem",
                        "Percentagem de dados no treino:",
                        min = 10,
                        max = 90,
                        step = 40,
                        value =10),
            
            br(), br(), br(),
            h3( htmlOutput("expText")),
            h4(htmlOutput("expDescription"))
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
           
            
         
            plotOutput("plotMetrics"),
            br(), br(),
            plotOutput("plotTime")
        )
    )
))
