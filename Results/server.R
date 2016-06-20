#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
source("script.R")

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    
    output$expText <- renderText({
        if(input$experimento == "experiment1"){
            "Experimento 1 "  
        }else{
            if(input$experimento == "experiment2"){
                "Experimento 2"  
            }else{
                if(input$experimento == "experiment3"){
                    "Experimento 3"  
                }else{
                    "Experimento 3" 
                }
            }
        }
        
    })
    
    output$expDescription<- renderText({
        if(input$experimento == "experiment1"){
            "Experimento sem remoção de regras ruins (técnica coverage) e atualizando o modelo apenas com instâncias de teste classificadas como pertencente a
            uma nova classe."
        }else{
            if(input$experimento == "experiment2"){
                "Experimento sem remoção de regras ruins (técnica coverage) e atualizando o modelo com instâncias de teste classificadas como pertencente a uma nova
classe e também com instâncias classificadas com 100% de confiança na primeira passagem."  
            }else{
                if(input$experimento == "experiment3"){
                    "Experimento com remoção de regras ruins (técnica coverage) e atualizando o modelo apenas com instâncias de teste classificadas como pertencente a
uma nova classe."  
                }else{
                    "Experimento com remoção de regras ruins (técnica coverage) e atualizando o modelo com instâncias de teste classificadas como pertencente a uma nova
classe e também com instâncias classificadas com 100% de confiança na primeira passagem." 
                }
            }
        }
        
    })
    
    output$plotMetrics <- renderPlot({
        experimento <-

        # gsub(" ", "", paste(experimento,"/", dataset ))
        
        dataFrame <- getDatFrame( gsub(" ", "", paste( input$experimento,"/", input$dataset )))
        
        subset <-dataFrame[which(dataFrame$Perc == input$percentagem/10),]
        plotMacMic(subset, input$dataset, input$percentagem/10)
        })
    
    output$plotTime <- renderPlot({
        experimento <- input$experimento
        dataset <- input$dataset
        # gsub(" ", "", paste(experimento,"/", dataset ))
        
        dataFrame <- getDatFrame( gsub(" ", "", paste(experimento,"/", dataset )))
        subset <-dataFrame[which(dataFrame$Perc == input$percentagem/10),]
        plotTime(subset, input$dataset, input$percentagem/10)
        
        
        
        
    })
    
    
  
    
})
