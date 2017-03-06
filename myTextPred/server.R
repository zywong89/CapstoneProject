library(shiny)
library(tm)
library(tau)
library(dplyr)
source("./loadModel.R")

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
        makePrediction <- reactive({
                string <- input$inputString
                result <- nextWordList(string)
                result$pred
        })
        output$predictedWord1 <- renderPrint(cat(makePrediction()[1]))
        output$predictedWord2 <- renderPrint(cat(makePrediction()[2]))
        output$predictedWord3 <- renderPrint(cat(makePrediction()[3]))
        output$predictedWord4 <- renderPrint(cat(makePrediction()[4]))
        output$predictedWord5 <- renderPrint(cat(makePrediction()[5]))
})
