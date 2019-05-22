#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    output$outputWord1 <- renderText({ input$inputWord1 })

    # nextWordPred <- reactive({
    #     wordPred(input$inputWord1)
    #     }) 

    output$outputWord2 <- renderText({ as.String(wordPred(input$inputWord1)[[1,1]]) })
    
    output$tableOut1 <- renderTable(wordPred(input$inputWord1), striped = TRUE, bordered = TRUE,
                                    width = "auto", rownames = TRUE)
    
    # output$outputWord1 <- renderText({input$inputWord1})
    # 
    # # output$distPlot <- renderPlot({
    # #   
    # #   # generate bins based on input$bins from ui.R
    # #   x    <- faithful[, 2] 
    # #   bins <- seq(min(x), max(x), length.out = input$bins + 1)
    # #   
    # #   # draw the histogram with the specified number of bins
    # #   hist(x, breaks = bins, col = 'darkgray', border = 'white')
    # 
    # # })
    
})

