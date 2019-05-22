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
    titlePanel("Coursera Data Science Capstone"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            textInput("inputWord1", label = h4("Introduce a word in the box:"),value = "", width = NULL, placeholder = NULL),
            h6("Only english words supported."),
            # submitButton(text = "Predict", icon = icon("refresh")),
            h6("You entered: "),
            verbatimTextOutput("outputWord1")
        ),

        mainPanel(
            h1("Predicted word"),
            verbatimTextOutput("outputWord2"),
            tableOutput("tableOut1")
        )
    )
))
