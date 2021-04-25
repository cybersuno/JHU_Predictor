#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

#library(quanteda)

#source("predictModel.R",local=TRUE)


# Define UI for application that draws a histogram
shinyUI(fluidPage(
    
    # Application title
    titlePanel("JHU Word Predictor"),
    
    # Sidebar with a slider input for number of bins
    #sidebarLayout(
    #    sidebarPanel(
    #        h2("List of suggestions:")
    #    ),
    
    # Show a plot of the generated distribution
    #    mainPanel(
    #        plotOutput("distPlot")
    #    )
    #)
    fluidRow(
        column(12, 
               sliderInput(
                   "suggestions", label = "Number of suggestions:",
                   min = 1, value = 5, max = 10)
        )
    ),
    fluidRow(
        column(12,
               textInput("text","Input here your text:"))
    ),
    fluidRow(
        column(12,
               textOutput("transformed_text"))
    ),
    fluidRow(
        column(12,
               textOutput("wd"))
    ),
    fluidRow(
        column(12,
               tableOutput("prediction"))
    ),
    fluidRow(
        column(12,
               textOutput("model_info"))
    )
)
)