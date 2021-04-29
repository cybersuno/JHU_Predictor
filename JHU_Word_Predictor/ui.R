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
    titlePanel("JHU Word Predictor - Data Science specialization capstone by Johns Hopkins University & Coursera"),
    sidebarPanel(
        tags$b("WELCOME!"),
        helpText("...to the word predictor app. This application tries to predict the word that you are writing or the next word"),
        tags$b("FIRST"),
        helpText("you can select the size of the suggestion list in the slider. For instance, if you set a 3 in the slider, then 3 suggestions will be shown"),
        tags$b("THEN"),
        helpText("you can write a sentence in the text input box. While you are writing, the prediction is working behind scenes"),
        helpText("Please note that the prediction is for the rest of the word you are writing or, when a space is found, for the next word."),
        helpText("An example, while you are writing \"my lovely cat i\", the prediction will use \"my lovely cat\" to make predictions but only will be collected words starting with an \"i\"."),
        helpText("Then, when you finish \"is\" and write the following blank space, the prediction will use \"lovely cat is\" to propose a suggestion list for the next word, without restrictions"),
        tags$b("MORE INFO"),
        helpText("Please find the presentation for this application"),
        tags$a(href="http://rpubs.com/cybersuno/JHU_Word_Predictor", "here!")
        ),
    mainPanel(
        h3("1.- Select the number of suggestions"),
        fluidRow(
            column(6, 
                   sliderInput(
                       "suggestions", label = "Number of suggestions:",
                       min = 1, value = 5, max = 10, ticks=FALSE)
            ), 
            column(6, textOutput("suggestion_list_size"))
        ),
        fluidRow(
            h3("2.- Input the text"),
            column(6,
                   textInput("text","Input here your text:")),
            column(6,
                   textOutput("transformed_text"))
        ),
        h3("3.- Prediction: Watch the suggestion list for next word"),
        tags$b("Predictions:"),
        fluidRow(
            column(12,
                   tableOutput("prediction"))
        )
    )
)
)