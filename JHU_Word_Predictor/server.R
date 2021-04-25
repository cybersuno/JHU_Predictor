#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

source("../predictModel.R",local=TRUE)

model<-loadModel_v4("../m210425_10pct.RData")

readyToPredict <- function(inputtext) {
    rslt<-"<empty>"
    if (inputtext=="") {
        rslt<-"<empty>"
    }
    else {
        #get last letter for the input
        last<-substr(inputtext,nchar(inputtext),nchar(inputtext))
        if(last==" ") {
            rslt<-tolower(inputtext)  
        }
    }
    
    rslt
}

usePattern <- function(inputtext) {
    #get last character for the input
    last<-substr(inputtext,nchar(inputtext),nchar(inputtext))
    
    #if last character is not a " ", then we use last characters as a pattern for our list
    last!=" "
}


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    #loadModel_v3("m210418.RData")
    
    
    inputText <- reactive({
        t <- input$text
        t
    })  
    
    
    
    output$transformed_text <- renderText({
        transformed<-paste("Text used to predict:",readyToPredict(inputText()),sep=" ")
        print(transformed)
        
    })
    
    output$prediction <- renderText({
        patt<-usePattern(input$text)
        totalList<-input$suggestions
        
        sl<-getSuggesionList_v4(model,input$text,input$suggestions,patt)
        if (length(sl)<totalList && patt) {
            sl2<-getSuggesionList_v4(model,input$text,(input$suggestions-length(sl))*2,FALSE)
        }
        sl<-unique(c(sl,sl2))
        sl
    })
    
    output$wd<-renderText({
        #setwd("W:/prj/JHU_DS/QuantedaTutorials")
        print(getwd())
        #print("Hello")
    })
})

