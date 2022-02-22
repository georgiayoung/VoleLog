#*LOADING PACKAGES*
library(shiny)
library(dplyr)
library(stringr)
library(lubridate)
library(rsconnect)
library(readr)
library(rhandsontable)


#LOADING DATA FROM LOCAL FILE
pvolelog <- read.csv("~/VoleLog/pvolelogcsv.csv")

pvolelog$DOB <- mdy(pvolelog$DOB)
pvolelog$Wean.On <-mdy(pvolelog$Wean.On)
pvolelog$Date.Weaned <-mdy(pvolelog$Date.Weaned)
pvolelog$Separate.By <-mdy(pvolelog$Separate.By)
pvolelog$Actual.Separation.Date <-mdy(pvolelog$Actual.Separation.Date)

#*MAKING APP*
ui <- fluidPage(
    #Enter intials and date
    dateInput("date","Date (mm-dd-yy)", format = "mm/dd/yy"),
    textInput("inputID", "Initials", placeholder = "ABC"),
    #See tasks
    br(),
    actionButton("submit", label = "See what's needed today"),
    br(),
    br(),
    textOutput("blah"),
    textOutput("pressedbutton"),
    textOutput("separate"),
    textOutput("pupcheck"),
    
    #Show data log
    br(),
    br(),
    rHandsontableOutput("showdata", height = "300px"))


server = function(input, output) {
    # Getting text for name
    output$blah <- eventReactive(input$submit, {
        paste0("Hi ", input$inputID, ", thanks for checking on the voles!")})
    
    #Getting individuals to wean
    output$pressedbutton <- eventReactive(input$submit, {
        ToWean <- pvolelog %>%
            select(From.Pair, Wean.On) %>%
            filter(Wean.On == input$date)
        paste("Wean:",str_c(ToWean$From.Pair, collapse = ", "))
    })
    
    #Getting individuals to separate
    output$separate <- eventReactive(input$submit, {
        ToSeparate <- pvolelog %>%
            select(From.Pair, Separate.By, Date.Weaned) %>%
            filter(input$date > Date.Weaned) %>%
            filter(input$date <= (Separate.By+days(7)))
        paste("Separate:",str_c(ToSeparate$From.Pair, collapse = ", "))
    })
    
    
    
    #Show Log
    renderedDataTable <- renderRHandsontable(rhandsontable
                                             (pvolelog %>%
                                                     arrange(desc(DOB))) %>%
                                                 hot_context_menu(allowRowEdit = TRUE, 
                                                                  allowColEdit = FALSE, 
                                                                  useTypes = FALSE) %>%
                                                 hot_col("From.Pair", type = "autocomplete"))
    output$showdata <- renderedDataTable
    
#Getting pairs to pupcheck
    output$pupcheck <- eventReactive(input$submit, {
        LastLitter <- pvolelog %>%
            select(DOB, From.Pair, Wean.On, Date.Weaned) %>%
            group_by(From.Pair) %>%
            filter(DOB == max(DOB))
        
        needscheck <- LastLitter %>%
            filter(input$date >= (DOB+days(20))) %>%
            filter(Date.Weaned == NA)
        
        paste("Check for Pups:",str_c(needscheck$From.Pair, collapse = ", "))
        
        
    })
    
    #Add new litter after pupcheck
    
    
    
    
}

shinyApp(ui, server)


