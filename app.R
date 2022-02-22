#*LOADING PACKAGES*
library(shiny)
library(dplyr)
library(stringr)
library(lubridate)
library(rsconnect)
library(readr)
library(rhandsontable)

#LOADING DATA
urlfile = "https://raw.githubusercontent.com/georgiayoung/VoleLog/main/pvolelogcsv.csv"
pvolelog <- read.csv(url(urlfile))

pvolelog$DOB <- mdy(pvolelog$DOB)
pvolelog$Wean.On <-mdy(pvolelog$Wean.On)
pvolelog$Date.Weaned <-mdy(pvolelog$Date.Weaned)
pvolelog$Separate.By <-mdy(pvolelog$Separate.By)
pvolelog$Actual.Separation.Date <-mdy(pvolelog$Actual.Separation.Date)

#*MAKING APP*
ui <- fluidPage(
    dateInput("date","Date (mm-dd-yy)", format = "mm/dd/yy"),
    textInput("inputID", "Initials", placeholder = "ABC"),
    actionButton("submit", label = "See what's needed today"),
    br(),
    textOutput("blah"),
    textOutput("pressedbutton"),
    textOutput("separate"),
    br(),
    br(),
    rHandsontableOutput("showdata"))


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
    renderedDataTable <- renderRHandsontable(rhandsontable(pvolelog))
    output$showdata <- renderedDataTable

    
    
    
}

shinyApp(ui, server)


