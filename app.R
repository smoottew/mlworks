# Created 2018-02-18 16_00_00 CEST
#
library(data.table)
library(purrr)
library(chR)
library(koR)
library(stringr)

library(shiny)

source("R/sms spam/2018-02-19 11_49_02 CEST core.R")

model <- readRDS("cloud data/sms spam/2018-02-18 16_07_54 CEST model3.rds")
props <- readRDS("cloud data/sms spam/2018-02-18 16_07_54 CEST props3.rds")

# Define UI for application that draws a histogram
ui <- fluidPage(
  # Application title
  titlePanel("SMS Ham/Spam Detector"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      textAreaInput(inputId = "smsText", label = "Please, enter your SMS text"),
      actionButton (inputId = "predictButton", label = "Is it Ham or Spam ?")
    ),

    # Show a plot of the generated distribution
    mainPanel(
      uiOutput(outputId = "hamSpamLabel")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  output$hamSpamLabel <- renderUI({
    txt <- input$smsText %>% str_trim()
    if (length(txt) == 0 || txt == "")
      tags$h1("Please, enter your text first.")
    else {
      dt <- txt %>% smsHamSpamDT(model, props)
      tags$h1("This is a ", dt[, Label])
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
