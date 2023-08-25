# Packages ----
library(shiny)  # Required to run any Shiny app
library(tidyverse)
library(readxl)
library(shinythemes)

# Loading data ----


# ui.R ----
ui <- 
  fluidPage(
    theme = shinytheme("superhero"),
    titlePanel(div(h3('elisr', style="margin: 0;"), h4('by marevino', style="margin: 0;"))),
    
    fluidRow(
      
      column(3, 
             selectInput("antigen", h4("Antigen"), 
                         choices = c("IFNB","IFNA","IL1b","CXCL10","IL6"), 
                         selected = "IFNB"), 
             fileInput(inputId = "background.file", 
                       h4("Background file (570 nm)")),
             fileInput(inputId = "signal.file", 
                       h4("Signal file (450 nm)"), 
                       buttonLabel = "Browse ..."),
             numericInput("standard_highest", h4("Highest standard concentration"), 
                            value = 250, min = 0, max = 1000),
             radioButtons("standard_orientation", h4("Standard orientation"),
                            choices = list("Columnwise" = 1, "Rowwise" = 2),
                            selected = 1),
             sliderInput("standard_dilutions", 
                            h4("Standard dilution steps (incl. undiluted, without blank)"), 
                           value = 7, min = 0, max = 12),
            textInput(inputId = "genotypes", h4("Genotypes (comma seperated)"), 
                     value = "Enter genotypes ..."), 
            textInput(inputId = "conditions", h4("Conditions (comma seperated)"), 
                      value = "Enter conditions ...")
     ),
     
     column(3,
            radioButtons("rows", h4("Rows represent"),
                         choices = list("Conditions" = 1, "Genotypes"),
                         selected = 1),
            selectInput("A", "A", choices = c("empty")),
            selectInput("B", "B", choices = c("empty")),
            selectInput("C", "C", choices = c("empty")),
            selectInput("D", "D", choices = c("empty")),
            selectInput("E", "E", choices = c("empty")),
            selectInput("F", "F", choices = c("empty")),
            selectInput("G", "G", choices = c("empty")),
            selectInput("H", "H", choices = c("empty"))
     ),
     
     column(3,
            radioButtons("Columns", h4("Columns represent"),
                         choices = list("Genotypes" = 1, "Conditions"),
                         selected = 1),
            selectInput("1", "1", choices = c("empty")),
            selectInput("2", "2", choices = c("empty")),
            selectInput("3", "3", choices = c("empty")),
            selectInput("4", "4", choices = c("empty")),
            selectInput("5", "5", choices = c("empty")),
            selectInput("6", "6", choices = c("empty")),
            selectInput("7", "7", choices = c("empty")),
            selectInput("8", "8", choices = c("empty")),
            selectInput("9", "9", choices = c("empty")),
            selectInput("10", "10", choices = c("empty")),
            selectInput("11", "11", choices = c("empty")),
            selectInput("12", "12", choices = c("empty")),
     ),
     
     mainPanel(
        plotOutput("standard_plot"),
        tableOutput("results")
     )
    )
  )
  
# server.R ----
server <- function(input, output) {
  
  background <- reactive({
    file <- input$background.file
    ext <- tools::file_ext(file$datapath)
    req(file)
    validate(need(ext == "xlsx", "Please upload a background xlsx file"))
    read_xlsx(file$datapath, range = "A1:L8", col_names = F)
  })
  
  signal <- reactive({
    file <- input$signal.file
    ext <- tools::file_ext(file$datapath)
    req(file)
    validate(need(ext == "xlsx", "Please upload a signal xlsx file"))
    read_xlsx(file$datapath, range = "A1:L8", col_names = F)
  })
  
  standard_df <- reactive({ # creating standards data frame
    
    abs <- signal() - background()
   
    standard_vector <- input$standard_highest/(2^(0:(input$standard_dilutions-1)))
      standard_df <- data.frame(
        conc = rep(standard_vector, 2), 
        abs = c(unlist(abs[1:7,1]), unlist(abs[1:7,2])))
  })
  
  output$standard_plot <- renderPlot({        # plot standard data
    
    ggplot(standard_df(), aes(x = conc, y = abs))+
      geom_point()+
      stat_smooth(method = "lm", formula = y~x, show.legend = T) +     
      labs(title = paste(input$antigen, "Standard curve"),
           x = "concentration",
           y = "background corrected absorbance") +
      theme_minimal()+
      theme(text = element_text(color="black", size= 16, family="TT Arial"))
    })
  
  
  genotypes <- reactive({
    unlist(strsplit(input$genotypes, ", "))
  })
  conditions <- reactive({
    unlist(strsplit(input$conditions, ", "))
  })
  rows <- reactive({
    input$rows == 1
  })
  columns <- reactive({
    input$columns == 1
  })
  
  observeEvent(conditions(),{
    choices <- conditions()
    updateSelectInput(inputId = "A", session = getDefaultReactiveDomain(), 
                      choices = c("empty", "Std", choices))
    updateSelectInput(inputId = "B", session = getDefaultReactiveDomain(), 
                      choices = c("empty", "Std", choices))
    updateSelectInput(inputId = "C", session = getDefaultReactiveDomain(), 
                      choices = c("empty", "Std", choices))
    updateSelectInput(inputId = "D", session = getDefaultReactiveDomain(), 
                      choices = c("empty", "Std", choices))
    updateSelectInput(inputId = "E", session = getDefaultReactiveDomain(), 
                      choices = c("empty", "Std", choices))
    updateSelectInput(inputId = "F", session = getDefaultReactiveDomain(), 
                      choices = c("empty", "Std", choices))
    updateSelectInput(inputId = "G", session = getDefaultReactiveDomain(), 
                      choices = c("empty", "Std", choices))
    updateSelectInput(inputId = "H", session = getDefaultReactiveDomain(), 
                      choices = c("empty", "Std", choices))
  })
  
  observeEvent(genotypes(),{
    choices <- genotypes()
    updateSelectInput(inputId = "1", session = getDefaultReactiveDomain(), 
                      choices = c("empty", "Std", choices))
    updateSelectInput(inputId = "2", session = getDefaultReactiveDomain(), 
                      choices = c("empty", "Std", choices))
    updateSelectInput(inputId = "3", session = getDefaultReactiveDomain(), 
                      choices = c("empty", "Std", choices))
    updateSelectInput(inputId = "4", session = getDefaultReactiveDomain(), 
                      choices = c("empty", "Std", choices))
    updateSelectInput(inputId = "5", session = getDefaultReactiveDomain(), 
                      choices = c("empty", "Std", choices))
    updateSelectInput(inputId = "6", session = getDefaultReactiveDomain(), 
                      choices = c("empty", "Std", choices))
    updateSelectInput(inputId = "7", session = getDefaultReactiveDomain(), 
                      choices = c("empty", "Std", choices))
    updateSelectInput(inputId = "8", session = getDefaultReactiveDomain(), 
                      choices = c("empty", "Std", choices))
    updateSelectInput(inputId = "9", session = getDefaultReactiveDomain(), 
                      choices = c("empty", "Std", choices))
    updateSelectInput(inputId = "10", session = getDefaultReactiveDomain(), 
                      choices = c("empty", "Std", choices))
    updateSelectInput(inputId = "11", session = getDefaultReactiveDomain(), 
                      choices = c("empty", "Std", choices))
    updateSelectInput(inputId = "12", session = getDefaultReactiveDomain(), 
                      choices = c("empty", "Std", choices))
  })
  
  output$results <- renderTable({
    genotypes <- input$genotypes
    genotypes <- as_factor(unlist(strsplit(genotypes, ", ")))
    conditions <- input$conditions
    conditions <- as_factor(unlist(strsplit(conditions, ", ")))
    data.frame(x=genotypes, y=conditions)
  })
  }
# Run the app ----
shinyApp(ui = ui, server = server)