
setwd('/Users/lucat/OneDrive/Documents/Uni Amsterdam/QHELP_Padova/Data/')
df <- mtcars
df$vs <- factor(df$vs, levels = c(0, 1), labels = c("a", "b"))
write.csv(df, "Cars.csv", row.names = FALSE)


library(shiny)
shinyApp(
  
  ui = fluidPage(
        titlePanel("Visual Linear Modelling"),
        
        fileInput("example", "", accept = c("csv")), 
        actionButton("load", "Upload data"),
        
        hr(),
        
        navbarPage(id = "model", "Model:",
                   tabPanel("Simple Linear Regression", value = 1),
                   tabPanel("Multiple Linear Regression", value = 2),
                   tabPanel("Hierarchical Multiple Linear Regression", value = 3)
        ),
        
        # radioButtons("model", "Choose one",
        #              choices = list(
        #                "Simple Linear Regression" = 1, 
        #                "Multiple Linear Regression" = 2, 
        #                "Hierarchical Multiple Linear Regression" = 3), 
        # ),
    
        
        conditionalPanel(
          condition = "input.load >= 1",
          uiOutput("dependent"),
          uiOutput("independent"),
          conditionalPanel(
            condition = "input.model >= 2",
            uiOutput("auswahl"),
            conditionalPanel(
              condition = "input.model == 3",
              uiOutput("auswahl2")
            )
          ),
          actionButton("select", "Select & Display")
        ),
        
        hr(),
        
        splitLayout(plotOutput("graph1"), 
                    plotOutput("graph2"),
                    plotOutput("graph3"))
  ),
  
  server = function(input, output){
    values <- reactiveValues()
    dataInput <- reactive({
        data <- read.csv(input$example$datapath)
    })
    
    observeEvent(input$load, {
      values$data <- data.frame(dataInput())
      if (any(sapply(values$data, is.character)) == TRUE) {
        values$data <- values$data[, !sapply(values$data, is.character) == T]
      } else {
        values$data <- values$data
      }
      values$choice <- colnames(values$data)
    })
    
    observeEvent(input$model, {
      
      output$dependent <- renderUI({
        selectInput("dependent", label = "Select Outcome", 
                    choices = c(values$choice), multiple = FALSE,
                    selected = values$choice[1], width = 170)
        #if (input$model == 1) {
        #   checkboxGroupInput("dependent", "Select Outcome", choices = values$choice, 
        #                      selected = tail(input$dependent, 1), inline = TRUE)
        # } else {
        #   checkboxGroupInput("dependent", "Select Outcome", choices = values$choice, 
        #                      selected = tail(input$dependent, 1), inline = TRUE)
        # }
      })
      
      output$independent <- renderUI({
        if (input$model == 1) {
          selectInput("independent", label = "Select Predictor", 
                      choices = values$choice, multiple = FALSE,
                      selected = values$choice[1], width = 170)
          # checkboxGroupInput("independent", "Select Predictors", choices = values$choice, 
          #                    selected = tail(input$independent, 1), inline = TRUE)
        } else {
          # selected <- input$independent
          # if (is.null(selected)) selected <- values$choices[1]
          # req(!all(values$choices %in% input$independent) | is.null(input$independent), cancelOutput = TRUE)
          selectInput("independent", label = "Select Predictors", 
                      choices = values$choice, multiple = TRUE,
                      selected = values$choice[1], width = 170)
          # checkboxGroupInput("independent", "Select Predictors", choices = values$choice, 
          #                    selected = values$choice, inline = TRUE)
        }
      })
      
      output$auswahl <- renderUI({
        selectInput("auswahl", label = "Select desired predictors",
                    choices = input$independent, multiple = FALSE,
                    selected = input$independent[1], width = 170)
      })
      
      output$auswahl2 <- renderUI({
        selectInput("auswahl2", label = "Select desired model",
                    choices = input$independent, multiple = FALSE,
                    selected = input$independent[1], width = 170)
      })
      
      # output$num <- renderUI({
      #   numericInput("num", label = "Select No. of Predictors", value = 1, width = 170, min = 1, max = length(values$choice))
      # })
      
    })
    
    observeEvent(input$select, {
      values$df <- values$data[c(input$dependent, input$independent)]
      values$fit <- lm(eval(parse(text = input$dependent)) ~ eval(parse(text = input$independent)), data = values$data)
      values$sum <- as.data.frame(summary(values$fit)$adj.r.squared)
      values$p1 <- ggplot2::ggplot(values$df, ggplot2::aes_string(input$dependent, input$independent)) +
        ggplot2::geom_point() +
        ggplot2::geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = 'turquoise4') +
        ggplot2::theme_minimal() +
        ggplot2::labs(x = input$dependent, y = input$independent, title = "Simple Linear Regression Plot") +
        ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, size = 20, face = "bold"))
      
      values$p2 <- ggplot2::ggplot(values$sum, ggplot2::aes(y = 1)) +
        ggplot2::geom_bar()
      
      values$p3 <- ggplot2::ggplot(values$sum, ggplot2::aes(y = summary(values$fit)$adj.r.squared)) +
        ggplot2::geom_bar()
      })
    
    output$graph1 <- renderPlot({
      validate(
        need(input$select > 0, "Waiting for data")
      )
      values$p1
    })
    
    output$graph2 <- renderPlot({
      validate(
        need(input$select > 0, "Waiting for data")
      )
      values$p2
    })
    
    output$graph3 <- renderPlot({
      validate(
        need(input$select > 0, "Waiting for data")
      )
      values$p3
    })
    
  },
  
  
  options = list(height = 800)
)



choices <- LETTERS[1:4]  
shinyApp(
  ui = fluidPage(
    uiOutput("select")
  ),
  server = function(input, output) {
    
    output$select <- renderUI({
      selected <- input$testSelect
      if(is.null(selected)) selected <- choices[1]
      selectizeInput(
        inputId = "testSelect",
        label = "Test",
        choices = choices,
        selected = selected,
        multiple = TRUE,
        options = list(maxItems = 2)
      )
    })
  }
)
 