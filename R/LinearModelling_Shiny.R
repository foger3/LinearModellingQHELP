
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
          condition = "input.model >= 2",
          numericInput("num", label = h3("Numeric input"), value = 3)
        ),
        
        conditionalPanel(
          condition = "input.load >= 1",
          uiOutput("var1"),
          uiOutput("var2"),
          # lapply(3:("input.num"), function(i) {
          #   strong(paste0("var", i),br())
          # }),
          uiOutput("var3"),
          actionButton("select", "Select & Display")
        ),
        
        hr(),
        
        fluidRow(
          splitLayout(plotOutput("graph1"), 
                      plotOutput("graph2"),
                      plotOutput("graph3"))
        ),
        fluidRow(column(4, verbatimTextOutput("txt")))
  ),
  
  server = function(input, output){
    values <- reactiveValues()
    dataInput <- reactive({
        data <- read.csv(input$example$datapath)
    })
    observeEvent(input$load, {
      values$data <- data.frame(dataInput())
      if (any(sapply(values$data, is.character)) == TRUE) {
        values$data = values$data[, !sapply(values$data, is.character) == T]
      } else {
        values$data = values$data
      }
    })
    
    
    observeEvent(input$model, {
      output$var1 <- renderUI({
        nam <- colnames(values$data) 
        selectInput("var1", label = "Select x:", 
                    choices = c(nam), multiple = FALSE,
                    selected = nam[1])
      })
      
      output$var2 <- renderUI({
        nam2 <- colnames(values$data) 
        selectInput("var2", label = "Select y:",
                    choices = c(nam2), multiple = FALSE,
                    selected = nam2[1])
      })
      
      if (input$model != 1) {
        output$varX <- renderUI({
          # lapply(3:(input$num + 2), function(i) {
          #   strong(paste0("var", i),br())
          # })
          
          nam3 <- colnames(values$data)
          selectInput("varX", label = "Select y:",
                      choices = c(nam3), multiple = FALSE,
                      selected = nam3[1])
        })
      } else {
        output$varX <- NULL
      }
    })
    
    observeEvent(input$select, {
      values$df <- values$data[c(input$var1, input$var2)]
      values$fit <- lm(eval(parse(text = input$var1)) ~ eval(parse(text = input$var2)), data = values$data)
      values$sum <- as.data.frame(summary(values$fit)$adj.r.squared)
      values$p1 <- ggplot2::ggplot(values$df, ggplot2::aes_string(input$var1, input$var2)) +
        ggplot2::geom_point() +
        ggplot2::geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = 'turquoise4') +
        ggplot2::theme_minimal() +
        ggplot2::labs(x = input$var1, y = input$var2, title = "Simple Linear Regression Plot") +
        ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, size = 20, face = "bold"))
      
      values$p2 <- ggplot2::ggplot(values$sum, ggplot2::aes(y = 1)) +
        ggplot2::geom_bar()
      
      values$p3 <- ggplot2::ggplot(values$sum, ggplot2::aes(y = summary(values$fit)$adj.r.squared)) +
        ggplot2::geom_bar()
      })
    
    output$txt <- renderText({ paste(class(input$model)) })
    
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



ui <- fluidPage(
  title = 'Creating a UI from a dynamic loop length',
  sidebarLayout(
    sidebarPanel(
      # Determine Length of Loop
      numericInput(inputId = "NumLoop", "Number of Loops", value = 5, min = 1, max = 10, step = 1)
    ),
    mainPanel(
      # UI output
      uiOutput('moreControls')
    )
  )
)

server <- function(input, output, session) {
  
  output$moreControls <- renderUI({
    lapply(1:input$NumLoop, function(i) {
      strong(paste0('Hi, this is output B#', i),br())
    })
  })
  
}

shinyApp(ui = ui, server = server)
 