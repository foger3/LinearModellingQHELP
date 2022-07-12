
setwd('/Users/lucat/OneDrive/Documents/Uni Amsterdam/QHELP_Padova/Data/')
df <- mtcars
df$vs <- factor(df$vs, levels = c(0, 1), labels = c("a", "b"))
write.csv(df, "Cars.csv", row.names = FALSE)

shiny::shinyApp(
  
  ui = fluidPage(
    sidebarLayout(
      
      sidebarPanel(
        fileInput("example", "", accept = c("csv")), 
        actionButton("load", "Upload data"),
        conditionalPanel(                     
          condition = "input.load >= '1'",  
          uiOutput("var1"),                
          uiOutput("var2"),                
          actionButton("select", "Select & Display")
        ),
      ),
      
      mainPanel(
        plotOutput("graph1"),
        plotOutput("graph2"),
        verbatimTextOutput("summary")
        #fluidRow(column(4, verbatimTextOutput("txt")))
      )
    )
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
      }
    })
    
    output$var1 <- renderUI({    # remember variable 1? here it is how we extract it
      nam <- colnames(values$data) # from the data set
      selectInput("var1", label = "Select x:", # create the input
                  choices = c(nam), multiple = FALSE,
                  selected = nam[1])
    })
    
    output$var2 <- renderUI({
      nam2 <- colnames(values$data) # create the input for variable 2
      selectInput("var2", label = "Select y:",
                  choices = c(nam2), multiple = FALSE,
                  selected = nam2[1])
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
      
      values$p2 <- ggplot2::ggplot(values$sum, ggplot2::aes(y = summary(values$fit)$adj.r.squared)) +
        ggplot2::geom_bar()
      })
    
    #output$txt <- renderText({ paste(colnames(values$sum)) })
    
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
    
    output$summary <- renderPrint({
      validate(
        need(input$select > 0, "Waiting for data")
      )
      summary(values$fit)
    })
    
  },
  
  
  options = list(height = 800)
)
 