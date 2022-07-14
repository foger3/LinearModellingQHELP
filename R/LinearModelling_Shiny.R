
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
      })
      
      output$independent <- renderUI({
        if (input$model == 1) {
          selectInput("independent", label = "Select Predictor", 
                      choices = values$choice, multiple = FALSE,
                      selected = values$choice[2], width = 170)
        } else {
          selectInput("independent", label = "Select Predictors", 
                      choices = values$choice, multiple = TRUE,
                      selected = values$choice[2], width = 170)
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
      
    })
    
    observeEvent(input$select, {

      if (input$model != 3) {
        values$model <- lm(paste(input$dependent, "~", paste(input$independent, collapse = "+")), data = values$data)
        values$sum <- summary(values$model)
      }
      
      output$graph1 <- renderPlot({
        validate(
          need(input$select > 0, "Waiting for data")
        )
        if (input$model == 1) {
          plot(values$data[, input$independent], values$data[, input$dependent])
          car::regLine(values$model)
        } else if (input$model == 2) {
          car::avPlot(values$model, input$auswahl, id = FALSE, grid = FALSE)
        }
      })
      
      output$graph2 <- renderPlot({
        validate(
          need(input$select > 0, "Waiting for data")
        )
        if (input$model != 3) {
          data <- data.frame(RS = values$sum$adj.r.squared, MO = "")
          ggplot2::ggplot(data, ggplot2::aes(x = MO, y = RS)) +
            ggplot2::geom_hline(yintercept = 0, color = "black", size = 0.5) +
            ggplot2::geom_bar(stat = "identity", fill = "grey", col = "black", width = .3) +
            ggplot2::xlab(label = "") +
            ggplot2::ylab(label = "Adjusted R Squared") +
            ggplot2::scale_y_continuous(breaks = seq(0, 1, 0.2), limits = c(0, 1)) +
            ggplot2::theme(panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(),
                           panel.background = ggplot2::element_blank(), axis.ticks = ggplot2::element_blank(),
                           axis.line.y = ggplot2::element_line(colour = "black")) +
            ggplot2::coord_cartesian(ylim = c(0.046, 1))
        }
      })

      output$graph3 <- renderPlot({
        validate(
          need(input$select > 0, "Waiting for data")
        )
        if (input$model != 3) {
          data <- data.frame(RS = values$sum$adj.r.squared, MO = "")
          ggplot2::ggplot(data, ggplot2::aes(x = MO, y = RS)) +
            ggplot2::geom_hline(yintercept = 0, color = "black", size = 0.5) +
            ggplot2::geom_bar(stat = "identity", fill = "grey", col = "black", width = .3) +
            ggplot2::xlab(label = "") +
            ggplot2::ylab(label = "Adjusted R Squared") +
            ggplot2::scale_y_continuous(breaks = seq(0, 1, 0.2), limits = c(0, 1)) +
            ggplot2::theme(panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(),
                           panel.background = ggplot2::element_blank(), axis.ticks = ggplot2::element_blank(),
                           axis.line.y = ggplot2::element_line(colour = "black")) +
            ggplot2::coord_cartesian(ylim = c(0.046, 1))
        }
      })
      
      # values$df <- values$data[c(input$dependent, input$independent)]
      # values$fit <- lm(eval(parse(text = input$dependent)) ~ eval(parse(text = input$independent)), data = values$data)
      # values$sum <- as.data.frame(summary(values$fit)$adj.r.squared)
      # values$p1 <- ggplot2::ggplot(values$df, ggplot2::aes_string(input$dependent, input$independent)) +
      #   ggplot2::geom_point() +
      #   ggplot2::geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = 'turquoise4') +
      #   ggplot2::theme_minimal() +
      #   ggplot2::labs(x = input$dependent, y = input$independent, title = "Simple Linear Regression Plot") +
      #   ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, size = 20, face = "bold"))
      # 
      # values$p2 <- ggplot2::ggplot(values$sum, ggplot2::aes(y = 1)) +
      #   ggplot2::geom_bar()
      # 
      # values$p3 <- ggplot2::ggplot(values$sum, ggplot2::aes(y = summary(values$fit)$adj.r.squared)) +
      #   ggplot2::geom_bar()
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
 