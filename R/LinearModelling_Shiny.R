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
        values$ind <- values$data[, input$independent]
        values$dep <- values$data[, input$dependent]
        values$model <- lm(paste(input$dependent, "~", paste(input$independent, collapse = "+")), data = values$data)
        values$sum <- summary(values$model)
        values$df <- data.frame(coef = c(values$sum$coefficients[-1, 1]),
                                coef_name = rownames(values$sum$coefficients)[-1])

      }
      
      output$graph1 <- renderPlot({
        validate(
          need(input$select > 0, "Waiting for data")
        )
        if (input$model == 1) {
          plot(values$ind, values$dep,
               pch = 21, cex = 2, col ="grey25", bg ="grey80", bty = "l",
               main = "Simple Linear Regression", xlab = input$independent, ylab = input$dependent)
          car::regLine(values$model, col = "black")
        } else if (input$model == 2) {
          car::avPlot(values$model, input$auswahl, id = FALSE, grid = FALSE, 
                      pch = 21, cex = 2, col ="grey25", bg ="grey80", col.lines = "black", bty = "l",
                      main = "Simple Linear Regression", xlab = input$auswahl, ylab = input$dependent)
        }
      })
      
      output$graph2 <- renderPlot({
        validate(
          need(input$select > 0, "Waiting for data")
        )
        if (input$model != 3) {
          ggplot2::ggplot(values$df, aes(x = coef_name, y = coef)) +
            ggplot2::theme_minimal() +
            ggplot2::geom_hline(yintercept = 0, color = "black", size = 0.5) +
            ggplot2::geom_bar(stat = "identity", fill = "grey", col = "black",
                              width = .5, position = position_dodge(.1)) +
            ggplot2::coord_flip() +
            ggplot2::labs(y = "Estimated Coefficients", x = "") +
            ggplot2::theme(panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(),
                           panel.background = ggplot2::element_blank(), axis.ticks = ggplot2::element_blank(),
                           axis.line.x = ggplot2::element_line(colour = "black"), text = element_text(size = 12),
                           axis.text = element_text(size = 12)) 
        }
      })

      output$graph3 <- renderPlot({
        validate(
          need(input$select > 0, "Waiting for data")
        )
        if (input$model != 3) {
          data <- data.frame(rs = values$sum$adj.r.squared, model = "")
          ggplot2::ggplot(data, ggplot2::aes(x = model, y = rs)) +
            ggplot2::geom_hline(yintercept = 0, color = "black", size = 0.5) +
            ggplot2::geom_bar(stat = "identity", fill = "grey", col = "black", 
                              width = .3, position = position_dodge(.1)) +
            ggplot2::xlab(label = "Model") +
            ggplot2::ylab(label = "Adjusted R Squared") +
            ggplot2::scale_y_continuous(breaks = seq(0, 1, 0.2), limits = c(0, 1)) +
            ggplot2::theme(panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(),
                           panel.background = ggplot2::element_blank(), axis.ticks = ggplot2::element_blank(),
                           axis.line.y = ggplot2::element_line(colour = "black"), text = element_text(size = 12),
                           axis.text = element_text(size = 12))+
            ggplot2::coord_cartesian(ylim = c(0.046, 1))
        }
      })
   })
    
  },
  
  options = list(height = 800)
)
 