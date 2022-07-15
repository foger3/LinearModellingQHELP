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
          actionButton("select", "Display")
        ),
        
        hr(),
        
        splitLayout(plotOutput("graph1"), 
                    plotOutput("graph2"),
                    plotOutput("graph3")),
        hr(),
        
        splitLayout(verbatimTextOutput("text1"),
                    verbatimTextOutput("text2"),
                    verbatimTextOutput("text3"))
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
      
      output$dependent <- shiny::renderUI({
        selectInput("dependent", label = "Select one Outcome", 
                    choices = c(values$choice), multiple = FALSE,
                    selected = values$choice[1], width = 300)
      })
      
      output$independent <- shiny::renderUI({
        if (input$model == 1) {
          selectInput("independent", label = "Select one Predictor", 
                      choices = values$choice[values$choice != input$dependent], 
                      multiple = FALSE, selected = values$choice[1], width = 300)
        } else {
          selectInput("independent", label = "Select all desired Predictors", 
                      choices = values$choice[values$choice != input$dependent], 
                      multiple = TRUE, selected = values$choice[1], width = 300)
        }
      })
      
      output$auswahl <- shiny::renderUI({
        selectInput("auswahl", label = "Investigate one Predictor (others are kept constant)",
                    choices = input$independent, multiple = FALSE,
                    selected = input$independent[1], width = 300)
      })
      
      output$auswahl2 <- shiny::renderUI({
        ls <- c()
        txtx <- NULL
        for (i in 1:length(input$independent)) {
          #ls[i] <- paste("Model", i, sep = "")
          txtx <- c(txtx, input$independent[i])
          ls[i] <- paste(input$dependent, "~", paste(txtx, collapse = "+"))
        }
        selectInput("auswahl2", label = "Select desired model",
                    choices = ls, multiple = FALSE,
                    selected = ls[1], width = 300)
      })
      
    })
    
    observeEvent(input$select, {
      
      if (input$model != 3) {
        values$mod <- input$model
        values$name <- c(input$independent, input$dependent)
        values$ind <- values$data[, input$independent]
        values$dep <- values$data[, input$dependent]
        values$model <- lm(paste(input$dependent, "~", paste(input$independent, collapse = "+")), data = values$data)
        values$sum <- summary(values$model)
        values$df <- data.frame(coef = c(values$sum$coefficients[-1, 1]),
                                coef_name = rownames(values$sum$coefficients)[-1])
      } else {
        values$mod <- input$model
        values$col <- c(input$dependent, input$independent)
        values$ind <- values$data[, input$independent]
        values$dep <- values$data[, input$dependent]
        out_list <- list(matrix(NA, nrow = length(values$col) - 1, ncol = length(values$col) - 1))
        txt <- NULL
        out <- NULL
        
        for (i in 2:length(values$col)) {
          txt <- c(txt, values$col[i])
          txt2 <- paste(values$col[1], "~", paste(txt, collapse = "+"))
          
          nam <- paste("values$fit", i, sep = "")
          out <- assign(nam, lm(data = values$data, formula = txt2))
          out_list[[1]][i-1,i-1:i] <- out$coef[-1]
        }
        
        out_df <- as.data.frame(out_list[[1]][length(values$ind):1, ])
        colnames(out_df) <- values$col[-1]
        modelnr <- c()
        for (i in 1:nrow(out_df)) {
          addition <-  nrow(out_df):i
          modelnr <- c(modelnr, addition)
        }
        
        coeff <- c()
        mod2 <- c()
        n <- 0
        for (i in nrow(out_df):1) {
          n <- n + 1
          dummy <- rep(n, i)
          dummy2 <- rep(values$col[n+1], i)
          coeff <- c(coeff, dummy)
          mod2 <- c(mod2, dummy2)
        }
        
        values$coeff_df2 <- data.frame(modelnr, coeff, mod2, valu = as.vector(na.omit(unlist(out_df))))
        
        #values$aus <- input$auswahl2
        
      }
      
      output$graph1 <- shiny::renderPlot({
        if (values$mod == 1) {
          plot(values$ind, values$dep,
               pch = 21, cex = 2, col ="grey25", bg ="grey80", bty = "l",
               main = "Simple Linear Regression", xlab = values$name[1], ylab = values$name[2])
          car::regLine(values$model, col = "black")
        } else if (values$mod == 2) {
          car::avPlot(values$model, input$auswahl, id = FALSE, grid = FALSE, 
                      pch = 21, cex = 2, col ="grey25", bg ="grey80", col.lines = "black", bty = "l",
                      main = "Multiple Linear Regression", xlab = input$auswahl, ylab = values$name[length(values$name)])
        } 
        # else {
        #   car::avPlot(values$model, input$auswahl2, id = FALSE, grid = FALSE, 
        #               pch = 21, cex = 2, col ="grey25", bg ="grey80", col.lines = "black", bty = "l",
        #               main = "Multiple Linear Regression", xlab = input$auswahl2, ylab = values$col[1])
        # }
      })
      
      output$graph2 <- shiny::renderPlot({
        if (values$mod != 3) {
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
        } else {
          ggplot2::ggplot(values$coeff_df2, ggplot2::aes(x = reorder(mod2, coeff), y = valu, fill = as.factor(modelnr))) +
            ggplot2::theme_minimal() + 
            ggplot2::geom_hline(yintercept = 0, color = "black", size = 0.5) +
            ggplot2::geom_bar(stat = "identity", width = .5, position = "dodge") +
            ggplot2::coord_flip() +
            ggplot2::labs(y = "Estimated Coefficients", x = "") +
            ggplot2::guides(fill = ggplot2::guide_legend(title = "Model Nr."))
        }
      })

      output$graph3 <- shiny::renderPlot({
        if (values$mod != 3) {
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
      
      
      if (values$mod == 1) {
        output$text1 <- shiny::renderText({
          paste("This model studies the ability of prediction of one predictor (independent variable) in one",
                "outcome (dependent variable).",
                "",
                "Predictor (Independent variable): x axis",
                "Outcome (Dependent variable): y axis",
                "",
                "b0 - intercept of the function with x=0",
                "Corresponds to the best estimate for our data when the independent variable (x axis) is 0. *",
                "",
                "b1: slope of the function",
                "Corresponds to the parameter estimating the effect of the independent variable for every subject i.",
                "Tells us how much our prediction of Y changes as X increases by one unit. *",
                "",
                "The greater the linear relation, the more predictive is the independent variable.",
                "*the notation of this function: Yi=b0+b1Xi", sep ="\n")
        })
        
        output$text2 <- shiny::renderText({
          paste("b0: corresponds to the best estimate for our data when the independent variable (x axis) is 0. *",
                "",
                "b1: is the parameter estimating the effect of the independent variable for every subject i. *",
                "",
                "*remind the notation of this function: Yi = b0+b1Xi", sep = "\n")
        })
        
        output$text3 <- shiny::renderText({
          paste("Is a corrected goodness-of-fit measure for linear models. Identifies the percentage of variance in",
                "the dependent variable that is explained by the independent variable, through that specific model,",
                "taking into consideration the number of parameters and the sample size. R-squared measures the goodness",
                "of fit of the regression model.",
                "",
                "If positive the regression model has a better prediction than the prediction made by the mean of the",
                "already available ‘y’ values (equivalent to the “Null Model”). Otherwise, it is negative.",
                "",
                "A higher R-squared indicates the model is a good fit while a lower R-squared indicates the model is not a good fit.",
                "",
                "** In case of the simple linear regression:",
                "Can be reductant having the two values b1 and the RSquared, since we only manipulate one independent variable.",
                "We end saying, in different ways, how much the model explains the prediction we are looking to. RSquare gives",
                "information on how much the model explains the variance in the dependent variable, in comparison to the null model",
                "(the mean of the values on y). b1 gives information on how much the independent variable explains the variation in", 
                "the dependent variable, which in the end is how much the model explains the variation in the VD, since only exists one VI.",
                sep = "\n")
        })
      } else if (values$mod == 2) {
        output$text1 <- shiny::renderText({
          paste("This model makes specific predictions for every observation based on one or more independent", 
                "variables (predictors) of interest.",
                "",
                "Predictor (Independent variable selected): x axis",
                "Outcome (Dependent variable): y axis",
                "",
                "For a graphic representation in 2D, one of the predictors (independent variables) variate, while the other ones",
                "chosen are held constant. Making the outcome (dependent variable) variate.",
                "We can see the effect of the predictor selected on the variation of the outcome when the other predictors are", 
                "held constant. In this way, we are looking for the effect that is only explained by the selected predictor.",
                "",
                "b0 - intercept of the function with x=0",
                "Corresponds to the best estimate for our data when the independent variables are 0. *",
                "",
                "b1: slope of the function",
                "Corresponds to the parameter estimating the effect of the independent variable selected for every subject i,",
                "when the other variables selected are held constant.", sep ="\n")
        })
        
        output$text2 <- shiny::renderText({
          paste("b0: corresponds to the best estimate for our data when the independent variables are 0. *",
                "",
                "bjbj - is the jj parameter estimating the effect of the independent variable jj for every subject ii. *",
                "",
                "*remind the notation of this function: Yi = b0+b1×Xi1...bj×Xi", sep = "\n")
        })
        
        output$text3 <- shiny::renderText({
          paste("Is a corrected goodness-of-fit measure for linear models. Identifies the percentage of variance in",
                "the dependent variable that is explained by the independent variable, through that specific model,",
                "taking into consideration the number of parameters and the sample size. R-squared measures the goodness",
                "of fit of the regression model.",
                "",
                "If positive the regression model has a better prediction than the prediction made by the mean of the",
                "already available ‘y’ values (equivalent to the “Null Model”). Otherwise, it is negative.",
                "",
                "A higher R-squared indicates the model is a good fit while a lower R-squared indicates the model is not a good fit.",
                sep = "\n")
        })      
      }
      
    })
    
  },
  
  options = list(height = 800)
)
 