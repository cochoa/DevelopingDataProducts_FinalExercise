#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyjs)
library(leaflet)
library(htmltools)
library(gridExtra)
library(ggplot2)
library(rpart)
library(randomForest)
library(caret)

#Function to calculate r squared
rsq <- function (x, y) cor(x, y) ^ 2

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    #Read data
    data_raw<-read.csv("data.csv",sep=";", encoding = "UTF-8")
    names(data_raw)[1]<-"country"
    predictors<-data_raw[,2:12]
    country<-data_raw[,1]
    smokers<-data_raw[,13]
    dat <- reactive({
      if (input$normalize) {
        predict(preProcess(data_raw, method = c("center","scale")),data_raw)
        } else {
          dat<-data_raw
        }
      })
    
    #Map
    output$smokemap <- renderLeaflet({
      dat() %>%
      leaflet() %>%
      addTiles() %>%
      addMarkers(
        ~lng, 
        ~lat, 
        label = ~htmlEscape(paste(country," - smokers: ",smokers*100,"%"))
        )
      })
    
      #Plot
      parameters <- reactive({ input$predictors })
      
      #Create a linear model per each predictor
      model_list <- reactive ({
        formulas <- sapply(
          parameters(),
          function(e) {
            paste("smokers ~ ",paste(e, collapse = " + "))
          }
        )
        lapply(
          formulas,
          function(e) {lm(formula=e, dat())}
        )
      })
      
      #A plot per each lineal model
      output$plot1 <- renderPlot({
      qplot_list <- lapply(
        1:length(parameters()),
        function(e) {
          qplot(
            x=dat()[,parameters()[e]],
            y=dat()$smokers,
            xlab=parameters()[e],
            ylab="% Smokers"
          ) 
        }
      )
      if (input$plotmodel) {
        qplot_list <- lapply(qplot_list,function(e) {e <- e + stat_smooth(method="lm", se=T)})
      }
      grid.arrange(grobs=qplot_list,ncol=length(qplot_list))
    })
    
    #Create a complete model with all the selected predictors
      
    #Create a formula expression with the parameters
    formula <- reactive({
      paste("smokers ~ ",paste(parameters(), collapse = " + "))
    })

    #Lineal model
    model_lineal <- reactive({
      lm(formula=formula(), dat())
    })

    output$model_lineal_coef <- renderTable(
      data.frame(
        Coefficients=names(model_lineal()$coefficients),
        Value=model_lineal()$coefficients
      ),
      digits=8
    )
    output$model_lineal_r2 <- renderText({paste("R squared: ", round(rsq(predict(model_lineal()),smokers),digits = 2))})

    #Tree model
    model_rpart <- reactive({
      rpart(formula=formula(), dat())
    })
    output$model_rpart_desc <- renderPrint({model_rpart()})
    output$model_rpart_r2 <- renderText({paste("R squared: ", round(rsq(predict(model_rpart()),smokers),digits = 2))})

    #Random forest model
    model_rf <- reactive({
      randomForest(as.formula(formula()), dat(), importance=T)
    })
    output$model_rf_desc <- renderPrint({summary(model_rf())})
    output$model_rf_importance <- renderTable(
      data.frame(
        Variables=rownames(importance(model_rf())),
        Importance=importance(model_rf(),type=1)[,1]
      ),
      digits=8
    )
    output$model_rf_r2 <- renderText({paste("R squared: ", round(rsq(predict(model_rf()),smokers),digits = 2))})    
    
    #Show each element depending on user's preferences
    observe({
      toggle(id = "lineal_div", condition = input$lineal)
      toggle(id = "rpart_div", condition = input$rpart)
      toggle(id = "rf_div", condition = input$rf)
      toggle(id = "map_div", condition = input$map)
    })    
})
