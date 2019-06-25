#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyjs)
library(leaflet)

#Read data
data<-read.csv("data.csv",sep=";", encoding = "UTF-8")
names(data)[1]<-"country"
predictors<-data[,2:12]
country<-data[,1]
smokers<-data[,13]

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  useShinyjs(),  # Set up shinyjs
  
  # Application title
  titlePanel("Smoke data for Europe"),
  p(id="subtitle",strong("This shiny app helps you to understand which variables influence the more the
    percentage of smoking population in each country. You may want to explore predicting
    variables such as latitude and longitude of the country, GFP per capita and several religions.
    Just select you preferred options in the lest panel, push the button and see the results.")),
  tags$head(tags$style("#subtitle{color: grey}")),
  hr(),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      p("Select your preferences for the analysis and click the button to see the results:"),
      selectInput("predictors","Predictors (multiple choice):",choices=names(predictors), selected = "lat",multiple = TRUE,selectize = TRUE),
      checkboxInput("normalize","Do you want to normalize predictors?", value = FALSE, width = NULL),
      h5(strong("Models to be fitted:")),
      checkboxInput("lineal","Lineal model", value = TRUE, width = NULL),
      checkboxInput("rpart","Tree model (CART)", value = FALSE, width = NULL),
      checkboxInput("rf","Random Forest", value = FALSE, width = NULL),
      h5(strong("Other options:")),
      checkboxInput("map","Show a map with each %smokers value", value = TRUE, width = NULL),
      checkboxInput("plotmodel", "Show lineal models on top of the the plot",value = TRUE, width = NULL),
      submitButton("Apply changes!")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      div(id="plot_title",
        h4(strong("Plot")),
        p("This section shows an scatter plot of each predictor selected in the left column compared to the % of smokers. If you have selected the option 'show lineal models on top...', you will see an independent lineal model for each separate predictor"),
        plotOutput("plot1", height = 300)
        ),
      div(id="Full Model",
          h4(strong("Complete models considering all the selected predictors together")),
          p("This section shows how well a set of different models descrive the relationship between %somokers and the selected predictors. An description of each model is shown as well as the R-squared of the fitted models. Please, select which models you wat to include in the 'Models to be fitted' section in left bar")
      ),
      div(id="lineal_div",
        h5(id="model_lineal_title",strong("LINEAL MODEL:")),
        p(strong("Coefficientes of the lineal model:"),"(normalize predictors in the left column to asses which predictors are more relevants)"),
        tableOutput("model_lineal_coef"),
        textOutput("model_lineal_r2"),
        tags$head(tags$style("#model_lineal_r2{color: red}")),
        hr(id="lineal_separator")
        ),
      div(id="rpart_div",
        h5(id="model_rpart_title",strong("TREE MODEL (CART):")),
        p(strong("Model description:")),
        textOutput("model_rpart_desc"),
        textOutput("model_rpart_r2"),
        tags$head(tags$style("#model_rpart_r2{color: red}")),
        hr(id="rpart_separator")
        ),
      div(id="rf_div",
        h5(id="model_rf_title",strong("RANDOM FOREST:")),
        p(strong("Model description:")),
        textOutput("model_rf_desc"),
        tableOutput("model_rf_importance"),
        textOutput("model_rf_r2"),
        tags$head(tags$style("#model_rf_r2{color: red}")),
        hr(id="rf_separator")
        ),
      div(id="map_div",
          h4("Map"),
          p("Click on each European country to see the name and the % of smokers"),
          leafletOutput("smokemap", height = 400),
          hr())
      
    )
  )
))
