library(shiny)
library(tidyverse)
library(gplots)
#Please put your winequality-red.csv in your D: drive
#setwd("D:\R")
redwine <- read.csv("winequality-red.csv")
ui <- fluidPage(
  headerPanel("Red Wine Quality Analysis Tool"),
  p(style = "font-family:Impact",
    "Databse from",
    a("KAGGLE",
      href = "https://www.kaggle.com/uciml/red-wine-quality-cortez-et-al-2009%22"))  ,
  p("Data size :", size_sum(redwine)),
  p("Quality Range : ", range(select(redwine, quality))[1], "~", range(select(redwine, quality))[2]), 
    selectInput(inputId = "function1", label = "Analysis Method", 
                choices = c("Quality Threshold" = "Quality Threshold", 
                            "Quality Distribution" = "Quality Distribution", 
                            "Third Variable" = "Third Variable", 
                            "Two Variable Correlation" = "Two Variable Relation",
                            "Continuous distribution of monovariable"="Monovariable")),
    conditionalPanel(
      condition = "input.function1 == 'Quality Threshold'",
      selectInput(inputId = "x1axis",label = "X variable:",names(redwine)),
      selectInput(inputId = "y1axis",label = "Y variable:",names(redwine)),
      sliderInput(inputId = "threshold", label = "Quality Thrshold", value = 5, min = 3, max = 8)),
    conditionalPanel(
      condition = "input.function1 == 'Third Variable'",
      selectInput(inputId = "x2axis",label = "X variable:",names(redwine)),
      selectInput(inputId = "y2axis",label = "Y variable:",names(redwine)),
      selectInput(inputId = "third", label = "Third Variable",names(redwine))),
    conditionalPanel(
      condition = "input.function1 == 'Two Variable Relation'"),
    conditionalPanel(
      condition = "input.function1 == 'Quality Distribution'", 
      selectInput(inputId = "y5axis", label = "Y variable:", names(redwine))),
    conditionalPanel(
      condition = "input.function1 == 'Monovariable'", 
      selectInput(inputId = "x4axis",label = "X-axis:",names(redwine))),
    plotOutput("plot")
)
server <- function(input,output) {
    output$plot <- renderPlot(    
      if (input$function1 == "Quality Threshold")(
        ggplot(data=redwine,mapping=aes_string(x=input$x1axis,y=input$y1axis))
        +geom_point(aes(color=quality<input$threshold),size=2.75)) +
        geom_smooth() +
        labs(title = "Red points mean better wine (quality >= threshold)\nBlue points mean worse wine (quality < threshold)")
      else if(input$function1 == "Third Variable"){
        ggplot(data = redwine) +
          aes_string(x = input$x2axis, y = input$y2axis, color = input$third) +
          geom_point(aes(size=1))
      }
      else if(input$function1 == "Two Variable Relation"){
        heatmap.2(cor(redwine),symm = TRUE,scale='none',col=redgreen(12),key.title = 'Pearson coefficient')
      }
      else if(input$function1 == "Quality Distribution"){
        ggplot(data = redwine) +
          aes_string(x = "quality", y = input$y5axis) +
          geom_boxplot(aes(group = cut_width(quality, 1)))
      }
      else if(input$function1 == "Monovariable"){
        ggplot(data = redwine) +
          aes_string(x = input$x4axis) +
          geom_histogram() +
          geom_freqpoly(size = 1.5, color = "blue")
      }
      ,width=1500,height=550) 
    }
shinyApp(ui = ui, server = server)