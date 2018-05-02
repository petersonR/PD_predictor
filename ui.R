# Shiny UI

# List of packages
pacs <- c("shiny", "shinyBS", "DT", "ggvis", "caret", "gbm", 
          "glmnet", "kernlab", "nnet", "pls", 
          "randomForest", "RANN", "nortest")

# Rate of Progression predictor
library(shiny)
library(shinyBS)
library(DT)
library(ggvis)

## Caret and modeling packages
library(caret)
library(gbm)
library(glmnet)
library(kernlab)
library(MASS)
library(nnet)
library(pls)
library(randomForest)
library(RANN)
library(nortest)

## Observed and predicted clinical outcomes

if(file.exists("outcomes.RData")) {
  load('outcomes.RData')
} else {
  stop("Model data not found")
}

load('glossary.RData')

AppInfo <- list('selectedOutcome' = RatesFitsVars[[5]], 
                'version' = '1.1')

## User interface
shinyUI(
  fluidPage(theme = "bootstrap.css",
    div(class = "outer",
        titlePanel("Parkinson's Rate of Progression Predictor"),
        sidebarLayout(
          sidebarPanel(
            fluidRow(
              column(12,
                     selectInput("outcome",
                                 "Clinical outcome to predict",
                                 names(RatesFitsVars),
                                 selected = names(RatesFitsVars)[5]),
                     selectInput("metric",
                                 "Metric",
                                 RatesFitsVars[[AppInfo$selectedOutcome]]),
                     sliderInput("topVars",
                                 "How many Variables to set?",
                                 min = 1, max = 111, 
                                 value = 3, step = 1), 
                     radioButtons('level', 'Level for Prediction Intervals', 
                                 choices = c(.7, .8, .9), selected = .8, inline = T),
                     hr(),
                     h3("Inputs for Prediction"),
                     uiOutput('predictionControls')
                  )
              )
            ),
          mainPanel(
            tabsetPanel(type = 'tabs',
              tabPanel("Predictions",
                       verbatimTextOutput("Predictions"),
                       plotOutput('plotSlope')),
              tabPanel("Effect Visualizer", 
                       plotOutput("plotPredictions"),
                       uiOutput('plotSelector'),
                       uiOutput('plotSelector2')),
              tabPanel("Model Info",
                       verbatimTextOutput("modelInfo"),
                       plotOutput("varImp")),
              tabPanel("Model Diagnostics",
                       plotOutput('resPlot'), 
                       plotOutput('resPlot2', height = '300px'),
                       verbatimTextOutput('skedTest')),
              tabPanel("Imputed Covariates", 
                       verbatimTextOutput('Imputations')), 
              tabPanel("Glossary", 
                       dataTableOutput("glossary")),
              tabPanel("References", 
                       uiOutput("references"),
                       uiOutput("references2"))
            )
          )
        ),
        hr(
          HTML(paste('Version', AppInfo$version, " - ")), 
          a("View app source code", href = "https://github.com/petersonR/PD_predictor"), 
          br(), HTML('\uA9 2018, R Peterson'), br(), HTML(RunDate)
        )
    )
  ))
