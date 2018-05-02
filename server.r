# Shiny Server

# List of packages
pacs <- c("shiny", "shinyBS", "DT", "ggvis", "caret", "gbm", 
          "glmnet", "kernlab", "nnet", "pls", "base", 
          "randomForest", "RANN", "nortest")

# Rate of Progression predictor

library(shiny)
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
# load("Programs\\Analysis\\Rate Predicting\\shiny\\outcomes.RData")
if(file.exists("outcomes.RData")) {
  load('outcomes.RData')
} else {
  stop("Model data not found")
}

load('glossary.RData')

## Descriptive statistics
describe <- function(x, digits = 3) {
  stats <- c(mean(x), sd(x), quantile(x, c(0, 0.25, 0.5, 0.75, 1)))
  structure(signif(stats, digits), names = c("Mean", "Std Dev", "Min", "25th",
                                             "Median", "75th", "Max"))
}

# Create function that takes in data frame (or NULL) and predicts slope + PI
getPredictions <- function(Fit, outcome = 'np1total.rate', 
                           user_input = NULL, plotAcross = NULL,
                           level = .8, plotObserved = T, plotXRes = 100, 
                           printImpute = F, plotAcrossTime = F, 
                           plotInterval = F,
                           addInterceptVar = NULL,
                           holdVarsConstant = F, shiftPI = T, shiftby = 0, 
                           compareToGLM = F, rateGLM = NULL) {
  outcomeBL <- substr(outcome, 1, gregexpr("\\.[^\\.]*$", outcome)[[1]]-1)
  
  if(is.null(Fit)) return('Outcome Model Not Recognized.')
  
  varIDX <- names(Fit$trainingData) != '.outcome'
  vars <- names(Fit$trainingData)[varIDX]
  fitData <- Fit$trainingData[varIDX]
  
  # In case of nptotal, for instance
  if(!any(names(fitData) == outcomeBL)) {
    cat('No Valid BL value found for outcome\n')
    outcomeBL <- 'age_at_bl'
  }
  
  ## Create newdata 
  newdata <- rbind(NA, fitData)[1,]
  
  # Put user input into newdata or use mean of outcomeBL
  if(length(user_input)) {
    user_inputVars <- names(user_input)
    recognized <- (user_inputVars %in% names(Fit$trainingData))
    
    if(!all(recognized)) {
      cat('Note: The following input variable(s) not recognized:\n', 
          paste(user_inputVars[!recognized]), '\n')
    } else if(any(recognized)) {
      newdata[user_inputVars[recognized]] <- user_input[recognized]
      cat('Recognized the following inputs:\n', 
          paste(paste(user_inputVars[recognized], unname(user_input[recognized]), 
                sep = ' = '), '\n'), '\n')
    }
    
  } else {
    newdata[[outcomeBL]] <- mean(fitData[[outcomeBL]], na.rm = T)
    user_inputVars <- outcomeBL
    cat('Imputing variables at mean of', outcomeBL, 'at BL\n')
  }
  
  # Warn if newdata is specified and they don't specify >1 BL covariate
  if(all(is.na(newdata))) {
    cat('Must specify one nonmissing BL covariate, choosing mean of', outcomeBL, 'at BL\n')
    newdata[[outcomeBL]] <- mean(fitData[[outcomeBL]], na.rm = T)
  }
  
  # Impute missing predictors using KNN and extract predictions
  p <- predict(Fit, newdata)
  
  impValues.std <- predict(Fit$preProcess, newdata)
  
  # In case you care how far from the observed data the prediction is
  # impDist <- dist(rbind(impValues, fitData))
  # avgDist <- apply(as.matrix(impDist), 2, mean)
  # avgDist[1] / mean(avgDist[-1])
  impValues <- (impValues.std) * (Fit$preProcess$std) + Fit$preProcess$mean
  v <- varImp(Fit, nonpara=FALSE)
  
  if(printImpute) {
    
    cat('Based on these inputs, the following were imputed, and taken to be KNOWN values:\n')
    i <- t(impValues)
    i <- as.data.frame(i)
    i$rownames <- rownames(i)
    rownames(i) <- NULL
    
    
    sorted <- i[order(v$importance, decreasing = T),]
    i <- subset(sorted, !(rownames %in% user_inputVars))
    rownames(i) <- i$rownames
    i$rownames <- NULL
    head(i)
    colnames(i) <- NULL

    print(round(i,2))
    return(list())
  }
  
  # Create PI
  z <- -qnorm((1-level)/2)
  cvRMSE <- min(Fit$results$RMSE)
  n <- dim(Fit$trainingData)[1]
  
  ## Add in White Correction here if time

  # Range correction is the weighted euclidian distance 
  
  # X <- as.matrix(cbind(1, predict(Fit$preProcess, newdata = fitData)))
  # XXinv <- ginv(t(X) %*% X)
  # x_p <- c(1, as.numeric(as.vector(impValues.std)) * v$importance[,1]/100)
  # rangeCorrect <- x_p %*% XXinv %*% x_p
  rangeCorrect <- 0
  
  # head(as.matrix(rangeCorrection))
  cilb <- p - z * cvRMSE * sqrt(1 + rangeCorrect)
  ciub <- p + z * cvRMSE * sqrt(1 + rangeCorrect)
  
  cat('Predicted yearly slope:\t', round(p * 12, 2), 
      ' units/yr (', round(level*100),'% CV Normal PI: [', round(cilb * 12, 2), ', ', 
      round(ciub * 12, 2), '])\n\n', sep = '')
  
  if(plotInterval) {
    xrange <- range(Fit$trainingData$.outcome * 12) * 2
    xobsrange <- c(min(Fit$trainingData$.outcome), max(Fit$trainingData$.outcome))* 12
    yrange = c(0, 6)
    py <- c(c(1,2,3,4,5)/2)
    par(mar = c(15,1,1,1))
    plot(0:5, 0:5, type = 'n', xlim = xrange, ylim = yrange,
         yaxs = 'i', yaxt = 'n', bty = 'n', ylab = '', 
         xlab = 'Predicted Slope (units/yr)')
    arrows(x0 = xobsrange[1], x1 = xobsrange[2], angle = 90,
           y0 = py[1], col = 'slategrey', length = .1, 
           code = 3, lwd = 2)
    # text(xrange[2], py[1], 'Observed Range of Outcomes', pos = 2)
    arrows(x0 = cilb * 12, x1 = ciub * 12, angle = 90,
           y0 = py[2], col = 'palegreen4', length = .1, 
           code = 3, lwd = 2)
    # text(xrange[2], py[2], 'CVnorm Prediction Interval', pos = 2)
    
    points(x = p*12, y = py[2], col = 'slateblue', pch = 18, cex = 1.7)
    
    # text(xrange[2], py[3], 'Best Model Prediction', pos = 2)
    label2 <- paste0('CVnorm ', round(level*100), '% Prediction Interval')
    legend('top', legend = c('Model Prediction', label2, 'Observed Range of Outcomes'),
           bty = 'n', pch = c(18, NA, NA), lwd = c(NA, 2,2), col = c('slateblue', 'palegreen4', 'slategrey'),
           pt.cex = 1.7)
    
    par(mar =  c(5, 4, 4, 2) + 0.1)
  }
  
  # cat('***NOTE***\n', 
  #     'The plot below does NOT provide time-specific predictions.\n', 
  #     'It is the predicted best fit line for the subject over time')
  if(length(plotAcross)) {
    if(!any(names(fitData) %in% plotAcross)) stop('PlotAcross not valid BL Value\n')
    
    # Set up Prediction X matrix to take user input
    x <- seq(min(fitData[[plotAcross]], na.rm = T), 
             max(fitData[[plotAcross]], na.rm = T), 
             length = plotXRes)
    predPlotX <- suppressWarnings(cbind(newdata[!(names(newdata) %in% plotAcross)], x))
    names(predPlotX)[names(predPlotX) == 'x'] <- plotAcross
    
    if(holdVarsConstant) {
      predPlotX <- suppressWarnings(cbind(x, impValues))
      predPlotX[[plotAcross]] <- x
      predPlotX$x <- NULL
      
    }
    
    if(!(plotAcross %in% user_inputVars)) 
      predPlotX[user_inputVars] <- newdata[user_inputVars]
    
    # Make predictions
    predPlotY <- predict(Fit, predPlotX)
    # Plot predicted y's across observed x values
    yrange <- c(min(predPlotY - z * cvRMSE)*12, max(predPlotY + z * cvRMSE) * 14)
    
    # Set correct bounds if points plotted
    if(plotObserved) {
      newyrange <- range(Fit$trainingData$.outcome) *12
      yrange <- c(min(newyrange, yrange), max(newyrange, yrange)) 
    }
    yrange <- yrange * 1.1
    
    plot(x, predPlotY * 12, type = 'l', bty = 'n',
         xlab = plotAcross, ylab = 'Predicted slope (units/yr)', 
         main = paste0(outcome,' Predictions across ', plotAcross),
         lwd = 2, ylim = yrange)
    
    # rangeCorrect <- numeric(plotXRes)
    # for(i in 1:plotXRes) {
    #   x_p <- c(1, as.numeric(as.vector(predict(Fit$preProcess, predPlotX[i,]))) * v$importance[,1]/100)
    #   rangeCorrect[i] <- x_p %*% XXinv %*% x_p
    # }
    
    # head(as.matrix(rangeCorrection))
    cilb2 <- predPlotY - z * cvRMSE * sqrt(1 + rangeCorrect)
    ciub2 <- predPlotY + z * cvRMSE * sqrt(1 + rangeCorrect)
    
    polygon(cbind(x, rev(x)), 
            cbind((ciub2) * 12, rev((cilb2) * 12)), 
            col = adjustcolor('blue', alpha.f = .2), border = 'blue')
    
    legend('topleft', legend = c('Prediction', paste0(round(100*level), '% CV Normal Prediction Interval')),
           col = c('black', 'blue'), bty = 'n', lwd = 2:1)
    
    i <- 1
    setVars <- 'Variables Set:'
    while(i < 4 & i <= length(user_inputVars)) {
      val <- ifelse(user_inputVars[i] == plotAcross, '--', user_input[[i]])
      setVars <- c(setVars, paste0(user_inputVars[i], ' = ', val))
      i <- i + 1
      if(i == 4 & i <= length(user_inputVars)) setVars <- c(setVars, '... more vars set')
    }
    
    legend('topright', legend = setVars, bty = 'n')
    
    if(holdVarsConstant) legend('bottomright', legend = 'Other variables held constant', bty = 'n')
    
    # if(compareToGLM) {
    #   
    #   xxx <- predict(Fit$preProcess, newdata = predPlotX)
    #   predPlotYGLM <- predict(rateGLM, newdata = xxx, se.fit = T, type = 'response')
    # 
    #   lines(x, predPlotYGLM$fit * 12, lwd = 2, col = 'green')
    #   
    #   polygon(cbind(x, rev(x)), 
    #           cbind((predPlotYGLM$fit + z * (predPlotYGLM$se.fit + predPlotYGLM$residual.scale)) * 12, 
    #                 rev((predPlotYGLM$fit - z * (predPlotYGLM$se.fit + predPlotYGLM$residual.scale)) * 12)), 
    #           col = adjustcolor('green', alpha.f = .2), border = 'green')
    #   
    #   legend('topleft', legend = c('Prediction', paste0(round(100*level), '% CV Asymptotic Prediction Interval'),
    #                                paste0(round(100*level), '% step-AIC-model Prediction Interval')),
    #          col = c('black', 'blue', 'green'), bty = 'n', lwd = c(2,2,1))
    #   
    #   pi.GLM.length <- round(mean((predPlotYGLM$fit + z * (predPlotYGLM$se.fit + predPlotYGLM$residual.scale)) *12  -
    #                       (predPlotYGLM$fit - z * (predPlotYGLM$se.fit + predPlotYGLM$residual.scale)) *12), 3)
    #   
    #   pi.length <- round(mean((predPlotY + z * cvRMSE) * 12  - (predPlotY - z * cvRMSE) * 12), 3)
    #   
    #   legend('bottomleft', legend = c(paste('Mean length of AIC-model PI:', pi.GLM.length), 
    #                                   paste('Mean length of final-model PI:', pi.length)),
    #          bty = 'n')
    # }
    
    # Plot observed y's across observed x values
    if(plotObserved) {
      obs <- Fit$trainingData[c(plotAcross, '.outcome')]
      obs <- na.omit(obs[order(obs[[plotAcross]]),])
      points(obs[[plotAcross]], obs$.outcome * 12, lty = 3, pch = 20, cex = .7)
    }
  }
  
  if(plotAcrossTime) {
    # Build in relative scale models if time
    t <- seq(0,60, length = 100)
    
    if(outcomeBL == 'age_at_bl') {
      outcomeForPlot <- 'nptotal'
      maxy <- with(Fit$trainingData, max(np1total + np2total + np3total, na.rm = T))
      miny <- with(Fit$trainingData, min(np1total + np2total + np3total, na.rm = T))
      baseline_outcome <- with(impValues, np1total + np2total + np3total)
    } else {
      outcomeForPlot <- outcomeBL
      maxy <- max(Fit$trainingData[outcomeForPlot], na.rm = T)
      miny <- min(Fit$trainingData[outcomeForPlot], na.rm = T)
      baseline_outcome <- impValues[[outcomeBL]]
    }
    
    yrange <- c(miny, maxy)
    
    # Makes sure the prediction is within the possible range of values
    correctPred <- function(x) {
      pmin(pmax(x, min(yrange)), max(yrange))
    }
    
    pilb <- correctPred(cilb * t + baseline_outcome)
    piub <- correctPred(ciub * t + baseline_outcome)
    
    plot(t, correctPred(p * t + baseline_outcome), ylim = yrange, 
         main = paste0('Predicted "Best Fit Line" through "', outcomeForPlot, '" Over Time'),
         bty = 'n', type = 'l', ylab = paste0(outcomeForPlot, ' (score units)'),
         xlab = 'Time (Months)', lwd = 2)
    
    if(!shiftPI) {
      polygon(cbind(t, rev(t)), 
              cbind(pilb, rev(piub)), 
              col = adjustcolor('blue', alpha.f = .2), 
              border = adjustcolor('blue', alpha.f = .2))
    } else {
      pilb <- correctPred(cilb * (t-shiftby) + baseline_outcome + shiftby * p)
      piub <- correctPred(ciub * (t-shiftby) + baseline_outcome + shiftby * p)
      polygon(cbind(t, rev(t)), 
              cbind(pilb, rev(piub)), 
              col = adjustcolor('blue', alpha.f = .2), 
              border = adjustcolor('blue', alpha.f = .2))
    }
    
    if(length(addInterceptVar)) {
      pilb <- correctPred(pilb - qnorm((1-level)/2) * addInterceptVar)
      piub <- correctPred(piub + qnorm((1-level)/2) * addInterceptVar)
      
      polygon(cbind(t, rev(t)), 
              cbind(pilb, rev(piub)), 
              col = adjustcolor('palegreen4', alpha.f = .2), 
              border = adjustcolor('palegreen4', alpha.f = .2))
      legend('bottomleft', legend = 'Including Intercept Variability', 
             col = adjustcolor('palegreen4', alpha.f = .2), lwd = 1, bty = 'n')
      
    }
    
    
    
    legend('topleft', legend = c('Prediction', paste0(round(100*level), '% CVnorm PI')),
           col = c('black', 'blue'), bty = 'n', lwd = 2:1)
    
    i <- 1
    setVars <- 'Variables Set:   '
    while(i < 4 & i <= length(user_inputVars) & !is.null(user_input)) {
      setVars <- c(setVars, paste0(user_inputVars[i], ' = ', user_input[[i]]))
      i <- i + 1
      if(i == 4 & i <= length(user_inputVars)) setVars <- c(setVars, '... more vars set')
    }
    
    legend('topright', legend = setVars, bty = 'n')
    
  }
  
  inputVars <- subset(newdata, select = !is.na(newdata))
  return(list(prediction = p, cvRMSE = cvRMSE, inputVars = inputVars, impValues = impValues))
}

diagnostics <- function(Fit, FitGLM, level = .8, QQ = F) {
  
  ## QQnorm plot ## Switched to histogram
  if(QQ) {
    sresids <- (Fit$pred$obs - Fit$pred$pred) / min(Fit$results$RMSE)
    hist(sresids, 
         main = 'Histogram of CV standardized residuals',
         xlab = 'CV Standardized Residual Value', freq = F, 
         breaks = 'scott', col = 'slategrey', border = 'white')
    abline(v = c(1,-1) * qnorm((1-level)/2), col = 'palegreen4', lty = 2)
    abline(v = quantile(sresids, c((1-level)/2,1-(1-level)/2)),
           col = 'slateblue', lty = 2)
    legend('topright', legend = c(paste(round(100 * level), '% standard norm quantiles'),
                                  paste(round(100 * level), '% empirical quantiles'),
                                  'Standard Normal Density', 'Empirical Density') , 
           bty = 'n', col = rep(c('palegreen4', 'slateblue'),2), lty = c(2,2,1,1), lwd = c(1,1,2,2))
    xseq <- seq(-5,5,length = 200)
    lines(y = dnorm(xseq),
          x = xseq, lwd = 2, col = "palegreen4") 
    
    lines(density(sresids), lwd = 2, col = 'slateblue')
    return(NULL)
  }
  
  # Use out-of-sample predictions for better diagnostics!
  
  
  par(mfrow = c(1,2))
  df <- Fit$pred[order(Fit$pred$rowIndex),]
  df2 <- data.frame(sampleno = unique(df$rowIndex))
  df2$preds <- aggregate(df$pred, by = list(df$rowIndex), FUN = mean)[,2]
  df2$obs <- aggregate(df$obs, by = list(df$rowIndex), FUN = mean)[,2]
  df2 <- df2[order(df2$preds),]
  n <- max(df2$sampleno)
  yrange <- range(df2[,2:3] * 12) *1.1
  ub <- (df2$preds - qnorm((1-level)/2) * min(Fit$results$RMSE))
  lb <- (df2$preds + qnorm((1-level)/2) * min(Fit$results$RMSE))
  plot(1:n, df2$obs * 12, ylab = 'Response Value (units/yr)', xlab = 'Sample no.', 
       pch = 20, col = 'slateblue', main = 'Coverage plot for \nCVnorm Prediction Intervals', ylim = yrange)
  points(1:n, df2$preds*12, col = 'palegreen4', pch = 20)
  points(1:n, lb * 12, col = 'palegreen4', pch = '_')
  points(1:n, ub * 12, col = 'palegreen4', pch = '_')
  legend('topleft', legend = c('Observed Outcome', 'Prediction', NA), 
         col = c('slateblue', 'palegreen4', 'palegreen4'), pch = c(20,20,NA),
         bty = 'n')
  legend('topleft', legend = c(NA, NA, paste0(round(100*level), '% CVnorm PI')), 
         col = c(NA, NA, 'palegreen4'), pch = c(NA, NA, '_'), bty = 'n')
  coverage <- mean(df2$obs <= ub & df2$obs >= lb) * 100
  legend('bottomright', legend = paste0('Coverage = ', round(coverage, 2), '%'), bty = 'n')
  
  # Use glmstepaic

  pred <- FitGLM
  residScale <- pred$residual.scale
  pred <- as.data.frame(pred[c(1,2,4)])
  pred <- pred[order(pred$fit),]

  n <- length(pred$fit)
  yrange <- range(c(pred$fit,pred$obs) * 12) *1.1
  ub <- pred$fit - (pred$se.fit + residScale) * qnorm((1-level)/2)
  lb <- pred$fit + (pred$se.fit + residScale) * qnorm((1-level)/2)
  plot(1:n, pred$obs * 12, ylab = 'Response Value (units/yr)', xlab = 'Sample no.',
       pch = 20, col = 'slateblue', main = 'Coverage plot for \nStep AIC linear model Prediction Interval', ylim = yrange)
  points(1:n, pred$fit*12, col = 'palegreen4', pch = 20)
  points(1:n, lb * 12, col = 'palegreen4', pch = '_')
  points(1:n, ub * 12, col = 'palegreen4', pch = '_')
  legend('topleft', legend = c('Observed Outcome', 'Prediction', NA),
         col = c('slateblue', 'palegreen4', 'palegreen4'), pch = c(20,20,NA),
         bty = 'n')
  legend('topleft', legend = c(NA, NA, paste0(round(100*level), '% Ordinary PI')),
         col = c(NA, NA, 'palegreen4'), pch = c(NA, NA, '_'), bty = 'n')
  coverage <- mean(pred$obs <= ub & pred$obs >= lb) * 100
  legend('bottomright', legend = paste0('Coverage = ', round(coverage, 2), '%'), bty = 'n')


  # abline(v = c(1, -1)*qnorm((1-level)/2), col = 'palegreen4', lwd = 2)
  # legend('top', legend = c(paste0(round(100*level), '% CV PI')), bty = 'n', xjust = .5)
  
  # White test
  
  fit <- lm((obs - pred)^2 ~ pred + I(pred^2), data = df, weights = rep(.2, nrow(df)))
  R2 <- summary(fit)$r.squared
  LM <- nrow(df) * R2 / 5
  p.value <- round(1 - pchisq(LM, 2),4)
  pretty.p <- ifelse(p.value == 0, '< 0.0001', paste('=', p.value))
  cat("White's test for Heteroskedasticity: p", pretty.p)
  if(p.value <=.05) cat('\nCV Normal Prediction interval width may depend on fitted value\n')
  
  # Test for normality
  standardized_residuals <- (Fit$pred$obs - Fit$pred$pred) / min(Fit$results$RMSE)
  lillie.test(standardized_residuals)
}

## Server functionality
shinyServer(function(input, output, session) {
  
  observeEvent(input$outcome, {
    updateSelectInput(session, "metric", choices = RatesFitsVars[[input$outcome]])
  })
  
  modelFit <- reactive({
    RatesFitsBest[[input$metric]]
  })
  
  AICmodelFit <- reactive({
    RatesGLM[[input$metric]]
  })
  
  modelOutcome <- reactive({
    RateOutcomes[[input$metric]]
  })
  

  impVals <- reactive({
    L = list()
    for(i in 1:input$topVars) L[[impNames()[i]]] = input[[paste0('var', i)]]
    as.data.frame(L, check.names = F)
  })
  
  # InterceptVar <- reactive({
  #   if(input$addInterceptVar) return(mean(RateOutcomes[[input$metric]]$fitRMSE[RateOutcomes[[input$metric]]$nvisits > 2]))
  #   return(NULL)
  # })
  # 
  
  output$plotSelector <- renderUI({
    selectInput('plotAcross', 'Plot predictions across...', allNames())
  })
  
  output$plotSelector2 <- renderUI({
    checkboxInput('holdVarsConstant', 'Hold other variables constant?')
  })
  
  output$plotSelector3 <- renderUI({
    checkboxInput('compareToGLM', 'Compare to OLS (StepAIC) Prediction Interval?')
  })
  
  # output$plotSlopeControl <- renderUI({
  #   checkboxInput('addInterceptVar', 'Add intercept prediction interval?')
  # })
  
  output$summary <- renderPrint({
    summary(modelOutcome())
  })
  
  output$modelInfo <- renderPrint({
    print(modelFit())
  })
  
  output$varImp <- renderPlot({
    plot(varImp(modelFit(), nonpara=FALSE),
         top = 20, main = paste0("Top 20 Baseline Factors in Predicting ", input$outcome),
         xlab = "Relative Importance (See 'Glossary' tab for more information on these factors)",
         scales = list(tck = c(1, 0)))
  }, bg="transparent")
  
  output$resPlot <- renderPlot({
    diagnostics(modelFit(), AICmodelFit(), as.numeric(input$level))
  }, bg="transparent")
  
  output$skedTest <- renderPrint({
    diagnostics(modelFit(), AICmodelFit(), as.numeric(input$level))
  })
  
  output$resPlot2 <- renderPlot({
    diagnostics(modelFit(), AICmodelFit(), as.numeric(input$level), QQ = T)
  }, bg="transparent")
  
  output$Predictions <- renderPrint({
    v = getPredictions(modelFit(), input$metric, user_input = impVals(), 
                       level = as.numeric(input$level), plotObserved = F, 
                       shiftby = mean(RateOutcomes[[input$metric]]$time_on_study))

  })

  output$plotPredictions <- renderPlot({
    try({
      getPredictions(modelFit(), input$metric, user_input = impVals(),
                     plotAcross = input$plotAcross,
                     level = as.numeric(input$level), holdVarsConstant = input$holdVarsConstant,
                     plotObserved = T, 
                     compareToGLM = input$compareToGLM, 
                     rateGLM = AICmodelFit())
    })
  }, bg="transparent")
  
  output$Imputations <- renderPrint({
    v = getPredictions(modelFit(), input$metric, user_input = impVals(),
                       plotObserved = F, printImpute = T)
  })
  
  output$plotSlope <- renderPlot({
    try({
      getPredictions(modelFit(), input$metric, user_input = impVals(),
                     plotInterval = T, level = as.numeric(input$level),
                     shiftby = mean(RateOutcomes[[input$metric]]$time_on_study, na.rm = T)/2)
    })
  }, bg="transparent")
  
  allNames <- reactive({
    v = varImp(RatesFitsBest[[input$metric]], nonpara=FALSE)
    rownames(v$importance)[order(v$importance, decreasing = T)]
  })
  
  impNames <- reactive({
    try({
      v = varImp(RatesFitsBest[[input$metric]], nonpara=FALSE)
      rownames(v$importance)[order(v$importance, decreasing = T)][1:input$topVars]
    })
  })
  
  impNamesPretty <- reactive({
    
    if(length(impNames())) {
      prettyNames <- unname(sapply(impNames(), function(x) gloss[gloss[,1] == x,3]))
    } else prettyNames <- NULL
    
    (prettyNames)  
  })
  
  output$predictionControls <- renderUI({
    
    try({
      if(length(modelFit())) {
        lapply(1:input$topVars, function(i) {
          sliderInput(inputId = paste0('var', i), 
                      label = impNamesPretty()[i], 
                      min = round(min(modelFit()$trainingData[[impNames()[i]]], na.rm = T),1), 
                      max = round(max(modelFit()$trainingData[[impNames()[i]]], na.rm = T),1), 
                      value = round(modelFit()$preProcess$mean[[impNames()[i]]], 1), 
                      sep = '')
        })
      }
    })
    
  })
  
  output$glossary <- renderDataTable({
    gloss[,2:3]
  })
  
  output$references <- renderUI({
    rrefs <- unlist(sapply(pacs, function(x) format(citation(x), "html")))
    rrefs <- rrefs[order(rrefs)]
    refs <- paste0("<p>", refs, "\n</p>")
    tags$body(
      hr(),
      p("This project is a part of a submission to",
        a("The Parkinson's Progression Markers Initiative", href = "http://www.ppmi-info.org/"),
        ", which is sponsored by",
        a("The Michael J. Fox Foundation", href = "https://www.michaeljfox.org/"),
        ". Information about the data challenge is available ",
        a("here", href = "https://www.michaeljfox.org/research/data-science.html?navid=data-science-challenge"),
        ". More information about this project available",
        a(" upon request", href = "mailto:ryan-peterson@uiowa.edu"), "."),
      hr(),
      h3("R Packages"), HTML(rrefs),
       h3("Other references"),
       HTML(refs)
     )
  })
  
})
