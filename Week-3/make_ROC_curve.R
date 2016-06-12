make_ROC <- function(res = res_LM){
  
  ################
  #  predict probabilities
  trainPred <- predict(res$model, res$train, type = "prob")
  
  ################
  #   make ROC object
  library(pROC)
  rocCurve   <- roc(response = res$train$outcome,
                    predictor = trainPred[, "yes"])
  
  ####  optional - use plot from pRoc package to plot rocCurve
  # plot(rocCurve)
  # plot(rocCurve, print.thres = "best") # shows "best" cut-off value for p and sens-spec coordinates in parentecies
  # best means highest sum sensitivity + specificity - see ?plot.roc
  
  names(rocCurve)
  rocCurve$thresholds
  rocCurve$sensitiviti
  
  ###############
  #  cut-off
  #  plot and choose threshold based on desired sens or speci
  library(dplyr); library(tidyr)
  
  resDF <- data.frame(pr_threshold = rocCurve$thresholds, 
                      sensitivity = rocCurve$sensitiviti, 
                      specificity = rocCurve$specificiti)
  
  ##############
  #  Plot for ROC curve
  p_roc <- ggplot(data=resDF, aes(x=sensitivity, y = specificity)) +
    #geom_point() + 
    geom_line() +
    #reference line
    geom_line(data = data.frame(sensitivity = 0:1, specificity = 1:0), linetype = "dashed") +
    xlab("Sensitivity (true positive rate, recall) \n (% of adopted identified as adopted)") +
    ylab("Specificity (true negative rate)\n (% of not-adopted identified as not-adopted)")
  
  #############
  #  Plot for spec and sens vs threshold
  resDF <- 
    resDF %>% 
    filter(! is.infinite(pr_threshold)) %>%
    gather(metrics, value, -pr_threshold)
   
  p <- ggplot(data=resDF, aes(x=pr_threshold, y = value, col = metrics)) +
    geom_point() + geom_line()
  
  # use plotly for interactive graph  
  
  ########### Determine a "best" cut-offs
  resDF_best <- resDF %>% 
    group_by(pr_threshold) %>%
    mutate(sumSS = sum(value)) %>%
    ungroup() %>%
    top_n(1, sumSS)
  
  print(resDF_best)
  best.cut.off <- resDF_best$pr_threshold[1]
  
  ###############
  #  add "best" dot on p_roc plot
  p_roc <- p_roc + geom_point(data = spread(resDF_best, metrics, value), col = 2, size = 2) +
    annotate("text", x = resDF_best$value[1] + 0.05, y = resDF_best$value[2]+ .05,
             label = round(resDF_best$pr_threshold[1], digits = 2), col = 2)
    
  ##############
  #  plot 2 graphs on the same page
  library(ggplot2); library(grid); library(gridExtra)
  grid.arrange(p_roc, p, ncol=2, nrow =1)
  
  #############
  #  possible, but not recomended.
  #best.cut.off <<- best.cut.off
  #trainPred <<- trainPred
  
  #############
  #  return object
  list(best.cut.off = best.cut.off, trainPred = trainPred)
}