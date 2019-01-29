


HmL <- function(x) max(x) - min(x)

volatility <- function(x) sd(x, na.rm = T) * sqrt(52L)

TDA <- function(x) {
  diag <- TDA::gridDiag(FUNvalues = x, sublevel = F, printProgress = F)$diagram
  values <- seq(min(diag[, c("Death", "Birth")]), max(diag[, c("Death", "Birth")]), length = 50L)
  out <- c(TDA::landscape(diag, dimension = 0L, KK = 2L, tseq = values)[, 1L],
           TDA::landscape(diag, dimension = 0L, KK = 3L, tseq = values)[, 1L])
  
  magrittr::set_names(as.list(out), paste("TDA", 1L:100L))
}


tune <- function(data){
  
  x <- apply(dplyr::select(data, -class), as.numeric, MARGIN = 2L); y <- data$class
  
  tune_control <- caret::trainControl(method = "cv", number = 3L, verboseIter = F, allowParallel = T)
  tune_grid <- expand.grid(nrounds = seq(from = 200L, to = 1000L, by = 50L),
                           eta = c(0.025, 0.05, 0.1, 0.3), max_depth = c(2L, 3L, 4L, 5L, 6L),
                           gamma = 0L, colsample_bytree = 1L, min_child_weight = 1L, subsample = 1L)
  xgb_tune <- caret::train(x = x, y = y, trControl = tune_control, tuneGrid = tune_grid, method = "xgbTree", verbose = T)
  
  tune_grid <- expand.grid(nrounds = seq(from = 50L, to = 1000L, by = 50L), eta = xgb_tune$bestTune$eta,
                           max_depth = ifelse(xgb_tune$bestTune$max_depth == 2L, c(xgb_tune$bestTune$max_depth:4),
                                              xgb_tune$bestTune$max_depth - 1L:xgb_tune$bestTune$max_depth + 1L),
                           gamma = 0L, colsample_bytree = 1L, min_child_weight = c(1L, 2L, 3L), subsample = 1L)
  xgb_tune <- caret::train(x = x, y = y, trControl = tune_control, tuneGrid = tune_grid, method = "xgbTree", verbose = T)
  
  tune_grid <- expand.grid(nrounds = seq(from = 50L, to = 1000L, by = 50L), eta = xgb_tune$bestTune$eta,
                           max_depth = xgb_tune$bestTune$max_depth, gamma = 0, colsample_bytree = c(0.4, 0.6, 0.8, 1.0),
                           min_child_weight = xgb_tune$bestTune$min_child_weight, subsample = c(0.5, 0.75, 1.0))
  xgb_tune <- caret::train(x = x, y = y, trControl = tune_control, tuneGrid = tune_grid, method = "xgbTree", verbose = T)
  
  tune_grid <- expand.grid(nrounds = seq(from = 50L, to = 1000L, by = 50L),eta = xgb_tune$bestTune$eta,
                           max_depth = xgb_tune$bestTune$max_depth, gamma = c(0, 0.05, 0.1, 0.5, 0.7, 0.9, 1.0),
                           colsample_bytree = xgb_tune$bestTune$colsample_bytree, 
                           min_child_weight = xgb_tune$bestTune$min_child_weight,
                           subsample = xgb_tune$bestTune$subsample)
  xgb_tune <- caret::train(x = x, y = y, trControl = tune_control, tuneGrid = tune_grid, method = "xgbTree", verbose = T)
  
  tune_grid <- expand.grid(nrounds = seq(from = 100L, to = 10000L, by = 100L), eta = c(0.01, 0.015, 0.025, 0.05, 0.1),
                           max_depth = xgb_tune$bestTune$max_depth, gamma = xgb_tune$bestTune$gamma, 
                           colsample_bytree = xgb_tune$bestTune$colsample_bytree, 
                           min_child_weight = xgb_tune$bestTune$min_child_weight, subsample = xgb_tune$bestTune$subsample)
  xgb_tune <- caret::train(x = x, y = y, trControl = tune_control, tuneGrid = tune_grid, method = "xgbTree", verbose = T)
  
  xgb_tune
}



tuneplot <- function(x, probs = .90)  
  ggplot(x) + coord_cartesian(ylim = c(quantile(x$results$RMSE, probs = probs), min(x$results$RMSE))) +
  theme_bw()

