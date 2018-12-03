
prices <-read.csv("data/001_data_prices_20072007_18072008.csv")

num_row <- dim(prices)[2]


matrix_TDA_features <- matrix(, nrow = num_row, ncol = 101L)

matrix_TDA_features[, 1L] <- colnames(prices)[1L:num_row]

i <- 1L
for(i in 1:num_row){
  X <- prices[colnames(prices)[i]]
  Diag <- TDA::gridDiag(FUNvalues = X ,sublevel = F, printProgress = F)
  Lmin <- min(Diag[['diagram']][, 2L:3L])
  Lmax <- max(Diag[['diagram']][, 2L:3L])
  tseq <- seq(Lmin, Lmax, length = 50L)
  L2 <- TDA::landscape(Diag[["diagram"]], dimension = 0L, KK = 2L, tseq = tseq)
  matrix_TDA_features[i, 2L:51L] <- L2
  L3 <- TDA::landscape(Diag[["diagram"]],dimension = 0L, KK = 3L, tseq = tseq)
  matrix_TDA_features[i, 52L:101L] <- L3
}
return(matrix_TDA_features)
#  write.csv(matrix_TDA_features,f_out)





















```{r TDA, message = F, warning = F}

diagram <- TDA::gridDiag(FUNvalues = mtcars["mpg"], sublevel = F, printProgress = F)$`diagram`

values <- seq(min(diagram[, c("Death", "Birth")]), max(diagram[, c("Death", "Birth")]), length = 50)

TDA::landscape(diagram, dimension = 0L, KK = 2L, tseq = values)

persistence <- lapply(2L:3L, function(x) TDA::landscape(diagram, dimension = 0L, KK = x, tseq = values)) %>%
  
  
  ```




