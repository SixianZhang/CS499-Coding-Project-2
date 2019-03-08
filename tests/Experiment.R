# This is a test script for binary data

# library(LinearModel)

data(spam, package = "ElemStatLearn")
data(SAheart, package = "ElemStatLearn")
data(zip.train, package = "ElemStatLearn")
data(prostate, package = "ElemStatLearn")
data(ozone, package = "ElemStatLearn")

data.list <- list(
  spam = list(
    features = as.matrix(spam[, 1:57]),
    labels = ifelse(spam$spam == "spam", 1, 0),
    is.01 = TRUE
  ),
  
  SAheart = list(
    features = as.matrix(SAheart[, 1:9]),
    labels = SAheart$chd,
    is.01 = TRUE
  )
  
  # zip.train = list(
  #   features = as.matrix(zip.train[, -1]),
  #   labels = zip.train[, 1],
  #   is.01 = True
  # ),
  # 
  # prostate = list(features = as.matrix(prostate[, 1:8]),
  #                 labels = prostate$lpsa),
  # 
  # ozone = list(features = a.matrix(ozone[,-1]),
  #              labels = ozone[, 1])
)

n.folds <- 4L

for (data.name in names(data.list)) {
  data.set <- data.list[[data.name]]
  test.loss.mat <- matrix(0, nrow = 4, ncol = 3)
  
  #Check data type here:
  
  set.seed(1)
  
  fold.vec <- sample(rep(1:n.folds, l = length(data.set$labels)))
  
  
  for (i.fold in (1:n.folds)) {
    train.index <- fold.vec != i.fold
    test.index <- !train.index
    
    x.train <- data.set$features[train.index, ]
    y.train <- data.set$labels[train.index]
    x.test <- data.set$feature[test.index, ]
    y.test <- data.set$labels[test.index]
    
    penalty.vec <- seq(5, 0.1, by = -0.1)
    
    if (data.set$is.01) {
      # binary data
      earlystopping.list <-
        LMLogisticLossEarlyStoppingCV(x.train, y.train, NULL, 100L, 0.5)
      L2.list <-
        LMLogisticLossL2CV(x.train,y.train, NULL, penalty.vec)
      
      earlystopping.predict <-
        ifelse(earlystopping.list$predict(x.test) > 0.5, 1, 0)
      L2.predict <- ifelse(L2.list$predict(x.test) > 0.5, 1, 0)
      baseline.predict <- ifelse(mean(y.test) > 0.5 , 1, 0)
      # baseline.predict <- mean(y.test)
      
    } else{
      # regression data
      earlystopping.list <-
        LMSquareLossEarlyStoppingCV(x.train, y.train, NULL, 15L)
      L2.list <- LMSquareLossL2CV(x.train, y.train, NULL, penalty.vec)
      
      earlystopping.predict <- earlystopping.list$predict(x.test)
      L2.predict <- L2.list$predict(x.test)
      baseline.predict <- mean(y.test)
    }
    
    # L2 loss
    earlystopping.loss <- mean((earlystopping.predict - y.test) ^ 2)
    L2.loss <- mean((L2.predict - y.test) ^ 2)
    baseline.loss <- mean((baseline.predict - y.test) ^ 2)
    
    test.loss.mat[n.folds,] = c(earlystopping.loss, L2.loss, baseline.loss)
  }
  # show result
  colnames(test.loss.mat, c("Early Stopping", "L2", "Baseline"))
  
  test.loss.mat
  
  # plot result
  barplot(
    test.loss.mat,
    main = c("Binary Classification: ", data.name),
    xlab = "mean loss value",
    legend = (rownames(test.loss.mat)),
    beside = TRUE
  )
  
  
  
}
