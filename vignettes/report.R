## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ------------------------------------------------------------------------
## Source Code:
library(LinearModel)

data(spam, package = "ElemStatLearn")
data(SAheart, package = "ElemStatLearn")
data(zip.train, package = "ElemStatLearn")
zip.train <- zip.train[zip.train[,1] %in% c(0,1),]
data(prostate, package = "ElemStatLearn")
data(ozone, package = "ElemStatLearn")

data.list <- list(
  spam = list(
    features = as.matrix(spam[, 1:57]),
    labels = ifelse(spam$spam == "spam", 1, 0),
    is.01 = TRUE
  ),

  SAheart = list(
    features = as.matrix(SAheart[, c(1:4,6:9)]),
    labels = SAheart$chd,
    is.01 = TRUE
  ),

  zip.train = list(
    features = as.matrix(zip.train[, -1]),
    labels = zip.train[, 1],
    is.01 = TRUE
  ),

  prostate = list(features = as.matrix(prostate[, 1:8]),
                  labels = prostate$lpsa,
                  is.01 = FALSE),

  ozone = list(features = as.matrix(ozone[,-1]),
               labels = ozone[, 1],
               is.01 = FALSE)
)

n.folds <- 4L

## ------------------------------------------------------------------------
#Spam
data.name  = 1
data.set <- data.list[[data.name]]
test.loss.mat <- matrix(0, nrow = 4, ncol = 3)

#Check data type here:
set.seed(2)

fold.vec <- sample(rep(1:n.folds, l = length(data.set$labels)))

penalty.vec <- seq(5, 0.1, by = -0.1)

for (i.fold in (1:n.folds)) {
  train.index <- fold.vec != i.fold
  test.index <- !train.index

  x.train <- data.set$features[train.index, ]
  y.train <- data.set$labels[train.index]
  x.test <- data.set$feature[test.index, ]
  y.test <- data.set$labels[test.index]

  if (data.set$is.01) {
    # binary data
    earlystopping.list <-
      LMLogisticLossEarlyStoppingCV(x.train, y.train, NULL, 100L, 0.5)
    L2.list <-
      LMLogisticLossL2CV(x.train, y.train, NULL, penalty.vec)

    earlystopping.predict <-
      ifelse(earlystopping.list$predict(x.test) > 0.5, 1, 0)
    L2.predict <- ifelse(L2.list$predict(x.test) > 0.5, 1, 0)
    baseline.predict <- ifelse(mean(y.test) > 0.5 , 1, 0)
    # baseline.predict <- mean(y.test)

  } else{
    # regression data
    earlystopping.list <-
      LMSquareLossEarlyStoppingCV(x.train, y.train, NULL, 50L)
    L2.list <- LMSquareLossL2CV(x.train, y.train, NULL, penalty.vec)

    earlystopping.predict <- earlystopping.list$predict(x.test)
    L2.predict <- L2.list$predict(x.test)
    baseline.predict <- mean(y.test)
  }

  # L2 loss
  earlystopping.loss <- mean((earlystopping.predict - y.test) ^ 2)
  L2.loss <- mean((L2.predict - y.test) ^ 2)
  baseline.loss <- mean((baseline.predict - y.test) ^ 2)

  test.loss.mat[i.fold,] = c(earlystopping.loss, L2.loss, baseline.loss)
}

## ---- fig.show='hold', fig.width= 6, fig.height= 6-----------------------
# show result
colnames(test.loss.mat) <- c("Early Stopping", "L2", "Baseline")

test.loss.mat

# plot result
barplot(
  test.loss.mat,
  main = c("Binary Classification: ", "spam"),
  xlab = "mean loss value",
  legend = (rownames(test.loss.mat)),
  beside = TRUE
)

## ---- fig.show='hold', fig.width= 6, fig.height= 6-----------------------
# Run CV for whole dataset
if(data.set$is.01){
  # Binary
  model.list <- LMLogisticLossL2CV(data.set$features, data.set$labels, NULL, penalty.vec)
}else{
  # Regression
  model.list <- LMSquareLossL2CV(data.set$features,data.set$labels,NULL, penalty.vec)
}

dot.x <- model.list$selected.penalty
dot.y <- model.list$mean.validation.loss.vec[penalty.vec == model.list$selected.penalty]

matplot(
  y = cbind(model.list$mean.validation.loss.vec, model.list$mean.train.loss.vec),
  x = as.matrix(penalty.vec),
  xlab = "penalty",
  ylab = "mean loss value",
  type = "l",
  lty = 1:2,
  pch = 15,
  col = c(17)
)

matpoints(x = dot.x,
          y = dot.y,
          col = 2,
          pch = 19)
legend(
  x = length(penalty.vec),
  0,
  c("Validation loss", "Train loss"),
  lty = 1:2,
  xjust = 1,
  yjust = 0
)

## ------------------------------------------------------------------------
#SAheart
data.name  = 2
data.set <- data.list[[data.name]]
test.loss.mat <- matrix(0, nrow = 4, ncol = 3)

#Check data type here:
set.seed(2)

fold.vec <- sample(rep(1:n.folds, l = length(data.set$labels)))

penalty.vec <- seq(5, 0.1, by = -0.1)

for (i.fold in (1:n.folds)) {
  train.index <- fold.vec != i.fold
  test.index <- !train.index

  x.train <- data.set$features[train.index, ]
  y.train <- data.set$labels[train.index]
  x.test <- data.set$feature[test.index, ]
  y.test <- data.set$labels[test.index]

  if (data.set$is.01) {
    # binary data
    earlystopping.list <-
      LMLogisticLossEarlyStoppingCV(x.train, y.train, NULL, 100L, 0.5)
    L2.list <-
      LMLogisticLossL2CV(x.train, y.train, NULL, penalty.vec)

    earlystopping.predict <-
      ifelse(earlystopping.list$predict(x.test) > 0.5, 1, 0)
    L2.predict <- ifelse(L2.list$predict(x.test) > 0.5, 1, 0)
    baseline.predict <- ifelse(mean(y.test) > 0.5 , 1, 0)
    # baseline.predict <- mean(y.test)

  } else{
    # regression data
    earlystopping.list <-
      LMSquareLossEarlyStoppingCV(x.train, y.train, NULL, 50L)
    L2.list <- LMSquareLossL2CV(x.train, y.train, NULL, penalty.vec)

    earlystopping.predict <- earlystopping.list$predict(x.test)
    L2.predict <- L2.list$predict(x.test)
    baseline.predict <- mean(y.test)
  }

  # L2 loss
  earlystopping.loss <- mean((earlystopping.predict - y.test) ^ 2)
  L2.loss <- mean((L2.predict - y.test) ^ 2)
  baseline.loss <- mean((baseline.predict - y.test) ^ 2)

  test.loss.mat[i.fold,] = c(earlystopping.loss, L2.loss, baseline.loss)
}

## ---- fig.show='hold', fig.width= 6, fig.height= 6-----------------------
# show result
colnames(test.loss.mat) <- c("Early Stopping", "L2", "Baseline")

test.loss.mat

# plot result
barplot(
  test.loss.mat,
  main = c("Binary Classification: ", "SAheart"),
  xlab = "mean loss value",
  legend = (rownames(test.loss.mat)),
  beside = TRUE
)

## ---- fig.show='hold', fig.width= 6, fig.height= 6-----------------------
# Run CV for whole dataset
if(data.set$is.01){
  # Binary
  model.list <- LMLogisticLossL2CV(data.set$features, data.set$labels, NULL, penalty.vec)
}else{
  # Regression
  model.list <- LMSquareLossL2CV(data.set$features,data.set$labels,NULL, penalty.vec)
}

dot.x <- model.list$selected.penalty
dot.y <- model.list$mean.validation.loss.vec[penalty.vec == model.list$selected.penalty]

matplot(
  y = cbind(model.list$mean.validation.loss.vec, model.list$mean.train.loss.vec),
  x = as.matrix(penalty.vec),
  xlab = "penalty",
  ylab = "mean loss value",
  type = "l",
  lty = 1:2,
  pch = 15,
  col = c(17)
)

matpoints(x = dot.x,
          y = dot.y,
          col = 2,
          pch = 19)
legend(
  x = length(penalty.vec),
  0,
  c("Validation loss", "Train loss"),
  lty = 1:2,
  xjust = 1,
  yjust = 0
)

<<<<<<< HEAD
## ------------------------------------------------------------------------
#zip.train
data.name  = 3
data.set <- data.list[[data.name]]
test.loss.mat <- matrix(0, nrow = 4, ncol = 3)

#Check data type here:
set.seed(2)

fold.vec <- sample(rep(1:n.folds, l = length(data.set$labels)))

penalty.vec <- seq(5, 0.1, by = -0.1)

for (i.fold in (1:n.folds)) {
  train.index <- fold.vec != i.fold
  test.index <- !train.index

  x.train <- data.set$features[train.index, ]
  y.train <- data.set$labels[train.index]
  x.test <- data.set$feature[test.index, ]
  y.test <- data.set$labels[test.index]

  if (data.set$is.01) {
    # binary data
    earlystopping.list <-
      LMLogisticLossEarlyStoppingCV(x.train, y.train, NULL, 100L, 0.5)
    L2.list <-
      LMLogisticLossL2CV(x.train, y.train, NULL, penalty.vec)

    earlystopping.predict <-
      ifelse(earlystopping.list$predict(x.test) > 0.5, 1, 0)
    L2.predict <- ifelse(L2.list$predict(x.test) > 0.5, 1, 0)
    baseline.predict <- ifelse(mean(y.test) > 0.5 , 1, 0)
    # baseline.predict <- mean(y.test)

  } else{
    # regression data
    earlystopping.list <-
      LMSquareLossEarlyStoppingCV(x.train, y.train, NULL, 50L)
    L2.list <- LMSquareLossL2CV(x.train, y.train, NULL, penalty.vec)

    earlystopping.predict <- earlystopping.list$predict(x.test)
    L2.predict <- L2.list$predict(x.test)
    baseline.predict <- mean(y.test)
  }

  # L2 loss
  earlystopping.loss <- mean((earlystopping.predict - y.test) ^ 2)
  L2.loss <- mean((L2.predict - y.test) ^ 2)
  baseline.loss <- mean((baseline.predict - y.test) ^ 2)

  test.loss.mat[i.fold,] = c(earlystopping.loss, L2.loss, baseline.loss)
}

## ---- fig.show='hold', fig.width= 6, fig.height= 6-----------------------
# show result
colnames(test.loss.mat) <- c("Early Stopping", "L2", "Baseline")

test.loss.mat

# plot result
barplot(
  test.loss.mat,
  main = c("Binary Classification: ", "zip.train"),
  xlab = "mean loss value",
  legend = (rownames(test.loss.mat)),
  beside = TRUE
)

## ---- fig.show='hold', fig.width= 6, fig.height= 6-----------------------
# Run CV for whole dataset
if(data.set$is.01){
  # Binary
  model.list <- LMLogisticLossL2CV(data.set$features, data.set$labels, NULL, penalty.vec)
}else{
  # Regression
  model.list <- LMSquareLossL2CV(data.set$features,data.set$labels,NULL, penalty.vec)
}

dot.x <- model.list$selected.penalty
dot.y <- model.list$mean.validation.loss.vec[penalty.vec == model.list$selected.penalty]

matplot(
  y = cbind(model.list$mean.validation.loss.vec, model.list$mean.train.loss.vec),
  x = as.matrix(penalty.vec),
  xlab = "penalty",
  ylab = "mean loss value",
  type = "l",
  lty = 1:2,
  pch = 15,
  col = c(17)
)

matpoints(x = dot.x,
          y = dot.y,
          col = 2,
          pch = 19)
legend(
  x = length(penalty.vec),
  0,
  c("Validation loss", "Train loss"),
  lty = 1:2,
  xjust = 1,
  yjust = 0
)

## ------------------------------------------------------------------------
#prostate
data.name  = 4
data.set <- data.list[[data.name]]
test.loss.mat <- matrix(0, nrow = 4, ncol = 3)

#Check data type here:
set.seed(2)

fold.vec <- sample(rep(1:n.folds, l = length(data.set$labels)))

penalty.vec <- seq(5, 0.1, by = -0.1)

for (i.fold in (1:n.folds)) {
  train.index <- fold.vec != i.fold
  test.index <- !train.index

  x.train <- data.set$features[train.index, ]
  y.train <- data.set$labels[train.index]
  x.test <- data.set$feature[test.index, ]
  y.test <- data.set$labels[test.index]

  if (data.set$is.01) {
    # binary data
    earlystopping.list <-
      LMLogisticLossEarlyStoppingCV(x.train, y.train, NULL, 100L, 0.5)
    L2.list <-
      LMLogisticLossL2CV(x.train, y.train, NULL, penalty.vec)

    earlystopping.predict <-
      ifelse(earlystopping.list$predict(x.test) > 0.5, 1, 0)
    L2.predict <- ifelse(L2.list$predict(x.test) > 0.5, 1, 0)
    baseline.predict <- ifelse(mean(y.test) > 0.5 , 1, 0)
    # baseline.predict <- mean(y.test)

  } else{
    # regression data
    earlystopping.list <-
      LMSquareLossEarlyStoppingCV(x.train, y.train, NULL, 50L)
    L2.list <- LMSquareLossL2CV(x.train, y.train, NULL, penalty.vec)

    earlystopping.predict <- earlystopping.list$predict(x.test)
    L2.predict <- L2.list$predict(x.test)
    baseline.predict <- mean(y.test)
  }

  # L2 loss
  earlystopping.loss <- mean((earlystopping.predict - y.test) ^ 2)
  L2.loss <- mean((L2.predict - y.test) ^ 2)
  baseline.loss <- mean((baseline.predict - y.test) ^ 2)

  test.loss.mat[i.fold,] = c(earlystopping.loss, L2.loss, baseline.loss)
}

## ---- fig.show='hold', fig.width= 6, fig.height= 6-----------------------
# show result
colnames(test.loss.mat) <- c("Early Stopping", "L2", "Baseline")

test.loss.mat

# plot result
barplot(
  test.loss.mat,
  main = c("Binary Classification: ", "prostate"),
  xlab = "mean loss value",
  legend = (rownames(test.loss.mat)),
  beside = TRUE
)

## ---- fig.show='hold', fig.width= 6, fig.height= 6-----------------------
# Run CV for whole dataset
if(data.set$is.01){
  # Binary
  model.list <- LMLogisticLossL2CV(data.set$features, data.set$labels, NULL, penalty.vec)
}else{
  # Regression
  model.list <- LMSquareLossL2CV(data.set$features,data.set$labels,NULL, penalty.vec)
}

dot.x <- model.list$selected.penalty
dot.y <- model.list$mean.validation.loss.vec[penalty.vec == model.list$selected.penalty]

matplot(
  y = cbind(model.list$mean.validation.loss.vec, model.list$mean.train.loss.vec),
  x = as.matrix(penalty.vec),
  xlab = "penalty",
  ylab = "mean loss value",
  type = "l",
  lty = 1:2,
  pch = 15,
  col = c(17)
)

matpoints(x = dot.x,
          y = dot.y,
          col = 2,
          pch = 19)
legend(
  x = length(penalty.vec),
  0,
  c("Validation loss", "Train loss"),
  lty = 1:2,
  xjust = 1,
  yjust = 0
)

## ------------------------------------------------------------------------
#ozone
data.name  = 5
data.set <- data.list[[data.name]]
test.loss.mat <- matrix(0, nrow = 4, ncol = 3)

#Check data type here:
set.seed(2)

fold.vec <- sample(rep(1:n.folds, l = length(data.set$labels)))

penalty.vec <- seq(5, 0.1, by = -0.1)

for (i.fold in (1:n.folds)) {
  train.index <- fold.vec != i.fold
  test.index <- !train.index

  x.train <- data.set$features[train.index, ]
  y.train <- data.set$labels[train.index]
  x.test <- data.set$feature[test.index, ]
  y.test <- data.set$labels[test.index]

  if (data.set$is.01) {
    # binary data
    earlystopping.list <-
      LMLogisticLossEarlyStoppingCV(x.train, y.train, NULL, 100L, 0.5)
    L2.list <-
      LMLogisticLossL2CV(x.train, y.train, NULL, penalty.vec)

    earlystopping.predict <-
      ifelse(earlystopping.list$predict(x.test) > 0.5, 1, 0)
    L2.predict <- ifelse(L2.list$predict(x.test) > 0.5, 1, 0)
    baseline.predict <- ifelse(mean(y.test) > 0.5 , 1, 0)
    # baseline.predict <- mean(y.test)

  } else{
    # regression data
    earlystopping.list <-
      LMSquareLossEarlyStoppingCV(x.train, y.train, NULL, 50L)
    L2.list <- LMSquareLossL2CV(x.train, y.train, NULL, penalty.vec)

    earlystopping.predict <- earlystopping.list$predict(x.test)
    L2.predict <- L2.list$predict(x.test)
    baseline.predict <- mean(y.test)
  }

  # L2 loss
  earlystopping.loss <- mean((earlystopping.predict - y.test) ^ 2)
  L2.loss <- mean((L2.predict - y.test) ^ 2)
  baseline.loss <- mean((baseline.predict - y.test) ^ 2)

  test.loss.mat[i.fold,] = c(earlystopping.loss, L2.loss, baseline.loss)
}

## ---- fig.show='hold', fig.width= 6, fig.height= 6-----------------------
# show result
colnames(test.loss.mat) <- c("Early Stopping", "L2", "Baseline")

test.loss.mat

# plot result
barplot(
  test.loss.mat,
  main = c("Binary Classification: ", "ozone"),
  xlab = "mean loss value",
  legend = (rownames(test.loss.mat)),
  beside = TRUE
)

## ---- fig.show='hold', fig.width= 6, fig.height= 6-----------------------
# Run CV for whole dataset
if(data.set$is.01){
  # Binary
  model.list <- LMLogisticLossL2CV(data.set$features, data.set$labels, NULL, penalty.vec)
}else{
  # Regression
  model.list <- LMSquareLossL2CV(data.set$features,data.set$labels,NULL, penalty.vec)
}

dot.x <- model.list$selected.penalty
dot.y <- model.list$mean.validation.loss.vec[penalty.vec == model.list$selected.penalty]

matplot(
  y = cbind(model.list$mean.validation.loss.vec, model.list$mean.train.loss.vec),
  x = as.matrix(penalty.vec),
  xlab = "penalty",
  ylab = "mean loss value",
  type = "l",
  lty = 1:2,
  pch = 15,
  col = c(17)
)

matpoints(x = dot.x,
          y = dot.y,
          col = 2,
          pch = 19)
legend(
  x = length(penalty.vec),
  0,
  c("Validation loss", "Train loss"),
  lty = 1:2,
  xjust = 1,
  yjust = 0
)

=======
>>>>>>> de06fb0d8efd2fa04e7364f1aa60fa7921db87fc
## ------------------------------------------------------------------------
#zip.train
data.name  = 3
data.set <- data.list[[data.name]]
test.loss.mat <- matrix(0, nrow = 4, ncol = 3)

#Check data type here:
set.seed(2)

fold.vec <- sample(rep(1:n.folds, l = length(data.set$labels)))

penalty.vec <- seq(5, 0.1, by = -0.1)

for (i.fold in (1:n.folds)) {
  train.index <- fold.vec != i.fold
  test.index <- !train.index

  x.train <- data.set$features[train.index, ]
  y.train <- data.set$labels[train.index]
  x.test <- data.set$feature[test.index, ]
  y.test <- data.set$labels[test.index]

  if (data.set$is.01) {
    # binary data
    earlystopping.list <-
      LMLogisticLossEarlyStoppingCV(x.train, y.train, NULL, 100L, 0.5)
    L2.list <-
      LMLogisticLossL2CV(x.train, y.train, NULL, penalty.vec)

    earlystopping.predict <-
      ifelse(earlystopping.list$predict(x.test) > 0.5, 1, 0)
    L2.predict <- ifelse(L2.list$predict(x.test) > 0.5, 1, 0)
    baseline.predict <- ifelse(mean(y.test) > 0.5 , 1, 0)
    # baseline.predict <- mean(y.test)

  } else{
    # regression data
    earlystopping.list <-
      LMSquareLossEarlyStoppingCV(x.train, y.train, NULL, 50L)
    L2.list <- LMSquareLossL2CV(x.train, y.train, NULL, penalty.vec)

    earlystopping.predict <- earlystopping.list$predict(x.test)
    L2.predict <- L2.list$predict(x.test)
    baseline.predict <- mean(y.test)
  }

  # L2 loss
  earlystopping.loss <- mean((earlystopping.predict - y.test) ^ 2)
  L2.loss <- mean((L2.predict - y.test) ^ 2)
  baseline.loss <- mean((baseline.predict - y.test) ^ 2)

  test.loss.mat[i.fold,] = c(earlystopping.loss, L2.loss, baseline.loss)
}

## ---- fig.show='hold', fig.width= 6, fig.height= 6-----------------------
# show result
colnames(test.loss.mat) <- c("Early Stopping", "L2", "Baseline")

test.loss.mat

# plot result
barplot(
  test.loss.mat,
  main = c("Binary Classification: ", "zip.train"),
  xlab = "mean loss value",
  legend = (rownames(test.loss.mat)),
  beside = TRUE
)

## ---- fig.show='hold', fig.width= 6, fig.height= 6-----------------------
# Run CV for whole dataset
if(data.set$is.01){
  # Binary
  model.list <- LMLogisticLossL2CV(data.set$features, data.set$labels, NULL, penalty.vec)
}else{
  # Regression
  model.list <- LMSquareLossL2CV(data.set$features,data.set$labels,NULL, penalty.vec)
}

dot.x <- model.list$selected.penalty
dot.y <- model.list$mean.validation.loss.vec[penalty.vec == model.list$selected.penalty]

matplot(
  y = cbind(model.list$mean.validation.loss.vec, model.list$mean.train.loss.vec),
  x = as.matrix(penalty.vec),
  xlab = "penalty",
  ylab = "mean loss value",
  type = "l",
  lty = 1:2,
  pch = 15,
  col = c(17)
)

matpoints(x = dot.x,
          y = dot.y,
          col = 2,
          pch = 19)
legend(
  x = length(penalty.vec),
  0,
  c("Validation loss", "Train loss"),
  lty = 1:2,
  xjust = 1,
  yjust = 0
)

## ------------------------------------------------------------------------
#prostate
data.name  = 4
data.set <- data.list[[data.name]]
test.loss.mat <- matrix(0, nrow = 4, ncol = 3)

#Check data type here:
set.seed(2)

fold.vec <- sample(rep(1:n.folds, l = length(data.set$labels)))

penalty.vec <- seq(5, 0.1, by = -0.1)

for (i.fold in (1:n.folds)) {
  train.index <- fold.vec != i.fold
  test.index <- !train.index

  x.train <- data.set$features[train.index, ]
  y.train <- data.set$labels[train.index]
  x.test <- data.set$feature[test.index, ]
  y.test <- data.set$labels[test.index]

  if (data.set$is.01) {
    # binary data
    earlystopping.list <-
      LMLogisticLossEarlyStoppingCV(x.train, y.train, NULL, 100L, 0.5)
    L2.list <-
      LMLogisticLossL2CV(x.train, y.train, NULL, penalty.vec)

    earlystopping.predict <-
      ifelse(earlystopping.list$predict(x.test) > 0.5, 1, 0)
    L2.predict <- ifelse(L2.list$predict(x.test) > 0.5, 1, 0)
    baseline.predict <- ifelse(mean(y.test) > 0.5 , 1, 0)
    # baseline.predict <- mean(y.test)

  } else{
    # regression data
    earlystopping.list <-
      LMSquareLossEarlyStoppingCV(x.train, y.train, NULL, 50L)
    L2.list <- LMSquareLossL2CV(x.train, y.train, NULL, penalty.vec)

    earlystopping.predict <- earlystopping.list$predict(x.test)
    L2.predict <- L2.list$predict(x.test)
    baseline.predict <- mean(y.test)
  }

  # L2 loss
  earlystopping.loss <- mean((earlystopping.predict - y.test) ^ 2)
  L2.loss <- mean((L2.predict - y.test) ^ 2)
  baseline.loss <- mean((baseline.predict - y.test) ^ 2)

  test.loss.mat[i.fold,] = c(earlystopping.loss, L2.loss, baseline.loss)
}

## ---- fig.show='hold', fig.width= 6, fig.height= 6-----------------------
# show result
colnames(test.loss.mat) <- c("Early Stopping", "L2", "Baseline")

test.loss.mat

# plot result
barplot(
  test.loss.mat,
  main = c("Binary Classification: ", "prostate"),
  xlab = "mean loss value",
  legend = (rownames(test.loss.mat)),
  beside = TRUE
)

## ---- fig.show='hold', fig.width= 6, fig.height= 6-----------------------
# Run CV for whole dataset
if(data.set$is.01){
  # Binary
  model.list <- LMLogisticLossL2CV(data.set$features, data.set$labels, NULL, penalty.vec)
}else{
  # Regression
  model.list <- LMSquareLossL2CV(data.set$features,data.set$labels,NULL, penalty.vec)
}

dot.x <- model.list$selected.penalty
dot.y <- model.list$mean.validation.loss.vec[penalty.vec == model.list$selected.penalty]

matplot(
  y = cbind(model.list$mean.validation.loss.vec, model.list$mean.train.loss.vec),
  x = as.matrix(penalty.vec),
  xlab = "penalty",
  ylab = "mean loss value",
  type = "l",
  lty = 1:2,
  pch = 15,
  col = c(17)
)

matpoints(x = dot.x,
          y = dot.y,
          col = 2,
          pch = 19)
legend(
  x = length(penalty.vec),
  0,
  c("Validation loss", "Train loss"),
  lty = 1:2,
  xjust = 1,
  yjust = 0
)

## ------------------------------------------------------------------------
#ozone
data.name  = 5
data.set <- data.list[[data.name]]
test.loss.mat <- matrix(0, nrow = 4, ncol = 3)

#Check data type here:
set.seed(2)

fold.vec <- sample(rep(1:n.folds, l = length(data.set$labels)))

penalty.vec <- seq(5, 0.1, by = -0.1)

for (i.fold in (1:n.folds)) {
  train.index <- fold.vec != i.fold
  test.index <- !train.index

  x.train <- data.set$features[train.index, ]
  y.train <- data.set$labels[train.index]
  x.test <- data.set$feature[test.index, ]
  y.test <- data.set$labels[test.index]

  if (data.set$is.01) {
    # binary data
    earlystopping.list <-
      LMLogisticLossEarlyStoppingCV(x.train, y.train, NULL, 100L, 0.5)
    L2.list <-
      LMLogisticLossL2CV(x.train, y.train, NULL, penalty.vec)

    earlystopping.predict <-
      ifelse(earlystopping.list$predict(x.test) > 0.5, 1, 0)
    L2.predict <- ifelse(L2.list$predict(x.test) > 0.5, 1, 0)
    baseline.predict <- ifelse(mean(y.test) > 0.5 , 1, 0)
    # baseline.predict <- mean(y.test)

  } else{
    # regression data
    earlystopping.list <-
      LMSquareLossEarlyStoppingCV(x.train, y.train, NULL, 50L)
    L2.list <- LMSquareLossL2CV(x.train, y.train, NULL, penalty.vec)

    earlystopping.predict <- earlystopping.list$predict(x.test)
    L2.predict <- L2.list$predict(x.test)
    baseline.predict <- mean(y.test)
  }

  # L2 loss
  earlystopping.loss <- mean((earlystopping.predict - y.test) ^ 2)
  L2.loss <- mean((L2.predict - y.test) ^ 2)
  baseline.loss <- mean((baseline.predict - y.test) ^ 2)

  test.loss.mat[i.fold,] = c(earlystopping.loss, L2.loss, baseline.loss)
}

## ---- fig.show='hold', fig.width= 6, fig.height= 6-----------------------
# show result
colnames(test.loss.mat) <- c("Early Stopping", "L2", "Baseline")

test.loss.mat

# plot result
barplot(
  test.loss.mat,
  main = c("Binary Classification: ", "ozone"),
  xlab = "mean loss value",
  legend = (rownames(test.loss.mat)),
  beside = TRUE
)

## ---- fig.show='hold', fig.width= 6, fig.height= 6-----------------------
# Run CV for whole dataset
if(data.set$is.01){
  # Binary
  model.list <- LMLogisticLossL2CV(data.set$features, data.set$labels, NULL, penalty.vec)
}else{
  # Regression
  model.list <- LMSquareLossL2CV(data.set$features,data.set$labels,NULL, penalty.vec)
}

dot.x <- model.list$selected.penalty
dot.y <- model.list$mean.validation.loss.vec[penalty.vec == model.list$selected.penalty]

matplot(
  y = cbind(model.list$mean.validation.loss.vec, model.list$mean.train.loss.vec),
  x = as.matrix(penalty.vec),
  xlab = "penalty",
  ylab = "mean loss value",
  type = "l",
  lty = 1:2,
  pch = 15,
  col = c(17)
)

matpoints(x = dot.x,
          y = dot.y,
          col = 2,
          pch = 19)
legend(
  x = length(penalty.vec),
  0,
  c("Validation loss", "Train loss"),
  lty = 1:2,
  xjust = 1,
  yjust = 0
)

