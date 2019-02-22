#' Cross validation algorithm using linear model with square loss
#'
#' @param X.mat 
#' @param y.vec 
#' @param fold.vec 
#' @param max.iteration 
#'
#' @return
#' @export
#'
#' @examples
LMSquareLossEarlyStoppingCV <- function(X.mat, y.vec, fold.vec, max.iteration){
  if (!all(is.matrix(X.mat),is.numeric(X.mat))){
    stop("X.mat must be a numeric matrix.")
  }
  
  if (!all(is.vector(y.vec), is.numeric(y.vec), length(y.vec) == nrow(X.mat))){
    stop("y.vec must be a numeric vector of the same number of rows as X.mat.")
  }
  
  if (!all(is.integer(max.iterations), max.iterations > 1, length(max.iterations) == 1)){
    stop("Input max.iterations must be a greater than 1 integer scalar number.")
  }
  
  if (!all(is.integer(fold.vec),is.vector(fold.vec))){
    stop("fold.vec must be assigned before input and it must be a integer vector")
  }
  # Find the num of K-fold
  n.folds <- length(unique(fold.vec))
  
  validation.loss.mat <- matrix(rep(0, n.folds * max.iteration), n.folds, max.iteration)
  train.loss.mat <- matrix(rep(0, n.folds * max.iteration), n.folds, max.iteration)
  
  
  #Learning process for each fold
  for (fold.i in seq_len(n.folds)){
    train.index <- which(fold.vec != fold.i)
    validation.index <-which(fold.vec == fold.i)
    
    #Calculate train.loss
    W.mat <- 
      LMSquareLossIterations(X.mat[train.index,], y.vec[train.index,], max.iterations)
    
    train.predit <- X.mat[train.index,] %*% W.mat
    train.loss <- (train.predit - y.vec[train.index,])^2
    
    #Calculate validation.loss
    validation.predict <- X.mat[validation.index,] %*% W.mat
    validation.loss <- (validation.predict - y.vec[validation.index,])^2
    
    mean.train.loss.vec <- colMeans(train.loss)
    mean.validation.loss.vec <- colMeans(validation.loss)
    
    train.loss.mat[fold.i,] = mean.train.loss.vec
    validation.loss.mat[fold.i,] = mean.validation.loss.vec 
  }
  
  mean.train.loss.vec <- colMeans(train.loss.mat)
  mean.validation.loss.vec <- colMeans(validation.loss.mat)
  
  #Overall optimal iteration steps
  selected.steps <- which.min(mean.validation.loss.vec)
  W.mat.all <- 
    LMSquareLossIterations(X.mat, y.vec, max.iterations = selected.steps)
  weight.vec <- W.mat.all[,selected.steps]
  
  predict <- function(testX.mat){
    if (!all(is.numeric(testX.mat),
             is.matrix(testX.mat),
             ncol(testX.mat) == ncol(X.mat))) {
      stop("testX.mat must be a numeric matrix with ncol(X.mat) columns")
    }
    prediction.vec <- testX.mat %*% weight.vec 
  }
  
  result.list <-
    list(
      mean.validation.loss.vec = mean.validation.loss.vec,
      mean.train.loss.vec = mean.train.loss.vec,
      selected.steps = selected.steps,
      weight.vec = weight.vec,
      predict = predict
    )
  return(result.list)
}



#' Cross validation algorithm using linear model with logistic loss
#'
#' @param X.mat 
#' @param y.vec 
#' @param fold.vec 
#' @param max.iteration 
#'
#' @return
#' @export
#'
#' @examples
LMLogisticLossEarlyStoppingCV <- function(X.mat, y.vec, fold.vec, max.iteration){
  
}