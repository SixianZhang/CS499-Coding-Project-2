#' Cross validation using linear model with L2 regularization and square loss
#'
#' @param X.mat 
#' @param y.vec 
#' @param fold.vec 
#' @param penalty.vec 
#'
#' @return
#' @export
#'
#' @examples
LMSquareLossL2CV <- function(X.mat, y.vec, fold.vec, penalty.vec){
  if (!all(is.matrix(X.mat),is.numeric(X.mat))){
    stop("X.mat must be a numeric matrix.")
  }
  
  if (!all(is.vector(y.vec), is.numeric(y.vec), length(y.vec) == nrow(X.mat))){
    stop("y.vec must be a numeric vector of the same number of rows as X.mat.")
  }
  
  if (!all(is.integer(fold.vec),is.vector(fold.vec))){
    stop("fold.vec must be assigned before input and it must be a integer vector")
  }
  
  if (!all(is.vector(penalty.vec), is.numeric(penalty.vec), penalty.vec >= 0,
           diff(penalty.vec) < 0)){
    stop("penalty.vec must be a non-negative decreasing numeric vector")
  }
  
  # Find the num of K-fold
  n.folds <- length(unique(fold.vec))
  
  validation.loss.mat <- matrix(rep(0, n.folds * length(penalty.vec)),
                                n.folds, length(penalty.vec))
  train.loss.mat <- matrix(rep(0, n.folds * length(penalty.vec)),
                           n.folds, length(penalty.vec))
  
  #Learning process for each fold
  for (fold.i in seq_len(n.folds)){
    train.index <- which(fold.vec != fold.i)
    validation.index <-which(fold.vec == fold.i)
    
    #Calculate train.loss
    W.mat <- 
      LMSquareLossL2penalties(X.mat[train.index,], y.vec[train.index,], penalty.vec)
    
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
  
  selected.penalty <- penalty.vec[which.min(mean.validation.loss.vec)]
  W.mat <- 
    LMSquareLossL2penalties(X.mat[train.index,], y.vec[train.index,], penalty.vec)
  weight.vec <- W.mat[,selected.penalty]
  
  predict <- function(testX.mat){
    if (!all(is.numeric(testX.mat),
             is.matrix(testX.mat),
             ncol(testX.mat) == ncol(X.mat))) {
      stop("testX.mat must be a numeric matrix with ncol(X.mat) columns")
    }
    prediction.vec <- testX.mat %*% weight.vec 
  }
  
  result.list <- list(
    mean.validation.loss.vec = mean.validation.loss.vec,
    mean.train.loss.vec = mean.train.loss.vec,
    penalty.vec = penalty.vec,
    selected.penalty = selected.penalty,
    weight.vec = weight.vec,
    predict
  )
  
  return(result.list)
}


#' Cross validation using linear model with L2 regularization and logistic loss
#'
#' @param X.mat 
#' @param y.vec 
#' @param fold.vec 
#' @param penalty.vec 
#'
#' @return
#' @export
#'
#' @examples
LMLogisticLossL2CV <- function(X.mat, y.vec, fold.vec, penalty.vec){
  
}