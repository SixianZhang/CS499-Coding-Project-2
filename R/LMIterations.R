#' Linear model iteration with square loss 
#'
#' @param X.mat train feature matrix of size [n x p]
#' @param y.vec train label vector of size [n x 1]
#' @param max.iterations integer scalar greater than 1
#' @param step.size integer scalar 
#'
#' @return W.mat matrix of weight vectors of size [n x max.iterations]
#' @export
#'
#' @examples
LMSquareLossIterations <- function(X.mat, y.vec, max.iterations, step.size = 0.5){
  if (!all(is.matrix(X.mat),is.numeric(X.mat))){
    stop("X.mat must be a numeric matrix.")
  }
  
  if (!all(is.vector(y.vec), is.numeric(y.vec), length(y.vec) == nrow(X.mat))){
    stop("y.vec must be a numeric vector of the same number of rows as X.mat.")
  }
  
  if (!all(is.integer(max.iterations), max.iterations > 1, length(max.iterations) == 1)){
    stop("Input max.iterations must be a greater than 1 integer scalar number.")
  }
  
  if (!all(is.numeric(step.size), length(step.size) == 1)){
    stop("step.size must be a numeric scalar value.")
  }
  
  #Obatin X.scaled.mat from the orginal X.mat, to make sure std = 1, u = 0
  num.train <- dim(X.mat)[1]
  num.feature <- dim(X.mat)[2]
  
  X.mean.vec <- colMeans(X.mat)
  X.mean.mat <- matrix(rep(X.mean.vec, num.train),
                       num.train, num.feature, byrow = TRUE) 
  X.std.vec <- sqrt(colSums((X.mat - X.mean.mat)^2))
  X.std.mat <- matrix(rep(X.std.vec, num.train),
                      num.train, num.feature, byrow = TRUE)
  X.scaled.mat <- (X.mat - X.mean.mat) / X.std.mat
  W.mat <- matrix(c(rep(0, num.feature * max.iterations), num.feature, max.iterations)) 
  
  # for-loop to get the W.mat matrix
  for (iter.index in (1:max.iterations)){
    if (iter.index == 1){
      mean.loss.temp.vec <- (2 * t(X.scaled.mat) %*% 
                           (X.scaled.mat %*% W.mat[,1])) / num.train
      W.vec.temp <- W.mat[,1] - step.size * mean.loss.temp.vec
    }else{
      mean.loss.temp <- (2 * t(X.scaled.mat) %*% 
                           (X.scaled.mat %*% W.mat[,iter.index - 1])) / num.train
      W.vec.temp <- W.mat[,iter.index - 1] - step.size * mean.loss.temp.vec
    }
    W.mat[,iter.index] = W.vec.temp
    
  }
  W.mat <- (W.mat * X.std.mat) + X.mean.vec 
  return(W.mat)
}

#' Linear model iteration with logistic loss
#'
#' @param X.mat train feature matrix of size [n x p]
#' @param y.vec train label vector of size [n x 1]
#' @param max.iterations integer scalar greater than 1
#' @param step.size integer scalar 
#'
#' @return W.mat matrix of weight vectors of size [n x max.iterations]
#' @export
#'
#' @examples
LMLogisticLossIterations <- function(X.mat, y.vec, max.iterations, step.size){
  
}