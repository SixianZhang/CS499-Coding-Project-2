#' Linear model with L2 penalties and square loss
#'
#' @param X.mat
#' @param y.vec
#' @param penalty.vec
#'
#' @return
#' @export
#'
#' @examples
LMSquareLossL2penalties <- function(X.mat, y.vec, penalty.vec) {
  if (!all(is.matrix(X.mat), is.numeric(X.mat))) {
    stop("X.mat must be a numeric matrix.")
  }
  
  if (!all(is.vector(y.vec),
           is.numeric(y.vec),
           length(y.vec) == nrow(X.mat))) {
    stop("y.vec must be a numeric vector of the same number of rows as X.mat.")
  }
  
  is.decending <- function(vec) {
    result <- all(diff(vec) < 0)
    return(result)
  }
  
  if (!all(
    is.vector(penalty.vec),
    is.numeric(penalty.vec),
    penalty.vec >= 0,
    is.decending(penalty.vec)
  )) {
    stop("penalty.vec must be a non-negative decreasing numeric vector")
  }
  
  X.mat <- X.mat[, -1]
  
  #Obatin X.scaled.mat from the orginal X.mat, to make sure std = 1, u = 0
  num.train <- dim(X.mat)[1]
  num.feature <- dim(X.mat)[2]
  
  X.mean.vec <- colMeans(X.mat)
  X.std.vec <- sqrt(rowSums((t(X.mat) - X.mean.vec) ^ 2) / num.train)
  X.std.mat <- diag(num.feature) * (1 / X.std.vec)
  
  X.scaled.mat <- t((t(X.mat) - X.mean.vec) / X.std.vec)
  
  slope.mat <- matrix(c(
    rep(0, num.feature * length(penalty.vec)),
    num.feature,
    length(penalty.vec)
  ))
  
  for (index in seq(length(penalty.vec))) {
    optimal.weight.vec <- LMSquareLossL2(X.scaled.mat,
                                         y.vec = y.vec,
                                         initial.weight.vec = penalty.vec[index])
    slope.mat[, index] <- optimal.weight.vec
  }
  
  intercept <- -t(slope.mat) %*% X.std.mat %*% X.mean.vec #m x 1
  slope <- t(slope.mat) %*% X.std.mat #m x f-1
  W.mat <- rbind(t(intercept), t(slope))
  return(W.mat)
}



#' Linear model with L2 penalties and logistic loss
#'
#' @param X.mat
#' @param y.vec
#' @param penalty.vec
#'
#' @return
#' @export
#'
#' @examples
LMLogisticLossL2penalties <- function(X.mat, y.vec, penalty.vec) {
  # Check type and dimension
  if (!all(is.numeric(X.mat), is.matrix(X.mat))) {
    stop("X.mat must be a numeric matrix")
  }
  
  if (!all(is.numeric(y.vec),
           is.vector(y.vec),
           length(y.vec) == nrow(X.mat))) {
    stop("y.vec must be a numeric vector of length nrow(X.mat)")
  }
  
  if (!all(is.numeric(penalty.vec),
           is.vector(penalty.vec),
           penalty.vec >= 0)) {
    stop("penallty.vec must be a non-negative numeric vector")
  }
  
  # Initializing
  X.mat <- X.mat[,-1]
  
  n.train <- nrow(X.mat)
  n.features <- ncol(X.mat)
  n.penalties <- length(penalty.vec)
  opt.thresh <- 5 # Do we need to expose this?
  
  # Scale X.mat with m = 0, sd = 1
  feature.mean.vec <- colMeans(X.mat)
  feature.sd.vec <- sqrt(rowSums((t(X.mat) - feature.mean.vec)^2)/n.train)
  feature.sd.mat <- 1 / diag(feature.sd.vec)
  
  X.scaled.mat <- t((t(X.mat) - feature.mean.vec)/feature.sd.vec)
  
  W.mat <- matrix(0, nrow = n.features, ncol = n.penalties)
  
  for(i.penalty in (1:n.penalties)){
    W.mat[,i.penalty] <- LMLogisticLossL2(X.scaled.mat,y.vec,opt.thresh, penalty.vec[i.penalty])
  }
  
  intercept.vec <- -t(feature.mean.vec) %*% feature.sd.mat %*% W.mat
  W.mat <- rbind(intercept.vec,feature.sd.mat %*% W.mat)
  
  return(W.mat)
  
}