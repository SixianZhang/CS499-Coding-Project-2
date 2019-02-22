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
LMSquareLossIterations <-
  function(X.mat, y.vec, max.iterations, step.size = 0.5) {
    if (!all(is.matrix(X.mat), is.numeric(X.mat))) {
      stop("X.mat must be a numeric matrix.")
    }
    
    if (!all(is.vector(y.vec),
             is.numeric(y.vec),
             length(y.vec) == nrow(X.mat))) {
      stop("y.vec must be a numeric vector of the same number of rows as X.mat.")
    }
    
    if (!all(is.integer(max.iterations),
             max.iterations > 1,
             length(max.iterations) == 1)) {
      stop("Input max.iterations must be a greater than 1 integer scalar number.")
    }
    
    if (!all(is.numeric(step.size), length(step.size) == 1)) {
      stop("step.size must be a numeric scalar value.")
    }
    
    X.mat <- X.mat[,-1]
    #Obatin X.scaled.mat from the orginal X.mat, to make sure std = 1, u = 0
    num.train <- dim(X.mat)[1]
    num.feature <- dim(X.mat)[2]
    
    X.mean.vec <- colMeans(X.mat)
    
    X.std.vec <-
      sqrt(rowSums((t(X.mat) - X.mean.mat) ^ 2) / num.train)
    X.std.mat <- diag(num.feature) * (1 / X.std.vec)
    
    X.scaled.mat <- (t(X.mat) - X.mean.vec) / X.std.vec
    slope.mat <-
      matrix(c(
        rep(0, num.feature * max.iterations),
        num.feature,
        max.iterations
      ))
    
    # for-loop to get the slope.mat matrix
    for (iter.index in (1:max.iterations)) {
      if (iter.index == 1) {
        mean.loss.temp.vec <- (2 * t(X.scaled.mat) %*%
                                 (X.scaled.mat %*% slope.mat[, 1])) / num.train
        slope.vec.temp <-
          slope.mat[, 1] - step.size * mean.loss.temp.vec
      } else{
        mean.loss.temp.vec <- (2 * t(X.scaled.mat) %*%
                                 (X.scaled.mat %*% slope.mat[, iter.index - 1])) / num.train
        slope.vec.temp <-
          slope.mat[, iter.index - 1] - step.size * mean.loss.temp.vec
      }
      slope.mat[, iter.index] = slope.vec.temp
      
    }
    itercept <- -t(slope.mat) %*% X.std.mat %*% X.mean.vec #m x 1
    slope <- t(slope.mat) %*% X.std.mat  #m x (p-1)
    W.mat <- rbind(t(itercept), t(slope)) #p x m
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
LMLogisticLossIterations <-
  function(X.mat, y.vec, max.iterations, step.size) {
    
    # Check type and dimension
    if (!all(is.numeric(X.mat), is.matrix(X.mat))) {
      stop("X.mat must be a numeric matrix")
    }
    
    if (!all(is.numeric(y.vec),
             is.vector(y.vec),
             length(y.vec) == nrow(X.mat))) {
      stop("y.vec must be a numeric vector of length nrow(X.mat)")
    }
    
    if (!all(
      is.numeric(max.iterations),
      is.integer(max.iterations),
      length(max.iterations) == 1,
      max.iterations > 1
    )) {
      stop("max.iterations must be an integer scalar greater than zero")
    }
    
    if (!all(is.numeric(step.size), length(step.size) == 1, step.size > 0)) {
      stop("step.size must be a positive scalar")
    }
    
    # remove the 1 vector if that is the case
    X.mat <- X.mat[,-1]
    
    n.train <- nrow(X.mat)
    n.features <- ncol(X.mat)
    
    # Scale X.mat with m = 0, sd = 1
    feature.mean.vec <- colMeans(X.mat)
    feature.sd.vec <- sqrt(rowSums((t(X.mat) - feature.mean.vec)^2)/n.train)
    feature.sd.mat <- diag(feature.sd.vec)
    
    X.scaled.mat <- t((t(X.mat) - feature.mean.vec)/feature.sd.vec)
    
    W.mat <- matrix(0, nrow = n.features, ncol = max.iterations)
    
    # Iteration
    for(n.interations in (2:max.iterations)){
      W.gradient.vec <- - t(X.scaled.mat) %*% y.vec /(1 + exp(y.vec * (X.scaled.mat %*% W.mat[, n.iterations - 1])))
      W.mat[,n.iterations] <- W.mat[,n.iterations] - step.size * W.gradient.vec
    
    }
    W.mat <- rbind(rep(1,max.iterations),feature.sd.mat %*% W.mat)
    
    return(W.mat)
    
    
  }