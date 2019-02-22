#' Linear model L2 regularization with square loss
#'
#' @param X.scaled.mat
#' @param y.vec
#' @param penalty
#' @param opt.thresh
#' @param initial.weight.vec
#'
#' @return
#' @export
#'
#' @examples
LMSquareLossL2 <-
  function(X.scaled.mat,
           y.vec,
           penalty,
           opt.thresh = 0.5,
           initial.weight.vec) {
    if (!all(is.matrix(X.mat), is.numeric(X.mat))) {
      stop("X.mat must be a numeric matrix.")
    }
    
    if (!all(is.vector(y.vec),
             is.numeric(y.vec),
             length(y.vec) == nrow(X.mat))) {
      stop("y.vec must be a numeric vector of the same number of rows as X.mat.")
    }
    
    if (!all(length(penalty) == 1, is.numeric(penalty), penalty >= 0)) {
      stop("penalty must be a non-negative numeric scalar.")
    }
    
    if (!all(length(opt.thresh) == 1,
             is.numeric(opt.thresh),
             opt.thresh > 0)) {
      stop("opt.thresh must be a positive numeric scalar.")
    }
    
    if (!all(is.vector(initial.weight.vec),
             is.numeric(initial.weight.vec))) {
      stop("initial.weight.vec must be a numeric vector.")
    }
    
    weight.vec <- initial.weight.vec
    # Compute the gradient
    while (TRUE) {
      grad.cost.func <- 2 * t(X.scaled.mat) %*%
        (X.scaled.mat %*% weight.vec - y.vec) + 2 * penalty * weight.vec
      
      if (sum(abs(grad.cost.func)) <= opt.thresh) {
        break
      } else{
        weight.vec <- weight.vec - penalty * (grad.cost.func)
      }
      
    }
    optimal.weight <- weight.vec
    return(optimal.weight)
    
  }


#' Linear model L2 regularization with logistic loss
#'
#' @param X.scaled.mat
#' @param y.vec
#' @param penalty
#' @param opt.thresh
#' @param initial.weight.vec
#'
#' @return
#' @export
#'
#' @examples
LMLogisticLossL2 <-
  function(X.scaled.mat,
           y.vec,
           penalty,
           opt.thresh,
           initial.weight.vec) {
    # Check type and dimension
    if (!all(is.numeric(X.scaled.mat), is.matrix(X.scaled.mat))) {
      stop("X.scaled.mat must be a numeric matrix")
    }
    
    if (!all(is.numeric(y.vec),
             is.vector(y.vec),
             length(y.vec) == nrow(X.scaled.mat))) {
      stop("y.vec must be a numeric vector of lenght nrow(X.scaled.mat")
    }
    
    if (!all(is.numeric(penalty), length(penalty) == 1, penalty >= 0)) {
      stop("penalty must be a non-negative numeric scalar")
    }
    
    if (!all(is.numeric(opt.thresh),
             length(opt.thresh) == 1,
             opt.thresh > 0)) {
      stop("opt.thresh must be a positive numeric scalar")
    }
    
    if (!all(
      is.numeric(initial.weight.vec),
      is.vector(initial.weight.vec),
      length(initial.weight.vec) == ncol(X.scaled.mat)
    )) {
      stop("initial.weight.vec must be a numeric vector of length ncol(X.scaled.mat)")
    }
    
    n.features <- ncol(X.scaled.mat)
    step.size <- 0.5 # Do we need to expose this?
    cost <-
      sum(1 + exp(-y.vec * (X.scaled.mat %*% t(
        initial.weight.vec
      )))) + penalty * sum(initial.weight.vec ^ 2) 
    # cost.gradient <- as.vector(rep(0,n.features))

    opt.weight.vec = initial.weight.vec

    # Iteration # L1 norm?
    while (cost > opt.thresh){ # Change this if it is a L1 norm of gradient
      last.weight.vec = opt.weight.vec
      loss.gradient.vec <- -t(X.scaled.mat) %*% y.vec / (1 + exp(y.vec * (X.scaled.mat %*% last.weight.vec)))
      cost.gradient.vec <- loss.gradient + penalty * 2 * last.weight.vec
      opt.weight.vec <- last.weight.vec - step.size * cost.gradient.vec
      cost <-
        sum(1 + exp(-y.vec * (X.scaled.mat %*% t(
          initial.weight.vec
        )))) + penalty * sum(initial.weight.vec ^ 2)
    }
    
    return(opt.weight.vec)
  }