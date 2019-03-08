#' Linear model L2 regularization with square loss
#'
#' @param X.scaled.mat a numeric matrix of size [n x p]
#' @param y.vec a numeric matrix of length nrow(X.scaled.mat)
#' @param penalty a non-negative numeric scalar
#' @param opt.thresh a positive numeric scalar
#' @param initial.weight.vec a numeric vector of size ncol(X.scaled.mat)
#'
#' @return opt.weight the optimal weight vector of length ncol(X.scaled)
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
    intercept.scalar <- 0
    # Compute the gradient
    while (TRUE) {
      grad.cost.func.slope <- 2 * t(X.scaled.mat) %*%
        (X.scaled.mat %*% weight.vec + c(rep(1, dim(X.scaled.mat)[1]))
         * intercept.scalar - y.vec) + 2 * penalty * weight.vec
      
      grad.cost.func.intercept <-
        2 * t(c(rep(1, dim(X.scaled.mat)[1]))) %*%
        (c(rep(1, dim(X.mat)[1])) * intercept.scalar + X.scaled.mat %*% weight.vec - y.vec)
      
      if (sum(abs(grad.cost.func)) <= opt.thresh) {
        break
      } else{
        weight.vec <- weight.vec - penalty * grad.cost.func.slope
        intercept.scalar <-
          intercept.scalar - penalty * grad.cost.func.intercept
      }
      
    }
    optimal.weight <- c(intercept.scalar, weight.vec)
    return(optimal.weight)
    
  }


#' Linear model L2 regularization with logistic loss, including beta during the training
#'
#' @param X.scaled.mat a numeric matrix of size [n x p]
#' @param y.vec a numeric matrix of length nrow(X.scaled.mat)
#' @param penalty a non-negative numeric scalar
#' @param opt.thresh a positive numeric scalar
#' @param initial.weight.vec a numeric vector of size ncol(X.scaled.mat)
#' @param step.size a numeric scalar greater than zero
#' @param max.iteration a integer scalar greater than one
#'
#' @return opt.weight the optimal weight vector of length ncol(X.scaled)
#' @export
#'
#' @examples
LMLogisticLossL2 <-
  function(X.scaled.mat,
           y.vec,
           penalty,
           opt.thresh,
           initial.weight.vec,
           step.size = 0.01,
           max.iteration = 10) {
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
      length(initial.weight.vec) == ncol(X.scaled.mat) + 1
    )) {
      stop("initial.weight.vec must be a numeric vector of length ncol(X.scaled.mat) + 1") # <- Change here
    }
    
    # Initializing
    # here n.feature is also p
    n.features <- ncol(X.scaled.mat)
    n.trains <- nrow(X.scaled.mat)
    
    opt.W.vec = initial.weight.vec[-1]
    opt.beta = initial.weight.vec[1]
    
    W.gradient.vec <-
      -t(X.scaled.mat) %*% (y.vec / (1 + exp(
        y.vec * (X.scaled.mat %*% opt.W.vec + rep(1, n.trains) * opt.beta)
      ))) +  2 * penalty * opt.W.vec
    beta.gradient <-
      -sum(y.vec / (1 + exp(
        y.vec * (X.scaled.mat %*% opt.W.vec + rep(1, n.trains) * opt.beta)
      )))
    
    # loss.gradient.vec <- -t(X.scaled.mat) %*% y.vec / (1 + exp(y.vec * (X.scaled.mat %*% opt.weight.vec)))
    # cost.gradient.vec <- loss.gradient.vec + penalty * opt.weight.vec # This is for L1 norm
    
    n.iteration <- 0
    
    while (norm(abs(W.gradient.vec)) > opt.thresh &&
           n.iteration <= max.iteration) {
      n.iteration = n.iteration + 1
      W.gradient.vec <-
        -t(X.scaled.mat) %*% (y.vec / (1 + exp(
          y.vec * (X.scaled.mat %*% opt.W.vec + rep(1, n.trains) * opt.beta)
        ))) +  2 * penalty * opt.W.vec
      beta.gradient <-
        -sum(y.vec / (1 + exp(
          y.vec * (X.scaled.mat %*% opt.W.vec + rep(1, n.trains) * opt.beta)
        )))
      
      opt.W.vec <- opt.W.vec - step.size * W.gradient.vec
      opt.beta <- opt.beta - step.size * beta.gradient
      
      # opt.weight.vec <- opt.weight.vec - step.size * cost.gradient.vec # Is this L1 norm gradient?
    }
    
    # # Iteration # L1 norm?
    # while (cost > opt.thresh){ # Change this if it is a L1 norm of gradient
    #   last.weight.vec <- opt.weight.vec
    #   loss.gradient.vec <- -t(X.scaled.mat) %*% y.vec / (1 + exp(y.vec * (X.scaled.mat %*% last.weight.vec)))
    #   cost.gradient.vec <- loss.gradient + penalty * 2 * last.weight.vec # This is for L2 norm
    #   opt.weight.vec <- opt.weight.vec - step.size * cost.gradient.vec
    #   cost <-
    #     sum(1 + exp(-y.vec * (X.scaled.mat %*% t(
    #       initial.weight.vec
    #     )))) + penalty * sum(initial.weight.vec ^ 2)
    # }
    
    opt.weight.vec <- c(opt.beta, opt.W.vec) #opt.weight.vec is p+1
    
    return(opt.weight.vec)
  }