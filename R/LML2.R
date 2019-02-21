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
LMSquareLossL2 <- function(X.scaled.mat, y.vec, penalty, opt.thresh = 0.5, initial.weight.vec){
  if (!all(is.matrix(X.mat),is.numeric(X.mat))){
    stop("X.mat must be a numeric matrix.")
  }
  
  if (!all(is.vector(y.vec), is.numeric(y.vec), length(y.vec) == nrow(X.mat))){
    stop("y.vec must be a numeric vector of the same number of rows as X.mat.")
  }
  
  if (!all(length(penalty) == 1, is.numeric(penalty), penalty >= 0)){
    stop("penalty must be a non-negative numeric scalar.")
  }
  
  if (!all(length(opt.thresh) == 1, is.numeric(opt.thresh), opt.thresh > 0)){
    stop("opt.thresh must be a positive numeric scalar.")
  }
  
  if (!all(is.vector(initial.weight.vec), is.numeric(initial.weight.vec))){
    stop("initial.weight.vec must be a numeric vector.")
  }
  
  weight.vec <- initial.weight.vec
  # Compute the gradient
  while(TRUE){
    grad.cost.func <- 2 * t(X.scaled.mat) %*% 
      (X.scaled.mat %*% weight.vec - y.vec) + 2 * penalty * weight.vec 
    
    if (t(grad.cost.func) %*% grad.cost.func <= opt.thresh){
      break
    }else{
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
LMLogisticLossL2 <- function(X.scaled.mat, y.vec, penalty, opt.thresh, initial.weight.vec){
  
}