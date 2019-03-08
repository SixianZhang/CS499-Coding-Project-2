library(testthat)
library(LinearModel)
data(spam, package = "ElemStatLearn")
X.mat <- data.matrix(spam[,-ncol(spam)])
y.vec <- as.vector(ifelse(spam$spam == 'spam',1,0))
fold.vec <- sample(rep(1:4, l = length(y.vec)))
max.iteration <- 5L
# LMLogisticLossEarlyStoppingCV X.mat, y.vec, fold.vec, max.iteration

test_that(
  "For valid inputs, your function returns an output of the expected type/dimension",
  {
    result.list <-
      LMLogisticLossEarlyStoppingCV(X.mat, y.vec, fold.vec, max.iteration)
    expect_true(is.list(result.list))
  }
)

test_that(
  "For an invalid input, your function stops with an informative error message.",
  {
    expect_error(
      result.list <- 
        LMLogisticLossEarlyStoppingCV(as.data.frame(X.mat), y.vec, fold.vec, max.iteration),
      "X.mat must be a numeric matrix",
      fixed = TRUE
    )
    expect_error(
      result.list <-
        LMLogisticLossEarlyStoppingCV(X.mat, y.vec[-1], fold.vec, max.iteration),
      "y.vec must be a numeric vector of length nrow(X.mat)",
      fixed = TRUE
    )
    expect_error(
      result.list <-
        LMLogisticLossEarlyStoppingCV(X.mat, y.vec, fold.vec[-1], max.iteration),
      "fold.vec must be a numeric vector of length nrow(X.mat)",
      fixed = TRUE
    )
    expect_error(
      result.list <-
        LMLogisticLossEarlyStoppingCV(X.mat, y.vec, fold.vec, as.double(max.iteration)),
      "max.iteration must be an integer scalar greater than 1",
      fixed = TRUE
    )
  }
)
