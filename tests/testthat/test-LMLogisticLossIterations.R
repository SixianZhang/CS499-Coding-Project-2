library(testthat)
library(LinearModel)
data(spam, package = "ElemStatLearn")
X.mat <- data.matrix(spam[,-ncol(spam)])
y.vec <- as.vector(ifelse(spam$spam == 'spam',1,0))
max.iteration <- 5L
step.size <- 0.5L
# LMLogisticLossIterations X.mat, y.vec, max.iterations, step.size = 0.5

test_that(
  "For valid inputs, your function returns an output of the expected type/dimension",
  {
    W.mat <-
      LMLogisticLossIterations(X.mat, y.vec, max.iteration, step.size)
    expect_true(is.numeric(W.mat))
    expect_true(is.matrix(W.mat))
    expect_equal(nrow(W.mat), ncol(cbind(1,X.mat)))
  }
)

test_that(
  "For an invalid input, your function stops with an informative error message.",
  {
    expect_error(
      W.mat <- 
        LMLogisticLossIterations(as.data.frame(X.mat), y.vec, max.iteration, step.size),
      "X.mat must be a numeric matrix",
      fixed = TRUE
    )
    expect_error(
      W.mat <-
        LMLogisticLossIterations(X.mat, y.vec[-1], max.iteration, step.size),
      "y.vec must be a numeric vector of length nrow(X.mat)",
      fixed = TRUE
    )
    expect_error(
      W.mat <-
        LMLogisticLossIterations(X.mat, y.vec, as.double(max.iteration), step.size),
      "max.iterations must be an integer scalar greater than one",
      fixed = TRUE
    )
    expect_error(
      W.mat <-
        LMLogisticLossIterations(X.mat, y.vec, max.iteration, c(rep(step.size,2))),
      "step.size must be a numeric scalar value.",
      fixed = TRUE
    )
  }
)
