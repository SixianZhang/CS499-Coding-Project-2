library(testthat)
library(LinearModel)
data(spam, package = "ElemStatLearn")
X.mat <- data.matrix(spam[,-ncol(spam)])
y.vec <- as.vector(ifelse(spam$spam == 'spam',1,0))
penalty.vec <- seq(5, 0.1, by = -0.1)
# X.mat, y.vec, penalty.vec opt.thresh = 0.5

# LMLogisticLossL2penalties X.mat, y.vec, penalty.vec
test_that(
  "For valid inputs, your function returns an output of the expected type/dimension",
  {
    W.mat <-
      LMLogisticLossL2penalties(X.mat, y.vec, penalty.vec, opt.thresh = 0.5)
    expect_true(is.numeric(W.mat))
    expect_true(is.matrix(W.mat))
    expect_equal(nrow(W.mat), ncol(cbind(1, X.mat)))
  }
)

test_that(
  "For an invalid input, your function stops with an informative error message.",
  {
    expect_error(
      W.mat <- 
        LMLogisticLossL2penalties(as.data.frame(X.mat), y.vec, penalty.vec),
      "X.mat must be a numeric matrix",
      fixed = TRUE
    )
    expect_error(
      W.mat <-
        LMLogisticLossL2penalties(X.mat, y.vec[-1], penalty.vec),
      "y.vec must be a numeric vector of length nrow(X.mat)",
      fixed = TRUE
    )
    expect_error(
      W.mat <-
        LMLogisticLossL2penalties(X.mat, y.vec, seq(0.1, 5, by = 0.1)),
      "penalty.vec must be a non-negative decreasing numeric vector",
      fixed = TRUE
    )
  }
)
