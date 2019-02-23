library(testthat)
library(LinearModel)
data(prostate, package = "ElemStatLearn")
X.mat <- data.matrix(subset(prostate, select = -c(train, lpsa)))
y.vec <- as.vector(data.matrix(subset(prostate, select = lpsa)))
penalty.vec <- as.vector(data.matrix(subset(prostate, select =  lpsa)))
# X.mat, y.vec, penalty.vec

# LMLogisticLossL2penalties X.mat, y.vec, penalty.vec
test_that(
  "For valid inputs, your function returns an output of the expected type/dimension",
  {
    W.mat <-
      LMLogisticLossL2penalties(X.mat, y.vec, penalty.vec)
    expect_true(is.numeric(W.mat))
    expect_true(is.matrix(W.mat))
    expect_equal(nrow(W.mat), ncol(X.mat))
    expect_equal(ncol(W.mat), len(penalty.vec))
  }
)

test_that(
  "For an invalid input, your function stops with an informative error message.",
  {
    expect_error(
      W.mat <- 
        LMLogisticLossL2penalties(as.data.frame(X.mat), y.vec, penalty.vec),
      "X.mat must be a numeric matrix.",
      fixed = TRUE
    )
    expect_error(
      W.mat <-
        LMLogisticLossL2penalties(X.mat, y.vec[-1], penalty.vec),
      "y.vec must be a numeric vector of the same number of rows as X.mat.",
      fixed = TRUE
    )
    expect_error(
      W.mat <-
        LMLogisticLossL2penalties(X.mat, y.vec, penalty.vec[-1]),
      "penalty.vec must be a non-negative decreasing numeric vector",
      fixed = TRUE
    )
  }
)