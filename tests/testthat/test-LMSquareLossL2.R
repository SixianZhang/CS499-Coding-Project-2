library(testthat)
library(LinearModel)
data(prostate, package = "ElemStatLearn")
X.mat <- data.matrix(subset(prostate, select = -c(train, lpsa)))
num.train <- dim(X.mat)[1]
num.feature <- dim(X.mat)[2]
X.mean.vec <- colMeans(X.mat)
X.std.vec <-
  sqrt(rowSums((t(X.mat) - X.mean.vec) ^ 2) / num.train)
X.std.mat <- diag(num.feature) * (1 / X.std.vec)

X.scaled.mat <- t((t(X.mat) - X.mean.vec) / X.std.vec)
y.vec <- as.vector(data.matrix(subset(prostate, select = lpsa)))
penalty <- 5
opt.thresh <- 0.5
initial.weight.vec <- c(rep(0, dim(X.scaled.mat)[2] + 1))
#LMSquareLossL2 X.scaled.mat, y.vec, penalty, opt.thresh = 0.5, initial.weight.vec

test_that(
  "For valid inputs, your function returns an output of the expected type/dimension",
  {
    optimal.weight <-
      LMSquareLossL2(X.scaled.mat, y.vec, penalty, opt.thresh, initial.weight.vec, 0.01)
    expect_true(is.vector(optimal.weight))
  }
)

test_that(
  "For an invalid input, your function stops with an informative error message.",
  {
    expect_error(
      optimal.weight <- 
        LMSquareLossL2(as.data.frame(X.scaled.mat), y.vec, penalty, opt.thresh, initial.weight.vec, 0.01),
      "X.mat must be a numeric matrix.",
      fixed = TRUE
    )
    expect_error(
      optimal.weight <-
        LMSquareLossL2(X.scaled.mat, y.vec[-1], penalty, opt.thresh, initial.weight.vec, 0.01),
      "y.vec must be a numeric vector of the same number of rows as X.mat.",
      fixed = TRUE
    )
    expect_error(
      optimal.weight <-
        LMSquareLossL2(X.scaled.mat, y.vec, c(rep(penalty,2)), opt.thresh, initial.weight.vec, 0.01),
      "penalty must be a non-negative numeric scalar.",
      fixed = TRUE
    )
    expect_error(
      optimal.weight <-
        LMSquareLossL2(X.scaled.mat, y.vec, penalty, c(rep(opt.thresh,2)), initial.weight.vec, 0.01),
      "opt.thresh must be a positive numeric scalar.",
      fixed = TRUE
    )    
    expect_error(
      optimal.weight <-
        LMSquareLossL2(X.scaled.mat, y.vec, penalty, opt.thresh, initial.weight.vec[-1], 0.01),
      "initial.weight.vec must be a numeric vector of length ncol(X.scaled.mat) + 1",
      fixed = TRUE
    )
  }
)
