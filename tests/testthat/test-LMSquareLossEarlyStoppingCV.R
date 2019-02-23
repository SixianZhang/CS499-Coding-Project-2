library(testthat)
library(LinearModel)
data(prostate, package = "ElemStatLearn")
X.mat <- data.matrix(subset(prostate, select = -c(train, lpsa)))
y.vec <- as.vector(data.matrix(subset(prostate, select = lpsa)))
fold.vec <- as.vector(data.matrix(subset(prostate, select =  lpsa)))
max.iteration <- 5L
# LMSquareLossEarlyStoppingCV X.mat, y.vec, fold.vec, max.iteration

test_that(
  "For valid inputs, your function returns an output of the expected type/dimension",
  {
    result.list <-
      LMSquareLossEarlyStoppingCV(X.mat, y.vec, fold.vec, max.iteration)
    expect_true(is.list(pred.list))
  }
)

test_that(
  "For an invalid input, your function stops with an informative error message.",
  {
    expect_error(
      result.list <- 
        LMSquareLossEarlyStoppingCV(as.data.frame(X.mat), y.vec, fold.vec, max.iteration),
      "X.mat must be a numeric matrix.",
      fixed = TRUE
    )
    expect_error(
      result.list <-
        LMSquareLossEarlyStoppingCV(X.mat, y.vec[-1], fold.vec, max.iteration),
      "y.vec must be a numeric vector of the same number of rows as X.mat.",
      fixed = TRUE
    )
    expect_error(
      result.list <-
        LMSquareLossEarlyStoppingCV(X.mat, y.vec, fold.vec[-1], max.iteration),
      "fold.vec must be assigned before input and it must be a integer vector",
      fixed = TRUE
    )
    expect_error(
      result.list <-
        LMSquareLossEarlyStoppingCV(X.mat, y.vec, fold.vec, as.double(max.iteration)),
      "Input max.iterations must be a greater than 1 integer scalar number.",
      fixed = TRUE
    )
  }
)
