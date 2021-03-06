% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/LML2.R
\name{LMSquareLossL2}
\alias{LMSquareLossL2}
\title{Linear model L2 regularization with square loss}
\usage{
LMSquareLossL2(X.scaled.mat, y.vec, penalty, opt.thresh = 0.5,
  initial.weight.vec, step.size = 0.01)
}
\arguments{
\item{X.scaled.mat}{a numeric matrix of size [n x p]}

\item{y.vec}{a numeric matrix of length nrow(X.scaled.mat)}

\item{penalty}{a non-negative numeric scalar}

\item{opt.thresh}{a positive numeric scalar}

\item{initial.weight.vec}{a numeric vector of size ncol(X.scaled.mat)}

\item{step.size}{a numeric scalar, which is also greater than 0}
}
\value{
opt.weight the optimal weight vector of length ncol(X.scaled)
}
\description{
Training by using L2 regularization on a linear model with square loss .
Return the optimal weight vector for the given threshold and penalty.
}
\examples{
data(ozone, package = "ElemStatLearn")
y.vec <- ozone[, 1]
X.mat <- as.matrix(ozone[,-1])
num.train <- dim(X.mat)[1]
num.feature <- dim(X.mat)[2]
X.mean.vec <- colMeans(X.mat)
X.std.vec <- sqrt(rowSums((t(X.mat) - X.mean.vec) ^ 2) / num.train)
X.std.mat <- diag(num.feature) * (1 / X.std.vec)
X.scaled.mat <- t((t(X.mat) - X.mean.vec) / X.std.vec)
optimal.weight.vec <- LMSquareLossL2(X.scaled.mat, y.vec, penalty = 0.5, initial.weight.vec = c(rep(0, ncol(X.mat) + 1)))
}
