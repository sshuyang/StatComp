\name{StatComp21016-package}
\alias{StatComp21016-package}
\alias{StatComp21016}
\docType{package}
\title{
  Statistic Computation Project
}
\description{
  This package is the final project of the course of statistic computation, containing two functions 'sirs' and 'fitmodel' and the whole homework of this course. 
}
\details{
  The 'sirs' function is the core pragram of the sequentially and iteratively reweighted squares(SIRS) algorithm (Lv and Fan, 2009), which solves the problem of sparse recovery using an adaptively reweighted L_2-regularization method. The 'fitmodel' function is to analyse the result of 'sirs', which prints the index of null zero variables, the number of null zero variables and the estimate error. When the true beta is input, 'fitmodel' print wheather the estimator is successful recover the true coeffcient.
}
\author{
21016

Maintainer: 21016 <ssy18055118551@163.com>
}
\references{
  Jinchi Lv and Yingying Fan(2009).A unified approach to model selection and sparse recovery using regularized least squares, The Annals of Statistics 37 3498–3528.
}
\keyword{ package }
\seealso{
  Optional links to other man pages
}
\examples{
  \dontrun{
library(MASS)
n = 100
p = 50
a = 0.4
rho = 0.5
mu = c(rep(0,p))
sigma1 <- matrix(0, p, p)
sigma1 <- rho ^ (abs(row(sigma1) - col(sigma1)))
A = mvrnorm(n,mu,sigma1)
y = A%*%beta
beta = c(0.5,-0.5,1,-1.2,-1,rep(0,p-5))
res = sirs(A,y,a)
obj = fitmodel(A,y,res,beta)
}
}
