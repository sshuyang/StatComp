#' @title SIRS algorithm to get a sparse coefficient beta in sparse recovery problem.
#' @name sirs
#' @description Core pragram of the sequentially and iteratively reweighted squares(SIRS) algorithm (Lv and Fan, 2009), 
#' which solves the problem of sparse recovery using an adaptively reweighted L_2-regularization method.
#' @param A n x p design matrix with sample size n and dimensionality p
#' @param y n-vector of response
#' @param a shape parameter of SICA penalty controling its maximum concavity. Default = 0.1
#' @param delta a small ridge parameter for stabilizing matrix inverse. Default = 1e-6
#' @param x0 the initial value. Default = rep(1,p) 
#' @param maxsize maximum size of the sparse model. Default = min(ceiling(n/2), p)
#' @param eps scalar between 0 and 1, downweighting undesirable predictors. Default = 1/p
#' @param thresh threshold of the circulation. Default = 1e-6
#' @param maxiter maximum number of iterations. Default = 50 
#' @param maxseq maximum number of sequential steps. Default = min(maxiter, maxsize)
#' @param tol tolerance for updating.  Default = 1e-6
#' @return a coeffcient value in sparse recovery problem.
#' @examples
#' \dontrun{
#' library(MASS)
#' n = 100
#' p = 50
#' rho = 0.5
#' mu = c(rep(0,p))
#' beta = c(0.5,-0.5,1,-1.2,-1,rep(0,p-5))
#' sigma1 <- matrix(0, p, p)
#' sigma1 <- rho ^ (abs(row(sigma1) - col(sigma1)))
#' A = mvrnorm(n,mu,sigma1)
#' y = A%*%beta
#' a = 0.4
#' res = sirs(A,y,a)
#' }
#' @import MASS
#' @import boot
#' @import bootstrap
#' @import stats
#' @useDynLib StatComp21016
#' @export
sirs <- function(A,y,a=0.1,delta=1e-6,x0=c(rep(1,p)),maxsize=min(ceiling(n/2),p),
                  eps=1/p,thresh=1e-6, maxiter=50, maxseq=50 ,tol=1e-6){
  n=nrow(A)
  p=ncol(A)
  x_ini = x0
  rep = 1
  move = 1
  xp0 = rep(0, p)
  while (move > tol && rep <= maxseq){
  xp = sirscore(A, y, a, delta, x_ini, maxsize, thresh, maxiter, tol)
  num = sum(xp != 0)
  if (num <= maxsize)
    break
  else{
    estmod = which(xp!=0)
    xd = xp(estmod)
    kth = sort(abs(xd),decreasing = T)[rep]
  
   x_ini = c(rep(eps,p))
   x_ini[xd>=kth] = 1
  
  move = sum((xp - xp0)^2)
  xp0 = xp
  rep = rep + 1
  
    }
  }
  return(xp)
}

sirscore <- function(A,y,a=0.1,delta=1e-6,x0=c(rep(1,p)),maxsize=min(ceiling(n/2),p),
                     thresh=1e-6, maxiter=50, tol=1e-6)
{
  n=nrow(A)
  p=ncol(A)
  Dw = function(t) {return(abs(t)*(a + abs(t))/(a + 1))}
  x = x0
  D = diag(Dw(x),p,p)
  k = 1
  upd = 1
  update = 1
  
  while (update > tol && k <= maxiter){
    k = k + 1
    xold = x
    
    if (p <= ceiling(log(n))*n){
      D1 = sqrt(D)
      x = D1%*%solve(delta*diag(p) + D1%*%t(A)%*%A%*%D1)%*%D1%*%t(A)%*%y}
    # faster to compute when n is large compared with p 
    else
    {x = D%*%t(A)%*%solve(delta*diag(n) + A%*%D%*%t(A))%*%y; 
    # faster to compute when p is large compared with n                              
    }
    
    update = sum((x - xold)^2)
    D = diag(as.numeric(Dw(x)),p,p)
  }
  
  xthre = which(abs(x) > thresh)
  x[-xthre] = 0
  num = length(xthre)
  
  if (num <= maxsize){
    xp = x
    estmod = which(xp!=0)
    A_mod = A[,estmod]
    xp[estmod] = solve(t(A_mod)%*%A_mod)%*%t(A_mod)%*%y
    xpthre = which(abs(xp) > thresh)
    xp[-xpthre] = 0}
  else xp = x
  
  return(xp)
  
}

#' @title analysis of the result based on SIRS Algorithm
#' @name fitmodel
#' @description A function to analysis the result of the function (\code{sirs}), including 
#' the index of null zero variables,the number of null zero variables, the estimate error, 
#' and wheather the estimator is successful recover the true coeffcient when the true beta is input.
#' when the true beta is input, set recovery=1 while the estimated 
#' index of null zero variables is true, else recovery=0.
#' @param A n x p design matrix with sample size n and dimensionality p
#' @param y n-vector of response
#' @param res result of the \code{sirs}
#' @param beta the true coefficient. Default is rep(zero,p)(unknown).
#' @return A list contain the number of null zero variables, the index of null zero variables, 
#' the estimate error, if the true beta is input, return recovery=1 while the estimated 
#' index of null zero variables is true, else return recovery=0.
#' @examples
#' \dontrun{
#' library(MASS)
#' n = 100
#' p = 50
#' a = 0.4
#' rho = 0.5
#' mu = c(rep(0,p))
#' sigma1 <- matrix(0, p, p)
#' sigma1 <- rho ^ (abs(row(sigma1) - col(sigma1)))
#' A = mvrnorm(n,mu,sigma1)
#' y = A%*%beta
#' beta = c(0.5,-0.5,1,-1.2,-1,rep(0,p-5))
#' res = sirs(A,y,a)
#' obj = fitmodel(A,y,res,beta)
#' }
#' @export
fitmodel <- function(A,y,res,beta){
  n = nrow(A)
  p = ncol(A)
  num = sum(res != 0)
  nullzero = which(res != 0)
  error = mean((y-A%*%res)^2)
  norm=sum(beta^2)
  if(norm!=0){
    s = length(which(beta!=0))
  index = which(beta != 0)
  num1 = sum(res[index] != 0)
  
  if (num1 == s) 
    recovery = 1
  else  recovery = 0
  
  return(list(number=num,nullzero=nullzero,
              error=error,recovery=recovery))
  }
  
  else return(list(number=num,nullzero=nullzero,error=error))

} 


#' @title A kernel function of naive density estimator
#' @name kernal
#' @description the value of naive density estimator kernel with sample X_i.
#' @param x independent variable
#' @param Xi the sample
#' @param h bandwidth
#' @return the value of the kernel function in x.
#' @examples
#' \dontrun{
#' n=1000
#' x1=rnorm(n,0,1)
#' x2=rnorm(n,1,0.3)
#' index<-sample(c(1,0),n,replace = T,prob = c(0.3,0.7))
#' X=index*x1+(1-index)*x2
#' h=0.05
#' x=seq(-2,3,0.01)
#' y=c(rep(0,length(x)))
#' for(i in 1:n){
#' y=y+kernel(x,X[i],h)/(n*h)
#'  }
#' plot(x,y,type='l',main ='h=0.05')
#' }
#' @export
kernel<-function(x,Xi,h){
  ker=numeric(length(x))
  for(i in 1:length(x)){
    if(abs(x[i]-Xi)<=h) ker[i]=1/2
    else ker[i]=0
  }
  return (ker)
}
