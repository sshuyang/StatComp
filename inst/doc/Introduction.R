## ----eval=FALSE---------------------------------------------------------------
#  sirs <- function(A,y,a=0.1,delta=1e-6,x0=c(rep(1,p)),maxsize=min(ceiling(n/2),p),
#                    eps=1/p,thresh=1e-6, maxiter=50, maxseq=50 ,tol=1e-6){
#    n=nrow(A)
#    p=ncol(A)
#    x_ini = x0
#    rep = 1
#    move = 1
#    xp0 = rep(0, p)
#    while (move > tol && rep <= maxseq){
#    xp = sirscore(A, y, a, delta, x_ini, maxsize, thresh, maxiter, tol)
#    num = sum(xp != 0)
#    if (num <= maxsize)
#      break
#    else{
#      estmod = which(xp!=0)
#      xd = xp(estmod)
#      kth = sort(abs(xd),decreasing = T)[rep]
#  
#     x_ini = c(rep(eps,p))
#     x_ini[xd>=kth] = 1
#  
#    move = sum((xp - xp0)^2)
#    xp0 = xp
#    rep = rep + 1
#  
#      }
#    }
#    return(xp)
#  }
#  
#  sirscore <- function(A,y,a=0.1,delta=1e-6,x0=c(rep(1,p)),maxsize=min(ceiling(n/2),p),
#                       thresh=1e-6, maxiter=50, tol=1e-6)
#  {
#    n=nrow(A)
#    p=ncol(A)
#    Dw = function(t) {return(abs(t)*(a + abs(t))/(a + 1))}
#    x = x0
#    D = diag(Dw(x),p,p)
#    k = 1
#    upd = 1
#    update = 1
#  
#    while (update > tol && k <= maxiter){
#      k = k + 1
#      xold = x
#  
#      if (p <= ceiling(log(n))*n){
#        D1 = sqrt(D)
#        x = D1%*%solve(delta*diag(p) + D1%*%t(A)%*%A%*%D1)%*%D1%*%t(A)%*%y}
#      # faster to compute when n is large compared with p
#      else
#      {x = D%*%t(A)%*%solve(delta*diag(n) + A%*%D%*%t(A))%*%y;
#      # faster to compute when p is large compared with n
#      }
#  
#      update = sum((x - xold)^2)
#      D = diag(as.numeric(Dw(x)),p,p)
#    }
#  
#    xthre = which(abs(x) > thresh)
#    x[-xthre] = 0
#    num = length(xthre)
#  
#    if (num <= maxsize){
#      xp = x
#      estmod = which(xp!=0)
#      A_mod = A[,estmod]
#      xp[estmod] = solve(t(A_mod)%*%A_mod)%*%t(A_mod)%*%y
#      xpthre = which(abs(xp) > thresh)
#      xp[-xpthre] = 0}
#    else xp = x
#  
#    return(xp)
#  }
#  
#  

## ----warning=FALSE------------------------------------------------------------
library(MASS)
library(StatComp21016)
n = 100
p = 50
rho = 0.5
mu = c(rep(0,p))
beta = c(0.5,-0.5,1,-1.2,-1,rep(0,p-5))
sigma1 <- matrix(0, p, p)
sigma1 <- rho ^ (abs(row(sigma1) - col(sigma1)))
A = mvrnorm(n,mu,sigma1)
y = A%*%beta
a = 0.4
res = sirs(A,y,a)
print(res)

## ----eval=FALSE---------------------------------------------------------------
#  fitmodel <- function(A,y,res,beta){
#    n = nrow(A)
#    p = ncol(A)
#    num = sum(res != 0)
#    nullzero = which(res != 0)
#    error = mean((y-A%*%res)^2)
#    norm=sum(beta^2)
#    if(norm!=0){
#      s = length(which(beta!=0))
#    index = which(beta != 0)
#    num1 = sum(res[index] != 0)
#  
#    if (num1 == s)
#      recovery = 1
#    else  recovery = 0
#  
#    return(list(number=num,nullzero=nullzero,
#                error=error,recovery=recovery))
#    }
#  
#    else return(list(number=num,nullzero=nullzero,error=error))
#  
#  }

## ----warning=FALSE------------------------------------------------------------
library(MASS)
library(StatComp21016)
n = 100
p = 50
a = 0.4
rho = 0.5
mu = c(rep(0,p))
beta = c(0.5,-0.5,1,-1.2,-1,rep(0,p-5))
sigma1 <- matrix(0, p, p)
sigma1 <- rho ^ (abs(row(sigma1) - col(sigma1)))
A = mvrnorm(n,mu,sigma1)
y = A%*%beta
res = sirs(A,y,a)
obj = fitmodel(A,y,res,beta)
print(obj)


## ----eval=FALSE---------------------------------------------------------------
#  kernel<-function(x,Xi,h){
#    ker=numeric(length(x))
#    for(i in 1:length(x)){
#      if(abs(x[i]-Xi)<=h) ker[i]=1/2
#      else ker[i]=0
#    }
#    return (ker)
#  }
#  

## -----------------------------------------------------------------------------
n=1000
x1=rnorm(n,0,1)
 x2=rnorm(n,1,0.3)
index<-sample(c(1,0),n,replace = TRUE,prob = c(0.3,0.7))
X=index*x1+(1-index)*x2
h=0.05
x=seq(-2,3,0.01)
y=c(rep(0,length(x)))
for(i in 1:n){
y=y+kernel(x,X[i],h)/(n*h)
 }
plot(x,y,type='l',main ='h=0.05')


