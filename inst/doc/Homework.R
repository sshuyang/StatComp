## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ----warning=FALSE------------------------------------------------------------
library(MASS)
attach(women)
lm_fit<-lm(weight~height)

## -----------------------------------------------------------------------------
plot(height,weight)
abline(lm_fit,col="red")

## -----------------------------------------------------------------------------
par(mar = c(2,2,2,2))
par(mfrow=c(2,2))
plot(lm_fit)

## -----------------------------------------------------------------------------
res<-data.frame(lm_fit$fitted.values,lm_fit$residuals)
knitr::kable(women)
knitr::kable(res)

## -----------------------------------------------------------------------------
n<-1e5
sigma<-5
lambda=sqrt(2)*sigma
U=runif(n)
X=sqrt(-2*sigma^2*log(U))

hist(X, prob = TRUE, main = expression(f(X)==X/sigma^2*exp(-X^2/(2*sigma^2))))  
y <- seq(0, 20, .01)
lines(y, y/sigma^2*exp(-y^2/(2*sigma^2))) 


## -----------------------------------------------------------------------------
n<-1e5
sigma<-10
lambda=sqrt(2)*sigma
U=runif(n)
X=sqrt(-2*sigma^2*log(U))
hist(X, prob = TRUE, main = expression(f(X)==X/sigma^2*exp(-X^2/(2*sigma^2))))  
y <- seq(0, 40, .01)
lines(y, y/sigma^2*exp(-y^2/(2*sigma^2))) 


## -----------------------------------------------------------------------------
n<-1e5
sigma<-1
lambda=sqrt(2)*sigma
U=runif(n)
X=sqrt(-2*sigma^2*log(U))
hist(X, prob = TRUE, main = expression(f(X)==X/sigma^2*exp(-X^2/(2*sigma^2))))  
y <- seq(0, 40, .01)
lines(y, y/sigma^2*exp(-y^2/(2*sigma^2))) 


## -----------------------------------------------------------------------------
n=1000
x1=rnorm(n,0,1)
x2=rnorm(n,3,1)
p1=0.75
p2=1-p1
r=sample(c(0,1),n,replace=T,prob=c(p1,p2))
z=r*x1+(1-r)*x2
hist(z,main="histogram of z with p1=0.75")

#p1=0.3
r1=sample(c(0,1),n,replace=T,prob=c(0.3,0.7))
z1=r1*x1+(1-r1)*x2
#p1=0.4
r2=sample(c(0,1),n,replace=T,prob=c(0.4,0.6))
z2=r2*x1+(1-r2)*x2
#p1=0.5
r3=sample(c(0,1),n,replace=T,prob=c(0.5,0.5))
z3=r3*x1+(1-r3)*x2
#p1=0.6
r4=sample(c(0,1),n,replace=T,prob=c(0.6,0.4))
z4=r4*x1+(1-r4)*x2
par(mfrow=c(2,2))
par(mar = c(2,2,2,2))
hist(z1,main="histogram of z with p1=0.3")
hist(z2,main="histogram of z with p1=0.4")
hist(z3,main="histogram of z with p1=0.5")
hist(z4,main="histogram of z with p1=0.6")


## -----------------------------------------------------------------------------
rcompoundpois<-function(n,lambda,alpha,beta,t){
  X<-numeric(n)
  for(i in 1:n){
    N<-rpois(1,lambda*t)
    Y<-rgamma(N,alpha,beta)
    X[i]<-sum(Y)
  }
  return(X)
}


## -----------------------------------------------------------------------------
n=1e5
t=10
lambda=c(1,3,3,5)
alpha=c(1,2,5,10)
beta=c(1,2,1,1)
X1=rcompoundpois(n,lambda[1],alpha[1],beta[1],t)
X2=rcompoundpois(n,lambda[2],alpha[2],beta[2],t)
X3=rcompoundpois(n,lambda[3],alpha[3],beta[3],t)
X4=rcompoundpois(n,lambda[4],alpha[4],beta[4],t)
mean=c(mean(X1),mean(X2),mean(X3),mean(X4))
sd2=c(sd(X1)^2,sd(X2)^2,sd(X3)^2,sd(X4)^2)
u=c(lambda*t*alpha/beta)
sigma2=lambda*t*alpha*(alpha+1)/(beta^2)
para=data.frame(mean,u,sigma2,sd2)
knitr::kable(para)

## -----------------------------------------------------------------------------
beta.hat<-function(x){
  n<-1e4
  X<-runif(n,0,x)
  g<-30*X^2*(1-X)^2*x
  F<-mean(g)
  return(F)
}
F.hat=numeric(9)
x=c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9)
F=pbeta(x,3,3)
for(i in 1:9){
  F.hat[i]=beta.hat(x[i])
}
CDF<-data.frame(F,F.hat)
knitr::kable(CDF)


## -----------------------------------------------------------------------------
n=1e4
sigma=1

U_1=runif(n)
X=sqrt(-2*sigma^2*log(U_1))
Xx=sqrt(-2*sigma^2*log(1-U_1))
U_2=runif(n)
X_1=sqrt(-2*sigma^2*log(U_2))
U_3=runif(n)
X_2=sqrt(-2*sigma^2*log(U_3))

var1=sd((X+Xx)/2)^2
var2=sd((X_1+X_2)/2)^2
var<-data.frame(var1,var2)
knitr::kable(var)


## -----------------------------------------------------------------------------
n=1e4
X_1=rnorm(n)
X_1[X_1<1]=0
theta.hat1=mean(X_1^2)

U=runif(n)
X_2=sqrt(-2*log(U)) 
X_2[X_2<1]=0
theta.hat2=mean(X_2)/sqrt(2*pi)
var1<-sd(X_1^2)^2
var2<-sd(X_2/sqrt(2*pi))^2
f<-function(x){
x^2*exp(-x^2/2)/sqrt(2*pi)}
theta<-integrate(f,1,Inf)$value
f1<-c(theta.hat1,var1)
f2<-c(theta.hat2,var2)
theta.hat<-data.frame(f1=f1,f2=f2)
row.names(theta.hat)<-c("est","var")
knitr::kable(theta.hat)

## -----------------------------------------------------------------------------
n=1e4
U=runif(n)
X_2=sqrt(-2*log(U)) 
X_2[X_2<1]=0
theta.hat=mean(X_2)/sqrt(2*pi)

f<-function(x){
x^2*exp(-x^2/2)/sqrt(2*pi)}
theta<-integrate(f,1,Inf)$value

result<-data.frame(theta.hat=theta.hat,theta=theta)
knitr::kable(result)

## -----------------------------------------------------------------------------
set.seed(5)
#norm mean=2,sd=3
n=20
m=1e4
alpha=0.05
U=V=y=numeric(m)
for(i in 1:m){
 X=rnorm(n,2,sd=3) 
 U[i]<-mean(X)-qt(1-alpha/2,n-1)*sd(X)/sqrt(n)
 V[i]<-mean(X)+qt(1-alpha/2,n-1)*sd(X)/sqrt(n)
 if(U[i]<2&&V[i]>2) y[i]=1 
 else y[i]=0
}
mean(y)

#chisq
set.seed(5)
n=20
m=1e4
alpha=0.05
U=V=y=numeric(m)
for(i in 1:m){
 X=rchisq(n,2) 
 U[i]<-mean(X)-qt(1-alpha/2,n-1)*sd(X)/sqrt(n)
 V[i]<-mean(X)+qt(1-alpha/2,n-1)*sd(X)/sqrt(n)
 if(U[i]<2&&V[i]>2) y[i]=1 
 else y[i]=0
}
mean(y)


## -----------------------------------------------------------------------------
#normal
n <- 20
alpha <- .05
UCL <- replicate(1000, expr = {
x <- rnorm(n, mean = 0, sd = 2)
(n-1) * var(x) / qchisq(alpha, df = n-1)
} )
mean(UCL > 4)

#chisq
n <- 20
alpha <- .05
UCL <- replicate(1000, expr = {
x <- rchisq(n,2)
(n-1) * var(x) / qchisq(alpha, df = n-1)
} )
mean(UCL > 4)


## -----------------------------------------------------------------------------
#n=20
n=20
m=1e4
alpha=0.05
U=V=y=p=numeric(m)
for(i in 1:m){
 X=rchisq(n,1) 
 T[i]=abs((mean(X)-1)*sqrt(n)/sd(X))
 p[i]=2*pt(T[i],n-1,lower.tail = F)
 if(p[i]<alpha) y[i]=1 
 else y[i]=0
}
#the result is mean(y)
mean(y)

#n=200
n=200
m=1e4
alpha=0.05
U=V=y=p=numeric(m)
for(i in 1:m){
 X=rchisq(n,1) 
 T[i]=abs((mean(X)-1)*sqrt(n)/sd(X))
 p[i]=2*pt(T[i],n-1,lower.tail = F)
 if(p[i]<alpha) y[i]=1 
 else y[i]=0
}
mean(y)

## -----------------------------------------------------------------------------
#n=20
n=20
m=1e4
alpha=0.05
U=V=y=p=numeric(m)
for(i in 1:m){
 X=runif(n,0,2)
 T[i]=abs((mean(X)-1)*sqrt(n)/sd(X))
 p[i]=2*pt(T[i],n-1,lower.tail = F)
 if(p[i]<alpha) y[i]=1 
 else y[i]=0
}
mean(y)


#n=200
n=200
m=1e4
alpha=0.05
U=V=y=p=numeric(m)
for(i in 1:m){
 X=runif(n,0,2)
 T[i]=abs((mean(X)-1)*sqrt(n)/sd(X))
 p[i]=2*pt(T[i],n-1,lower.tail = F)
 if(p[i]<alpha) y[i]=1 
 else y[i]=0
}
mean(y)

## -----------------------------------------------------------------------------
#n=20
n=20
m=1e4
alpha=0.05
U=V=y=p=numeric(m)
for(i in 1:m){
 X=rexp(n,1)
 T[i]=abs((mean(X)-1)*sqrt(n)/sd(X))
 p[i]=2*pt(T[i],n-1,lower.tail = F)
 if(p[i]<alpha) y[i]=1 
 else y[i]=0
}
mean(y)

#n=200
n=200
m=1e4
alpha=0.05
U=V=y=p=numeric(m)
for(i in 1:m){
 X=rexp(n,1)
 T[i]=abs((mean(X)-1)*sqrt(n)/sd(X))
 p[i]=2*pt(T[i],n-1,lower.tail = F)
 if(p[i]<alpha) y[i]=1 
 else y[i]=0
}
mean(y)

## ----warning=FALSE------------------------------------------------------------
library(MASS)

sk <- function(x) {
  #computes the sample skewness coeff.
  xbar <- colMeans(x)
  xbarm<-matrix(rep(xbar,nrow(x)),nrow(x),ncol(x),byrow = T)
  sigmabar<-t(x-xbarm)%*%(x-xbarm)/nrow(x)
  a <- (x-xbarm)%*%solve(sigmabar)%*%t(x-xbarm)
  b<-sum(colSums(a^3))/(nrow(x)^2)
  return( b )
}

alpha=0.05
n <- c(10, 20, 30, 50, 100, 500);m <- 10000
cv <- qchisq(0.95,10)
mu=c(0,0,0)
sigma=matrix(c(1,0,0,0,1,0,0,0,1),3,3)
p.reject <- numeric(length(n)) #to store sim. results

for(i in 1:length(n)){
  
  sktests <- numeric(m) 
  
  for (j in 1:m) {
    x<-mvrnorm(n[i],mu,sigma)
    b=sk(x)
    sktests[j] <- as.integer(n[i]*b/6 >= cv )
  }
  p.reject[i] <- mean(sktests) 
}
p.reject


## ----warning=FALSE------------------------------------------------------------

library(MASS)

sk <- function(x) {
  #computes the sample skewness coeff.
  xbar <- colMeans(x)
  xbarm<-matrix(rep(xbar,nrow(x)),nrow(x),ncol(x),byrow = T)
  sigmabar<-t(x-xbarm)%*%(x-xbarm)/nrow(x)
  a <- (x-xbarm)%*%solve(sigmabar)%*%t(x-xbarm)
  b<-sum(colSums(a^3))/(nrow(x)^2)
  return( b )
}
alpha <- .1
n <- 50
m <- 2500
epsilon <- c(seq(0, .15, .01), seq(.15, 1, .05))
N <- length(epsilon)
pwr <- numeric(N)
mu=c(0,0,0)
sigma1=matrix(c(1,0,0,0,1,0,0,0,1),3,3)
sigma2=matrix(c(100,0,0,0,100,0,0,0,100),3,3)
#critical value for the skewness test
cv <- qchisq(1-alpha,10)

for (j in 1:N) { #for each epsilon
  e <- epsilon[j]
  sktests <- numeric(m)
  
  for (i in 1:m) { #for each replicate
    index <- sample(c(0, 1), replace = TRUE,size = n, prob = c(1-e, e))
    x1 <- mvrnorm(n, mu, sigma1)
    x2 <- mvrnorm(n, mu, sigma2)
    x<-(1-index)*x1+index*x2
    sktests[i] <- as.integer(n*sk(x)/6 >= cv)
  }
  pwr[j] <- mean(sktests)
}
plot(epsilon, pwr, type = "b",
     xlab = bquote(epsilon), ylim = c(0,1))
abline(h = .1, lty = 3)
se <- sqrt(pwr * (1-pwr) / m) #add standard errors
lines(epsilon, pwr+se, lty = 3)
lines(epsilon, pwr-se, lty = 3)



## ----warning=FALSE------------------------------------------------------------
library(boot)
library(bootstrap)
data=scor
B=2000
n=nrow(data)
covhat=cov(data)
eigenhat=eigen(covhat)
thetahat=eigenhat$values[1]/sum(eigenhat$values)
thetastar=numeric(B)
for(i in 1:B)
{
  index=sample(c(1:n),n,replace = TRUE)
  xstar=data[index,]
  cov=cov(xstar)
  eigen=eigen(cov)
  thetastar[i]=eigen$values[1]/sum(eigen$values)
}
bias=mean(thetastar)-thetahat
sd=sd(thetastar)
c(bias.boot=bias,sd.boot=sd)


## ----warning=FALSE------------------------------------------------------------
library(bootstrap)
stat<-function(x,i){
  cov=cov(x[i,])
  eigen=eigen(cov)
  theta=eigen$values[1]/sum(eigen$values)
  return(theta)
}

data=scor
n=nrow(data)
thetahat=stat(data,(1:n))

theta.jack=numeric(n)
for(i in 1:n){
  theta.jack[i]=stat(data,(1:n)[-i])
}
bias.jack <- (n-1)*(mean(theta.jack)-thetahat)
se.jack <- sqrt((n-1)*mean((theta.jack-mean(theta.jack))^2))
round(c(thetahat=thetahat,bias.jack=bias.jack,
se.jack=se.jack),4)


## ----warning=FALSE------------------------------------------------------------
library(boot)
library(bootstrap)
stat<-function(x,i){
  cov=cov(x[i,])
  eigen=eigen(cov)
  theta=eigen$values[1]/sum(eigen$values)
  return(theta)
}
data=scor
obj=boot(data,stat,R=2000)
ci <- boot.ci(obj,type=c("perc","bca"))
ci.perc<-ci$percent[4:5]
ci.bca<-ci$bca[4:5]
c(ci.perc=ci.perc,ci.bca=ci.bca)


## ----warning=FALSE------------------------------------------------------------
library(boot)
stat<-function(x,i){
  xbar=mean(x[i])
  m2=mean((x[i]-xbar)^3)
  m3=mean((x[i]-xbar)^2)
  return(m2/m3^1.5)
}
#normal populations (skewness 0)
m=1e3
n=20
u=1;sd=1
ci.basic<-matrix(NA,m,2)
ci.perc<-matrix(NA,m,2)
ci.norm<-matrix(NA,m,2)
for(i in 1:m){
data=rnorm(n,u,sd)
obj=boot(data,stat,R=999)
ci=boot.ci(obj,type=c("norm","basic","perc"))
ci.norm[i,]=ci$norm[2:3]
ci.basic[i,]=ci$basic[4:5]
ci.perc[i,]=ci$percent[4:5]
}
#正态分布N(1,1)的norm,basic,percent置信区间覆盖率
cat('normalnorm =',mean(ci.norm[,1]<= 0 & ci.norm[,2]>=0),
    'normalbasic =',mean(ci.basic[,1]<= 0 & ci.basic[,2]>=0),
'normalperc =',mean(ci.perc[,1]<=0 & ci.perc[,2]>=0))
#正态分布N(1,1)的norm,basic,percent置信区间左侧和右侧覆盖率
cat('normalnormleft =',mean(ci.norm[,1]<=0),
    'normalnormright =',mean(ci.norm[,2]>=0),
    'normalbasicleft =',mean(ci.basic[,1]<= 0),
    'normalbasicright =',mean(ci.basic[,2]>=0),
'normalpercleft =',mean(ci.perc[,1]<=0),
'normalpercright =',mean(ci.perc[,2]>=0))


#χ2(5) distributions 
df=5
skewness=sqrt(8/df)
ci.basic<-matrix(NA,m,2)
ci.perc<-matrix(NA,m,2)
ci.norm<-matrix(NA,m,2)
for(i in 1:m){
data=rchisq(n,df)
obj=boot(data,stat,R=999)
ci=boot.ci(obj,type=c("norm","basic","perc"))
ci.norm[i,]=ci$norm[2:3]
ci.basic[i,]=ci$basic[4:5]
ci.perc[i,]=ci$percent[4:5]
}
#卡方分布norm,basic,percent置信区间覆盖率
cat('chisqnorm =',mean(ci.norm[,1]<= skewness & ci.norm[,2]>=skewness),
    'chisqbasic =',mean(ci.basic[,1]<= skewness & ci.basic[,2]>=skewness),
'chisqperc =',mean(ci.perc[,1]<=skewness & ci.perc[,2]>=skewness))
#卡方分布norm,basic,percent置信区间的左侧和右侧的覆盖率
cat('chisqnormleft =',mean(ci.norm[,1]<= skewness),
    'chisqnormright =',mean(ci.norm[,2]>=skewness),
    'chisqbasicleft =',mean(ci.basic[,1]<= skewness),
    'chisqbasicright =',mean(ci.basic[,2]>=skewness),
'chisqpercleft =',mean(ci.perc[,1]<=skewness),
'chisqpercright =',mean(ci.perc[,2]>=skewness))



## -----------------------------------------------------------------------------
set.seed(345)
R=999
K=1:60
n<-30
x<-rnorm(n,0,1)
y<-rnorm(n,100,1)
z<-c(x,y)
t0_cortest<-cor.test(x,y)$statistic
p0_cortest<-cor.test(x,y)$p.value
t0_spearman<-cor(x,y,method = "spearman")
rep1=numeric(R)
rep2=numeric(R)
for(i in 1:R){
  k <- sample(K, size = n, replace = FALSE)
  x1 <- z[k]; y1 <- z[-k] 
  rep1[i] <- cor.test(x1, y1)$statistic
  rep2[i]<-cor(x1,y1,method = "spearman")
}
p_cortest <- mean(abs(c(t0_cortest, rep1)) >= abs(t0_cortest))
p_spearman <- mean(abs(c(t0_spearman, rep2)) >= abs(t0_spearman))
round(c(p0_cortest=p0_cortest,p_cortest=p_cortest,p_spearman=p_spearman),3)


## ----warning=FALSE------------------------------------------------------------
library(RANN) 
library(boot)
library(energy)
library(Ball)

#NN
set.seed(12345)
Tn <- function(z, ix, sizes,k) {
n1 <- sizes[1]; n2 <- sizes[2]; n <- n1 + n2
if(is.vector(z)) z <- data.frame(z);
z <- z[ix, ];
NN <- nn2(data=z, k=k+1) # What is the first column?
block1 <- NN$nn.idx[1:n1,-1]
block2 <- NN$nn.idx[(n1+1):n,-1]
i1 <- sum(block1 <= n1); i2 <- sum(block2 > n1)
(i1 + i2) / (k * n)
}
#Unequal variances and equal expectations  
n=30
x<-rnorm(n,0,1)
y<-rnorm(n,0,5)
z <- c(x, y)
N=c(length(x),length(y))
boot.obj <- boot(data = z, statistic = Tn, R = 999,
sim = "permutation", sizes = N, k=3)
ts <- c(boot.obj$t0,boot.obj$t)
NN_p.value <- mean(ts>=ts[1])

boot.obs <- eqdist.etest(z, sizes=N, R=999)
energy_p.value <- boot.obs$p.value
ball_p.value = bd.test(x = x, y = y, num.permutations=9999)$p.value

round(c(NN_p.value=NN_p.value,energy_p.value=energy_p.value,ball_p.value),4)

#Unequal variances and unequal expectations 
set.seed(12345)
n=30
x<-rnorm(n,0,1)
y<-rnorm(n,2,5)
z <- c(x, y)
N=c(length(x),length(y))
boot.obj <- boot(data = z, statistic = Tn, R = 999,
sim = "permutation", sizes = N, k=3)
ts <- c(boot.obj$t0,boot.obj$t)
NN_p.value <- mean(ts>=ts[1])

boot.obs <- eqdist.etest(z, sizes=N, R=999)
energy_p.value <- boot.obs$p.value
ball_p.value = bd.test(x = x, y = y, num.permutations=9999)$p.value

round(c(NN_p.value=NN_p.value,energy_p.value=energy_p.value,ball_p.value),4)

#t distribution with 1 df (heavy-tailed
#distribution), bimodel distribution (mixture of two normal
#distributions)
set.seed(345)
n=30
x<-rt(n,1)
y1<-rnorm(n,0,1)
y2<-rnorm(n,3,1)
k<-sample(c(0,1),n,replace = TRUE)
y<-k*y1+(1-k)*y2
z <- c(x, y)
N=c(length(x),length(y))
boot.obj <- boot(data = z, statistic = Tn, R = 999,
sim = "permutation", sizes = N, k=3)
ts <- c(boot.obj$t0,boot.obj$t)
NN_p.value <- mean(ts>=ts[1])

boot.obs <- eqdist.etest(z, sizes=N, R=999)
energy_p.value <- boot.obs$p.value
ball_p.value = bd.test(x = x, y = y, num.permutations=9999)$p.value

round(c(NN_p.value=NN_p.value,energy_p.value=energy_p.value,ball_p.value),4)

#Unbalanced samples (say, 1 case versus 10 controls)
set.seed(234)
n1=10;n2=100
x<-rnorm(n1,0,1)
y<-rnorm(n2,2,1)

z <- c(x, y)
N=c(length(x),length(y))
boot.obj <- boot(data = z, statistic = Tn, R = 999,
sim = "permutation", sizes = N, k=3)
ts <- c(boot.obj$t0,boot.obj$t)
NN_p.value <- mean(ts>=ts[1])

boot.obs <- eqdist.etest(z, sizes=N, R=999)
energy_p.value <- boot.obs$p.value
ball_p.value = bd.test(x = x, y = y, num.permutations=9999)$p.value

round(c(NN_p.value=NN_p.value,energy_p.value=energy_p.value,ball_p.value),4)


## -----------------------------------------------------------------------------
set.seed(12345)
m<-5000
sigma=0.5
x<-numeric(m)
x[1]<-rnorm(1,0,sigma)
k=0
u <- runif(m)

  
for(i in 2:m){
  y<- rnorm(1, x[i-1], sigma)
  if(u[i]<= (dt(y, 1) / dt(x[i-1], 1)) ) x[i]=y
  else {x[i]=x[i-1]
  k=k+1}
}

b<-1001
y1<-x[b:m]
a<-ppoints(10)
Qsample <- quantile(y1, a)
QR<-qt(a,1)
qqplot(QR, Qsample, main="",
xlab="Standard Cauchy Quantiles", ylab="Sample Quantiles")
res<-data.frame(QR,Qsample)
knitr::kable(res)



## -----------------------------------------------------------------------------
N=5000
burn=1000
X <- matrix(0, N, 2)
a=2
b=1
n=10
X[1,]=c(5,0.5)

for (i in 2:N) {
x2 <- X[i-1, 2]
X[i, 1] <- rbinom(1, n, x2)
x1 <- X[i, 1]
X[i, 2] <- rbeta(1,x1+a,n-x1+b)
}
b <- burn + 1
x <- X[b:N, ]
plot(x, main="", cex=.5, xlab=bquote(X[1]),
     ylab=bquote(X[2]), ylim=range(x[,2]))



## -----------------------------------------------------------------------------
#monitor convergence of the chain
Gelman.Rubin <- function(psi) {
  psi<-as.matrix(psi)
  m<-ncol(psi)
  n<-nrow(psi)
  psi.means<-rowMeans(psi)
  B<-n*var(psi.means)
  psi.w<-apply(psi,1,"var")
  W<-mean(psi.w)
  v.hat<-W*(n-1)/n+B/n
  r.hat<-v.hat/W
  return(r.hat)
}

cauchy.chain<-function(sigma,N,X1){
  x<-rep(0,N)
  x[1]<-X1
  u<-runif(N)
  
  for(i in 2:N){
    y<-rnorm(1,x[i-1],sigma)
    r=dt(y,1)/dt(x[i-1],1)
    if(u[i]<=r) x[i]=y
    else x[i]=x[i-1]
  }
  return(x)
}

set.seed(12345)
sigma=0.5
n<-20000
b<-1000
x0<-c(-10,-5,5,10)
X<-matrix(0,4,n)
for(i in 1:4)
  X[i,]=cauchy.chain(sigma,n,x0[i])

psi<-t(apply(X,1,cumsum))
for(i in 1:nrow(psi))
psi[i,] <- psi[i,] / (1:ncol(psi))


par(mfrow=c(2,2))
par(mar=c(2,2,2,1))
for (i in 1:4)
plot(psi[i, (b+1):n], type="l",
xlab=i, ylab=bquote(psi))
par(mfrow=c(1,1))
par(mar=c(5.1,4.1,4.1,2.1))

rhat <- rep(0, n)
for (j in (b+1):n)
rhat[j] <- Gelman.Rubin(psi[,1:j])
plot(rhat[(b+1):n], type="l", xlab="", ylab="R")
abline(h=1.2, lty=2)


## -----------------------------------------------------------------------------
Gelman.Rubin <- function(psi) {
  psi<-as.matrix(psi)
  m<-ncol(psi)
  n<-nrow(psi)
  psi.means<-rowMeans(psi)
  B<-n*var(psi.means)
  psi.w<-apply(psi,1,"var")
  W<-mean(psi.w)
  v.hat<-W*(n-1)/n+B/n
  r.hat<-v.hat/W
  return(r.hat)
}

gibbs.chain<-function(N,x0){
  X <- matrix(0, N, 2)
  a=2
  b=1
  n=10
  X[1,]=x0
  for (i in 2:N) {
x2 <- X[i-1, 2]
X[i, 1] <- rbinom(1, n, x2)
x1 <- X[i, 1]
X[i, 2] <- rbeta(1,x1+a,n-x1+b)
}
  return(X)
}

set.seed(8)
n<-20000
b<-1000
x0<-matrix(data=c(1,3,5,9,0.7,0.5,0.3,0.1),nrow = 4,ncol=2)
X1<-matrix(0,4,n)
X2<-matrix(0,4,n)
for(i in 1:4) {
  X1[i,]=gibbs.chain(n,x0[i,])[,1]
  X2[i,]=gibbs.chain(n,x0[i,])[,2]
}

psi1<-t(apply(X1,1,cumsum))
psi2<-t(apply(X2,1,cumsum))
for(i in 1:nrow(psi1)){
psi1[i,] <- psi1[i,] / (1:ncol(psi1))
psi2[i,] <- psi2[i,] / (1:ncol(psi2))
}

rhat1 <- rep(0, n)
rhat2 <- rep(0, n)
for (j in (b+1):n){
rhat1[j] <- Gelman.Rubin(psi1[,1:j])
rhat2[j] <- Gelman.Rubin(psi2[,1:j])
}
par(mfrow=c(1,2))
plot(rhat1[(b+1):n], type="l", xlab="", ylab="R")
abline(h=1.2, lty=2)
plot(rhat2[(b+1):n], type="l", xlab="", ylab="R")
abline(h=1.2, lty=2)

## -----------------------------------------------------------------------------

g<-function(k,d){
  return(
    exp(lgamma(k+3/2)+lgamma((d+1)/2)-lgamma(k+d/2+1))
    )
}


kth<-function(a,k){
  d<-length(a)
  anorm<-0
for(i in 1:d){
    anorm<-a[i]^2+anorm
}
  b=anorm^(k+1)/((2*k+1)*(2*k+2))
return((-1)^k*b*g(k,d)/(factorial(k)*2^k))
}



## -----------------------------------------------------------------------------
sumfun<-function(a,k){
  res<-0
  for(i in 1:(k+1)){
    res<-res+kth(a,i-1)
  }
  return(res)
}


## -----------------------------------------------------------------------------
a<-c(1,2)
k=c(10,100,100)
funsum<-numeric(3)
for(i in 1:3){
  funsum[i]=sumfun(a,k[i])
}
res<-data.frame(k,funsum)
knitr::kable(res)


## -----------------------------------------------------------------------------
sa<-function(a,k){
  ck=sqrt(a^2*k/(k+1-a^2))
  res=1/2-pt(ck,k,lower.tail = F)
  return(res)
}
f<-function(a,k){
  return(sa(a,k)-sa(a,k-1))
}

k<-c(4:25,100,500,1000)
a <- seq(0, 4, by=0.01)
plot(a, f(a, k[23]), lty=1, col=1, type="l", xlim=c(0, 4), xlab="a", ylab="f(a|k)", main="f(a) with different k")
lines(a, f(a, k[24]), xlim = c(0, 4), lty=2, col=2)
lines(a, f(a, k[25]), xlim = c(0, 4), lty=3, col=3)
legend("topright", legend=c("k=100", "k=500", "k=1000"), col=1:3,lty=1:3)


solve <- function(k){
  output = uniroot(function(a){sa(a,k)-sa(a,k-1)},lower=1,upper=2)
  output$root
}

root<-matrix(0,2,length(k))
root[1,]=k
for(i in 1:length(k)){
  root[2,i]<-solve(k[i])
}

rownames(root) = c('k','A(k)')
print(root)


## -----------------------------------------------------------------------------

em<-function(lambda0){
  e<-0.0001
  n=10000
for(i in 1:n){
  lambda=(3*(1+lambda0)+3.75)/10
  if(lambda-lambda0>e)
    lambda0=lambda
  else
    break
 }
return(lambda)
}
res<-matrix(0,2,4)
  lambda0=c(0.2,0.5,1,2)
  res[1,]=lambda0
for(i in 1:4){
res[2,i]=em(lambda0[i])
}
  rownames(res)=c('lambda0','lambdahat')
  print(res)

