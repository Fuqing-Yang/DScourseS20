library(nloptr)


set.seed(100)#Q4

N <- 100000
K <- 10
sigma <- 0.5

X <- matrix(rnorm(N*K,mean=0,sd=sigma), N, K)

X[,1] <- 1

eps <- rnorm(N,mean=0,sd=sigma)
eps <- rnorm(N,mean=0,sd=0.5)
betaTrue <- c(1.5,-1,-0.25, 0.75, 3.5, -2, 0.5,1, 1.25, 2)
y <- X%*%betaTrue + eps

beta_OLS <- solve(t(X)%*%X)%*%(t(X)%*%Y)#Q5

alpha <- 0.0000003#Q6
gradient <- function(beta,y,X) {
  return ( as.vector(-2*t(X)%*%(y-X%*%beta)) )
}

iter <- 1
beta_0 <- 0*beta_OLS
beta <- runif(dim(X)[2])
while (norm(as.matrix(beta0)-as.matrix(beta))>1e-8) {
  beta_0 <- beta
  beta <- beta_0 - alpha*gradient(beta0,Y,X)
  if (iter%%10000==0) {
    print(beta)
  }
  iter <- iter+1
}

print(iter)
print(paste("The minimum of f(beta_GD,Y,X) is ", beta, sep = ""))

#Q7
objfun <- function(beta,y,X) {
  return (sum((y-X%*%beta)^2))
}

beta0 <- runif(dim(X)[2])

result <- nloptr( x0=beta0,eval_f=objfun,eval_grad_f=gradient,opts=options,y=y,X=X)


options <- list("algorithm"="NLOPT_LD_LBFGS","xtol_rel"=1.0e-8,"maxeval"=1e3)

result <- nloptr( x0=beta0,eval_f=objfun,eval_grad_f=gradient,opts=options,y=y,X=X)

result_NM <- result$solution

#Q8
objfun  <- function(theta,y,X) {
  beta    <- theta[1:(length(theta)-1)]
  sig     <- theta[length(theta)]
  loglike <- -sum( -.5*(log(2*pi*(sig^2)) + ((y-X%*%beta)/sig)^2) ) 
  return (loglike)
}

gradient <- function (theta ,y,X) {
  grad <- as.vector (rep (0, length (theta )))
  beta <- theta [1:( length ( theta) -1)]
  sig <- theta [ length (theta )]
  grad [1:( length ( theta) -1)] <- -t(X)%*%(y - X%*%beta)/(sig ^2)
  grad[ length (theta )] <- dim(X)[1]/sig - crossprod (y-X%*%beta)/(sig^3)
  return ( grad )                                                 
}

beta0 <- runif(dim(X)[2]+1)
options <- list("algorithm"="NLOPT_LD_LBFGS","xtol_rel"=1.0e-6,"maxeval"=1e4)
result <- nloptr( x0=beta0,eval_f=objfun,eval_grad_f=gradient,opts=options,y=y,X=X)
print(result)

#Q9
Estimate<- lm(y ~ X -1)
summary(Estimate)
beta_lm <- Estimate$coefficients
library(stargazer)
stargazer(Estimate,beta_lm)


