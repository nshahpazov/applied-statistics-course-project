myocarde = read.table("http://freakonometrics.free.fr/myocarde.csv",head=TRUE, sep=";")
myocarde$PRONO = (myocarde$PRONO=="SURVIE")*1
y = myocarde$PRONO
X = as.matrix(cbind(1,myocarde[,1:7]))

y = myocarde$PRONO
X = cbind(1,as.matrix(myocarde[,1:7]))

nll = function (beta) {
  -sum(-y*log(1 + exp(-X%*%beta))-(1-y)*log(1 + exp(X%*%beta)))
}

# we use OLS as a starting point for the parameters
beta_init = lm(PRONO~.,data=myocarde)$coefficients

lo = optim(par=beta_init, nll, hessian = TRUE, method = "BFGS", control = list(abstol=1e-9))

# Let us verify that our solution is ok by using another starting point
simu = function (i) {
  logistic_opt_i = optim(par = rnorm(8, 0, 3) * beta_init, 
                         nll, hessian = TRUE, method = "BFGS", 
                         control=list(abstol=1e-9))
  logistic_opt_i$par[2:3]
}

v_beta = t(Vectorize(simu)(1:1000))
plot(v_beta)
par(mfrow=c(1,2))
hist(v_beta[,1],xlab=names(myocarde)[1])
hist(v_beta[,2],xlab=names(myocarde)[2])

# it's probably supposed to not be binomial with various results
