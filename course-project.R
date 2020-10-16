install.packages("TSA")
install.packages("rmaf")
install.packages("itsmr")
install.packages("portes")
install.packages("spgs")
install.packages("StatMethRank")

library("TSA")
library("rmaf")
library("itsmr")
library("portes")
library("spgs")
library("StatMethRank")

# plotting a function
eq = function(x){0.9^x}
plot(eq(seq(1, 30)), type='l')

# a function for moving average smoothing
ma = function(x, n = 5) { filter(x, rep(1 / n, n), sides=2)}

# function for exponential smoothing
exp2 = function(x, alpha) { 
  xalpha = x * alpha
  xalpha[1] = x[1]
  filter(xalpha, filter=1-alpha, method = "recursive")
}

x = arima.sim(n = 1000, list(ar = c(0.3, 0.5)))
eacf(x)
plot.ts(x)
# to check for autoregression we plot:
plot(y=x, x=zlag(x))

y = arima.sim(n = 400, list(ma = c(0.8, 0.6, 0.4)), rand.gen = rnorm, sd=4)
plot.ts(y)

# trend estimation and elimination
# moving average smooting
z = y + seq(1:400)
z2 = rnorm(400, sd=35) + seq(1:400)

# test for trend
rank.test(z2)

# eliminating trend by difference operator
difference = z2-zlag(z2)
plot(z2-zlag(z2), type="l")
plot(z2, type="l")
plot(ma(z2, n=30))
plot(z2-exp2(z, alpha=0.05))

plot(z2-ma(z2, n=30))
d = ma.filter(z, q = 3, seasonal = FALSE, period = NULL)
plot(z-d[,2])
z-d[,2] == d[,3]


# seasonal component elimination
z3 = rnorm(400, sd=35) + seq(1:400) + sin(1:400)
plot(ma(z3, n=56), type="l", col="red")
plot(z3, type="l")
d.z3 = z3 - zlag(z3)
ma.z3 = z3-ma(z3, n=56)
plot(d.z3,type="l")
plot(ma.z3,type="l")

# removal though differencing
plot.ts(deaths)
d12 = diff(deaths, differences = 12)
plot.ts(d12)
abline(h=mean(d12))
acf(d12)


# testing for independence
# Ljung-Box or Box-Pierce tests checking whether or not the
# residuals appear to be white noise.
# turning point test


# TODO: learn how to use this: LB.test(d12)
BoxPierce(d12)
independence.test(d12, method = c("spearman", "kendall"))
# test for trend
rank.test(z2)
# z2 has a trend

# testing wether the data is i.i.d noise
Box.test(d12, lag = 1, type = "Ljung-Box")
# we reject the hypothesis that the data is i.i.d

# check whether the residuals data is normally distributed
qqnorm(d12)
qqline(d12)
shapiro.test(d12)
shapiro.test(z2-zlag(z2))

# To do: understand q-q plots
# To do: understand hypothesis testing
# To do: understand ks test and how to use it
# To do: send problem solution to prof. Doncho Donchev


# tempdub
library(TSA)
plot.ts(tempdub)
tempdub
differenced = diff(tempdub, differences = 12)
plot.ts(differenced)
plot(x=zlag(differenced), y=differenced)
acf(diff(tempdub, differences = 12))

data("rwalk")
model1 = lm(rwalk~time(rwalk))
abline(model1)

# temp
month = TSA::season(tempdub)
model2 = lm(tempdub~month-1)
points(t, fitted(model2), col='red', pch=20)
lines(sort(t), fitted(model2)[order(t)], col='red', type='b') 
summary(model2)

t = time(tempdub)
# you can use harmonic(t, 1) which will give the same as the below
model3 = lm(tempdub~cos(2*pi*t) + sin(2*pi*t))
plot(tempdub, type="points")
points(t, fitted(model3), col='red', pch=20)
# remember this, this is a way of plotting the fitted model of 
# linear regression with transformed variables
lines(sort(t), fitted(model3)[order(t)], col='red', type='b') 
plot(cos(t*23))

# R^2
model12 = lm(rwalk~time(rwalk))
summary(model12)

plot(y=ma2.s, x=zlag(ma2.s, 2), ylab=expression(Y[t]), xlab=expression(Y[t-2]))

# the actual Course Project















