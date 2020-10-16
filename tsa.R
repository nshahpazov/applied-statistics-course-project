library(TSA)
data(larain)
plot(larain, ylab='Inches', xlab='Year', type='o')

# example 1

# zlag makes the kth element the k-d elements i.e.
# a new vector v' for which v'[k] = v[k-d]
zlag(1:7, d=2)

plot(y=larain, x=zlag(larain), ylab='Inches', xlab='Year')

# so in this case the plot will show y values according to y-1 values
# if there's an increasement in the ys w.r.t. last year there's going to be a view 
# of correlation. From this I imply there's an autoregressive correlation
# So in a case it's going to show whether increasing in rain in last year
# is going to increase this year's rainfall

# example 2 - color property of a chemical process
data(color)
plot(color,ylab='Color Property', xlab='Batch', type='o')
plot(y=color, x = zlag(color), ylab='Color Property', xlab='Batch')
# Observations:
# low values tend to be followed in the next batch by low values, 
# middle-sized values tend to be followed by middle-sized values, 
# and high values tend to be followed by high values

# Question: how to calculate correlation between y and y-1

# example 3
data(hare); plot(hare,ylab='Abundance',xlab='Year',type='o')
plot(y=hare, x=zlag(hare),ylab='Abundance', xlab='Year')
# again we see low values in previous year give low values in current year
# and high values in previouos year give high in current year


# everything should be made as simple as possible but not simpler.
# First part of the time series analysis is to make a good plot and analyze what it says

# General TSA Strategy
# 1. model specification (or identification)
# 2. model fitting, and
# 3. model diagnostics

# random walk example
plot(cumsum(rnorm(1000)), type="l") # manual example
data(rwalk)
plot(rwalk, type="o")


# example cosinus
# question: cannot understand this example and the periodicity
plot(cos(x = 2*pi*(x/12 + runif(1))))

