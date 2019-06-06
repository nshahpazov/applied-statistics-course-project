# load the data

load("lung.Rdata")
str(lung)
# medv is median value of owner occupied homes in $1000s
# lstat is the percentage of lower status people
plot(LungCap~Age, lung)
attach(lung)

# is there a relationship netween age and capacity?
cor(Age, LungCap)

# linear model
model = lm(LungCap~Age, data=lung)
plot(Age, LungCap)
abline(model, col='red')

# checking assumptions for simple linear regression model
# 1. the y values are independent
# 2. the y values can be expressed as a linear function of x variables
# 3. variation around the regression line is constant (homoscedasticity)
# 4. for a given value of x, y values are normally distributed

par(mfrow=c(2,2))
plot(model)

# conclusion: residuals seem to be normal

# residuals plot
plot(Age,model$residuals, pch=20,col="blue")
abline(h=0, lwd=3)
# as we can see the residuals are most likely normally distributed
anova(model)

# interpreting the summary of linear model
# to-do: read the articles about interpreting the 
summary(model)


# lungcap ~ height + age model
model2 = lm(LungCap ~ Height + Age)
summary(model2)

# multiple R-squared is 0.84 which means that approximately 
# 84% of the variation in capacity can be explained with age and height


# F-statistic:  1938 on 2 and 722 DF,  p-value: < 2.2e-16
# This hypothesis tests whether all the model coefficients are 0
# in our case, it tests whether the age and height coefficients are 0
# The p-value is very small, so we reject the null hypothesis that they
# are 0

# Residual standard error: 1.056 on 722 degrees of freedom
# This gives us an idea of how far the predicted (fitted value) 
# are from the observed ones, sigmahat = sqrt(RSS / (n - r))

# (Intercept) -11.747065 :- this tells us the expected 
# lung capacity when all Xs, i.e. Height and Age are 0s
#  which in our case doesn't make sense since the height and age cannot be 0
# To give a meaningful interpretation to the intercept we should center
# the height and age

# Age 0.12636 *** This is the effect of Age
# to lung capacity, meaning with increase of 1 year (unit) in age
# we get an increase of 0.126368 of lung capacity. p-value for the
# slope of age is < 2e-16 *** which means that we reject the null hypothesis
# that the slope of age is 0, i.e. that age doesn't have effect on lung
# capacity.

# Height coefficient is 0.278432 which is the estimated effect of height
# to lung capacity, meaning that with with 0.278432 increase in inches
# we get 0.278432 increase in lung capacity. the p-value is again < 2e-16 ***
# which again means that we reject the hypothesis that the height doesn't 
# have effect on the lung capacity.

# Confidence Interval
confint(model2)
# we are 95% sure that the effect of age is between
# 0.09 and 0.16 and we are 95% sure that theeffect of
# the Height is between 0.25894454 0.2979192


######## Conclusions ##########
# We have a strong relationship between the explanatory variables 
# and the dependent variable Y, i.e. Lung Capacity.
