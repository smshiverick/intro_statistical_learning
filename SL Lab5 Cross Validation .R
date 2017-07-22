#### Lab 5.3: CROSS VALIDATION AND THE BOOTSTRAP
## The Validation Set Approach

# Load ISLR and Bootstrap applications using library()  
library(ISLR)

# Cross-Validation for Generalized Linear Models cv.glm(data, glmfit, cost, K)
?cv.glm
plot(mpg~horsepower,data=Auto)

# "Leave-one-out" Cross Validation (LOOCV)   
# fit linear model (default is linear if you don't enter 'family')
glm.fit = glm(mpg~horsepower, data=Auto)

# LOOCV Fits linear model repeatedly n times; each time leaving out one observation, 
# produces fit for remaining data. Delta is cross-validation prediction model
# pretty slow because it doesn’t use formula (5.2) on page 180
cv.glm(Auto,glm.fit)$delta 

## Write simple function for the formula (5.2) on page 180
loocv = function(fit){
    h=lm.influence(fit)$h
    mean((residuals(fit)/(1-h))^2)
}

## Run LOOCV again using this function on the linear model
loocv(glm.fit)

# Use this result to fit polynomials of degrees 1-5 to data
# Create a vector for collecting the errors, range of degrees
# Loop over degrees, fitting the model, collecting the errors in vector
cv.error = rep(0,5)
degree = 1:5
for(d in degree){
    glm.fit = glm(mpg~poly(horsepower,d), data=Auto)
    cv.error[d] = loocv(glm.fit)
}
plot(degree,cv.error,type="b")

## K-fold Cross-Validation with K=10 ("10-fold CV")
# Divide data into 10 pieces, 9 training sets and 1 test set
# Repeat process 10 times unit each piece used as test set: 10 results
cv.error10 = rep(0,5)
for(d in degree){
    glm.fit=glm(mpg~poly(horsepower,d), data=Auto)
    cv.error10[d]=cv.glm(Auto,glm.fit,K=10)$delta[1]
}
lines(degree,cv.error10,type="b",col="red")


# BOOTSTRAP Example: Minimum risk investment - Section 5.2
# Method for estimating error for Sampling Distributions from Sample (TRAIN set)
# Write function alpha that minimizes error for investment risk
alpha = function(x,y){
    vx = var(x)
    vy = var(y)
    cxy = cov(x,y)
    (vy-cxy)/(vx+vy-2*cxy)
}
alpha(Portfolio$X,Portfolio$Y)

## What is Estimated Standard Error for alpha?
# Function indexes rows of dataFrame, computes statistic
# Index of numbers 1-n, with replacement
# with data in index, compute alpha for X, Y
alpha.fn = function(data, index){
    with(data[index,], alpha(X,Y))
}
# Run function once to see that it works
alpha.fn(Portfolio,1:100)

# Set random number seed to obtain consistent sample
set.seed(1)
alpha.fn(Portfolio, sample(1:100,100,replace=TRUE))

# Run Bootstrap for 1000 iterations
boot.out = boot(Portfolio, alpha.fn, R=1000)
boot.out
plot(boot.out)



#### QUIZ 5.R.R1
# 1. load it into R using load("5.R.RData"). 
# Consider the linear regression model of y on X1 and X2. 
# What is the standard error for β1
data = load('5.R.RData')
dim(Xy)
names(Xy)
glm.fit = glm(y~X1+X2, data=Xy)
summary(glm.fit)

# Next, plot the data using matplot(Xy,type="l"). 
# Which is most likely given what you see?
matplot(Xy,type="l")
Xy['X1']

# Now, use the (standard) bootstrap to estimate s.e.(β^1). 
# To within 10%, what do you get?

# This first try gave an estimated Bootstrap SE = 0.05, which is too big. 
# Second try below seems to provide a result that is more consistent with expectation

alpha.fn = function(data, index){
    X=Xy$X1[index]
    Y=Xy$y[index]
    return((var(Y)-var(X*Y))/(var(X)+var(Y)-2*cov(X,Y)))
}
# Run function once to see that it works
alpha.fn(data,1:100)

# Set random number seed to obtain consistent sample
set.seed(1)
alpha.fn(Xy, sample(1:100,100,replace=TRUE))

# Run Bootstrap for 1000 iterations
boot.out = boot(Xy, alpha.fn, R=1000)
boot.out




########### TRY AGAIN: Standard BOOTSTRAP
# BOOTSTRAP Example: Minimum risk investment - Section 5.2
# Method for estimating error for Sampling Distributions from Sample (TRAIN set)
# Write function alpha that minimizes error for investment risk
alpha = function(x,y){
    vx = var(x)
    vy = var(y)
    cxy = cov(x,y)
    (vy-cxy)/(vx+vy-2*cxy)
}
alpha(Xy$X1,Xy$y)

## What is Estimated Standard Error for alpha?
alpha.fn = function(Xy, index){
    with(Xy[index,], alpha(X1,y))
}
# Run function once to see that it works
alpha.fn(Xy,1:100)

# Set random number seed to obtain consistent sample
set.seed(1)
alpha.fn(Xy, sample(1:100,100,replace=TRUE))

# Run Bootstrap for 1000 iterations
boot.out = boot(Xy, alpha.fn, R=1000)
boot.out

summary(lm(y~X1, data=Xy))$coeff

boot.fn=function(Xy, index)
    coefficients(lm(y~X1+X2, data=Xy, subset=index))
set.seed(3)
boot(Xy, boot.fn,1000)
