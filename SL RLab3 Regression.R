#### 3.6 R Lab: Linear Regression
# library() fuction load library, each time to use package
# Datasets and data associated with textbook (ISL)
library(MASS)
library(ISLR)
install.packages('ISLR')

# Examine Boston dataset; use ?Boston command for details
fix(Boston)
names(Boston)
dim(Boston)

#### SIMPLE LINEAR REGRESSION
# Plot median home value, lower SES for Boston data
plot(medv~lstat,data=Boston)
attach(Boston)
plot(medv~lstat,col='red')
plot(medv~lstat,pch=20)
plot(medv~lstat,pch='*')

# Fit simple linear regression model of medv(Y) on lstat(X)
fit1=lm(medv~lstat, data=Boston)
fit1
summary(fit1)

# fit regression line on scatterplot
abline(fit1,col='red', lwd='3')

names(fit1)
# Obtain 95% CI for each coefficient 
confint(fit1)

# predict function returns CI for given value of X
predict(fit1, data.frame(lstat=c(5,10,15)), interval='confidence')

# Diagnostic plots in quad
par(mfrow=c(2,2))
plot(fit1)

par(mfrow=c(1,1))

#### MULTIPLE LINEAR REGRESSION
# Regression of median value on lower status and age of home
fit2 = lm(medv~lstat+age, data=Boston)
summary(fit2)

# Fit all predictors into model
fit3 = lm(medv~., Boston)
summary(fit3)
# Get individual items from summary, R^2, and RSE
summary(fit3)$r.sq
summary(fit3)$sigma

# Diagnostic plots in quad for all predictors
par(mfrow=c(2,2))
plot(fit3)
par(mfrow=c(1,1))

# Update command alters model, remove predictors: age and indus 
fit4 = update(fit3, ~.-age-indus)
summary(fit4)

## NON-LINEAR TERMS and INTERACTIONS
fit5 = lm(medv~lstat*age, Boston)
summary(fit5)

# Transformation of nonlinear variable, quadratic term: I(X^2)
# two commands included in one line; separated by semi-colon
fit6 = lm(medv~lstat+I(lstat^2), Boston); summary(fit6)
fit6

# Plotting predictors and outcome variable, using quadratic term
attach(Boston)
par(mfrow=c(1,1))

# Plot outcomes as points, fitted values (lstat^2), as filled circles
plot(medv~lstat)
points(lstat, fitted(fit6), col='red', pch=20)

# Easy way to fit polynomials using poly() function 
fit7=lm(medv~poly(lstat,4))
points(lstat, fitted(fit7), col='blue', pch=20)

# Show plotting characters
plot(1:20, 1:20, pch=1:20, cex=2)

### QUALITATIVE PREDICTORS
fix(Carseats)
names(Carseats)
# Summary shows qualitative predictors ("ShelfLoc", ‘Urban’, ‘US’)
summary(Carseats)

# Run Multiple Regression of Sales on Predictors with Interactions
fit1 = lm(Sales~.+Income:Advertising+Age:Price, data=Carseats)
summary(fit1)
contrasts(Carseats$ShelveLoc)

### WRITING R FUNCTIONS
# function takes x, y as arguments, fits linear model, creates plot
# Rewrite regplot function with "..." to include extra arguments
regplot=function(x,y,...){
    fit=lm(y~x)
    plot(x,y,...)
    abline(fit,col='red', lwd=2)
}
attach(Carseats)
regplot(Price, Sales, xlab="Price", ylab="Sales", col="blue", pch=20)


lm(Sales~Price*Age)
lm(Sales~I(Price*Age))

