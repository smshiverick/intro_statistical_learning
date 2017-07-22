#### Lab 4.6: CLASSIFICATION
## Logistic Regression, Linear Discriminant Analysis (LDA)
## Quadratic Discriminant Analysis (QDA) and K-Nearest Neighbors (KNN)

# Load MASS andn ISLR library() with associated Datasets 
library(MASS)
library(ISLR)
names(Smarket)
dim(Smarket)
summary(Smarket)
?Smarket

# Obtain plot matrix for features, by color for Binary response
pairs(Smarket,col=Smarket$Direction)

# Correlation matrix for features, variables
# Without binary variable: Direction (up, down)
cor(Smarket[,-9])

# LOGISTIC REGRESSION: fit using general linear model glm() function
glm.fit=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, 
            data=Smarket,family=binomial)
summary(glm.fit)

# Make Predictions from the fitted model (glm.fit)
# Predictions on Training Data used to fit the model
glm.probs = predict(glm.fit, type="response")
glm.probs[1:5]

# Returns vector of fitted probabilities, all close to 50%
# PREDICTION: whether market will go UP or DOWN based on predictors

# Turn predictions into classifications, thresholding at 0.5 
# Using the ifelse command: If > 0.5, then ‘UP’, else “DOWN”
glm.pred = ifelse(glm.probs > 0.5, "Up", "Down")

# Look at performance, make table of prediction by True Direction
attach(Smarket)
table(glm.pred, Direction)

# Get mean classification performance
mean(glm.pred==Direction)

# Split Dataset into TRAIN and TEST Subsets
# Boolean vector returns as TRUE or FALSE by Year: 
train = Year < 2005
# Fit logistic model on TRAIN data subset
glm.fit = glm(Direction~ Lag1+Lag2+Lag3+Lag4+Lag5+Volume,
              data=Smarket, family=binomial, subset=train)

# Obtain probabialities, run model on TEST model subset [!train]
glm.probs = predict(glm.fit, newdata=Smarket[!train,], type="response")
glm.pred = ifelse(glm.probs > 0.5, "Up", "Down")

# Create new Variable for Test Data, and prediction table
Direction.2005 = Smarket$Direction[!train]
table(glm.pred, Direction.2005)

# Get mean classification performance for TEST subset
mean(glm.pred==Direction.2005)

## Fit a SMALLER model: Lag1, Lag2
glm.fit = glm(Direction~ Lag1+Lag2, data=Smarket, 
              family=binomial, subset=train)

# Run model on TEST set [!train]
glm.probs = predict(glm.fit, newdata=Smarket[!train,], type="response")
glm.pred = ifelse(glm.probs > 0.5, "Up", "Down")

# Create prediction table and mean performance for TEST subset
table(glm.pred, Direction.2005)
mean(glm.pred==Direction.2005)

#### 4. MULTIVARIATE LOGISTIC REGRESSION
β0=-6
β1=0.05
β2=1
X1=40
X2=3.5
exp = β0 + β1*X1 + β2*X2 
e = 2.71828
num = e^exp
denom = 1 + e^exp
prx = num / denom
prx

#### LINEAR DISCRIMINANT ANALYSIS (LDA)
# Fit LDA with lda() function which is part of MASS library
library(MASS)
?lda
# Fit model on TRAIN subset
lda.fit = lda(Direction~Lag1+Lag2, data=Smarket, subset=Year<2005)
lda.fit

# Plot LDA fit, discriminant function separately for each group
plot(lda.fit)

# Now, run model on TEST subset of Smarket data, call predict on LDA
Smarket.2005 = subset(Smarket, Year==2005)
lda.pred = predict(lda.fit, Smarket.2005)
lda.pred[1:5,]

# Variable was returned as list, save as DataFrame
class(lda.pred)
data.frame(lda.pred)[1:5,]

# Create table of predicted class versus the true class, get mean
table(lda.pred$class, Smarket.2005$Direction)
mean(lda.pred$class==Smarket.2005$Direction)

# QUADRATIC DISCRIMINANT ANALYSIS (QDA)
# Fit QDA model to Smarktet data using qda() function in MASS library
# Syntax is identical to lda()
qda.fit = qda(Direction~Lag1+Lag2, data=Smarket, subset=Year<2005)
qda.fit

# Plot LDA fit, discriminant function separately for each group
plot(qda.fit)

# Run qda model on TEST subset of Smarket data, call predict on LDA
qda.pred = predict(qda.fit, Smarket.2005)
data.frame(qda.pred)[1:5,]

# Create table of predicted class versus the true class, get mean
table(qda.pred$class, Smarket.2005$Direction)
mean(qda.pred$class==Smarket.2005$Direction)


#### K-NEAREST NEIGHBORS (KNN)
# Simple classification rule, effective much of the time (1/3)
library(class)
?knn

# Assign first two days to composite variable, add training subset
attach(Smarket)
Xlag = cbind(Lag1, Lag2)
Xlag[1:5,]

# Call KNN on TRAIN and TEST subsets [!train], response class factor, groups
train = Year<2005
knn.pred = knn(Xlag[train,], Xlag[!train,], Direction[train], k=1)

# Create prediction table and mean performance for TEST subset
table(knn.pred, Direction[!train])
mean(knn.pred==Direction[!train])

# Rerun analysis using K=3
knn.pred = knn(Xlag[train,], Xlag[!train,], Direction[train], k=3)
table(knn.pred, Direction[!train])
mean(knn.pred==Direction[!train])


#### EXAMPLE 4.6.6 (p.165 in textbook)
# APPLICATION TO CARAVAN INSURANCE DATA
library(ISLR)
dim(Caravan)
attach(Caravan)
summary(Purchase)

# Standardize variables, so all have mean=0, sd=1
standardized.x = scale(Caravan[,-86])
var(Caravan[,1])
var(Caravan[,2])
var(standardized.x[,1])
var(standardized.x[,2])

# Split observations into TRAIN and TEST sets
test = 1:1000
train.X = standardized.x[-test,]
test.X = standardized.x[test,]
train.Y = Purchase[-test]
test.Y = Purchase[test]
set.seed(1)

# Class KNN on TRAIN, TEST data, for Purchase outcome, with k=1
knn.pred = knn(train.X, test.X, train.Y, k=1)
mean(test.Y!=knn.pred)
mean(test.Y!="No")

# Create table for predicted versus actual insurance purchase
table(knn.pred, test.Y)
10 / (68+10)

# Increasing k can improve prediction performance
# Rerun KNN on TRAIN, TEST data, with k=3
knn.pred = knn(train.X, test.X, train.Y, k=3)
table(knn.pred, test.Y) 

# Rerun KNN on Purchase outcome, with k=5
knn.pred = knn(train.X, test.X, train.Y, k=5)
table(knn.pred, test.Y)

# By Comparison, if we fit LINEAR LOGISTIC REGRESSION model to data
# With predicted probability threshold set at 0.5
glm.fit = glm(Purchase~., data=Caravan, family=binomial, subset=-test)
glm.probs = predict(glm.fit, Caravan[test,], type="response")
glm.pred = rep("No", 1000)
glm.pred[glm.probs > 0.5] = "Yes"
table(glm.pred, test.Y)

# Predict purchase any time predicted probability exceeds threshold 0.25
glm.pred = rep("No", 1000)
glm.pred[glm.probs > 0.25] = "Yes"
table(glm.pred, test.Y)
11 / (22 + 11)
