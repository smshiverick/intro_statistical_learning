### Ch 2.3 Lab: Introduction to R
# Vectors, data, matrices, subsetting
# Basic commands, function, assignment: 

# Create vestor with c() for concatenate
x <- c(2, 7, 5)
x
y = seq(from=4, length=3, by=3)
y
?seq

# R does vector operations in parallel, element to element
x+y
x/y
x^y

# Subsetting, indexing
x[2]
x[2:3]

# Remove items from vector with negative sign in brackets
# No scalars in R, scalar is a vector of 1
x[-2]   
x[-c(1,2)]

# Matrix is two dimensional array
# First argument is numbers, and the dimensions of matrix
# Create matrix with 4 rows and 3 columns
# Default by column order, can select fill by rows
z = matrix(seq(1,12), 4, 3)
z
z = matrix(seq(1,12), 4, 3, byrow=TRUE)
z

# Subsetting, Indexing, or Slicing elements of a matrix
# Return third and fourth rows, second and third columns
z[3:4, 2:3]
# Return second and third column, across all rows
z[, 2:3]
# First column of z, returned as vector
z[,1]
# drop argument preserves matrix status of column
z[,1, drop=FALSE]

# Query the dimension of a matrix
dim(z)
# List contents of working directory
ls()

# Generate random data
x=rnorm(50)
y=x+rnorm(50,mean=50, sd=0.1)

# Reproduce exact set of data with set.seed()
set.seed(1303)
rnorm(50)

set.seed(3)
y=rnorm(100)
mean(y)
var(y)
sqrt(var(y))
sd(y)

## Generating random data, graphics
x = runif(50)
y = rnorm(50)
plot(x,y)
plot(x,y,xlab="randomUniform", ylab="Random Normal", pch='*', col="Blue")

# split plot frame using par (partition)
par(mfrow=c(2,1))
plot(x,y)
hist(y)
# reset plot frame to single plot
par(mfrow=c(1,1))

## Reading in data from file
Auto=read.csv("Auto.csv")
names(Auto)
dim(Auto)
class(Auto)
summary(Auto)
summary(mpg)

# plot elements of the DF
plot(Auto$cylinders, Auto$mpg)
plot(Auto$cyl, Auto$mpg)

# Create workspace for DF using attach()
attach(Auto)
search()

# Use plot command more directly, cyl,mpg as boxplot
plot(cylinders, mpg)

# Convert qualitative variable into qualitative variable
cylinders=as.factor(cylinders)

# Customizing plots
plot(cylinders, mpg, col='red')
plot(cylinders, mpg, col='red', varwidth=T)
plot(cylinders, mpg, col='red', varwidth=T, horizontal=T)
plot(cylinders, mpg, col='red', varwidth=T, xlab='cylinders', ylab='MPG')

# Histograms
hist(mpg)
hist(mpg, col=2, breaks=15)

# Create scatterplot matrix for every pair of variables
pairs(Auto)
pairs(~mpg + displacement + horsepower + weight + acceleration, Auto)

# Identify value for particular variable for points on a plot
plot(horsepower, mpg)
identify(horsepower, mpg, name)


