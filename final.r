#!/bin/R -f

# The first step, is to read in the dataset we will be working with

data <- read.table(
  file="data.tsv",
  header=TRUE,

  # Field Separator
  sep='\t',

  # Types of value in each column
  colClasses=c(
    "character",
    "character",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric"
  )
)

# Split the data into frames of the location, the Easter data, and the Friday data
Loc <- data[,1:2]
Fri <- data[,3:15]
Eas <- data[,16:28]

# Next, to transform the data set into a binomial variable for each day, for each city
#
# s is the (hardcoded) preset proportion level for determining if a day is "rainy"
s <- 1/3

# Function to compute the mean, ignoring NA values
mean.na <- function(v) mean(v[!is.na(v)])

bFri <- as.numeric(apply(Fri, 1, mean.na) > s) # numeric() casts the logical value into {0,1}
bEas <- as.numeric(apply(Eas, 1, mean.na) > s)

# Now, transform the values into our test statistic:

#    /  1 if it is raining on Friday, and Sunny on Easter
#  p {
#    \  0 otherwise
bP   <- as.numeric(bFri & !bEas)

# Now, join those values into the combined data frame
data <- data.frame(Loc$City, bP)

###############################################################################
#                                Hypothesis Test                              #

n    <- length(bP)
s    <- sd(bP)
pHat <- mean(bP)

# The Alternative Hypothesis is that p > 0.5; so this is a single-tailed-test
p0   <- 0.25
a    <- 0.05

pp   <- pHat
B    <- pnorm(
          (p0 - pp + qnorm(a, lower.tail=FALSE)*sqrt(p0*(1-p0)/n))/sqrt(pp*(1-pp)/n),
          lower.tail=TRUE
        )
power <- 1-B

# Compute the test statistic
z      <- (pHat - p0)/sqrt(p0*(1-p0)/n)
pValue <- pnorm(z, lower.tail=FALSE)


pValue
a
power
print("The value of β (and hence, the power) is calculated at p = pHat")

if (pValue < a) {
  print("We reject the Null Hypothesis!")
} else {
  print("We do not reject the Null Hypothesis")
}


# Necessary sample size for specified power
power <- 0.05
B     <- 1-power
n     <- ( (qnorm(a, lower.tail=FALSE)*sqrt(p0*(1-p0)) + qnorm(B, lower.tail=FALSE)*sqrt(pp*(1-pp)))/
           (pp-p0) )^2

n
