#!/bin/R -f
# vim: textwidth=80 autoindent expandtab

# First we need to read in our dataset
rawdata <- read.table(
  file="data.tsv",
  header=TRUE,

  # Field Separator
  sep='\t',

  # Column value types
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

# Split the imported data frame into a subframes:
#   - Location
#   - Good Friday weather
#   - Easter weather
Loc <- rawdata[,1:2]
Fri <- rawdata[,3:15]
Eas <- rawdata[,16:28]

# Significance Level for all tests
a  <- 0.05


###############################################################################
#                                Hypothesis Tests                             #

# A variable to keep a running tally of the number of "successful" hypothesis
# tests; that is, tests which were able to reject the null hypothesis.
rejects <- 0

#########
# 1. Proportion claim, tested with p-score analysis
#
# Claim: we are attempting to demonstrate that the weather patterns tend to be
#   rainy on Good Friday, and sunny on Easter Sunday, more so than would be
#   expected based on random weather patterns.
#
# Notes:
#   - Since we have a substantical number of samples (50), we can just use the
#     regular proportion test.

# Compress each row of each weather sub-frame into a single {1,0} number
# indicating the overall weather for the day:
#
#   1 if the day was "rainy" (True)
#   0 if the day was "sunny" (False)
#
# We'll need the following function to compute the arithmatic mean of a
# vector, while ignoring NA (missing) values
mean.na <- function(v) mean(v[!is.na(v)])
#
# `s' is the threshold for declaring a day "rainy"; a day will be declared
# rainy if and only if it has has a greater proportion of rainy to sunny
# hours than `s'.
s <- 1/2

bFri <- as.numeric(apply(Fri, 1, mean.na) > s) # numeric() casts the logical
bEas <- as.numeric(apply(Eas, 1, mean.na) > s) # values into {0,1}

# Compress the two values of each corresponding row in the weather subframes
# into a single value {1,0}, indicating whether the weather goes from rain to
# sun between Good Friday and Easter:
#
#   1 if it is raining on Good Friday, and Sunny on Easter (True)
#   0 otherwise                                            (False)
bP   <- as.numeric(bFri & !bEas)

# For pretty-printing, join this column with the `Cities' column of the
# Location sub-frame
data <- data.frame(City=Loc$City, Result=bP)
print(data)


# Define the variables from the final transformed data set
n    <- length(bP)
s    <- sd(bP)

pHat <- mean(bP)
p0   <- 0.25

# Compute the test statistic. This is an upper-tailed test.
z      <- (pHat - p0)/sqrt(p0*(1-p0)/n)
pValue <- pnorm(z, lower.tail=FALSE)

if (pValue < a) {
  print("Proportion claim: we reject the Null Hypothesis!")
  rejects <- rejects + 1
} else {
  print("Proportion claim: we do not reject the Null Hypothesis.")
}



#########
# 2. Mean claim, tested with critical value analysis
#
# Claim: we collected data for 13 hours of the day, from 6am to 6pm, inclusive.
#   We wish to show that, statistically, it rains for more than half of those
#   thirteen hours each on Good Friday and Easter Sunday respectively.
#
# Notes:
#   - We are actually running two hypothesis test here; one considering the
#     hourly weather samples for Easter Sunday, and one considering the samples
#     for Good Friday.
#   - We don't know the distribution of weather, but since we have 50 samples
#     in each case, we can exploit the Central Limit Theorem and use a
#     large-sample test statistic.


# Again, a quick function for summation while ignoring missing values
sum.na <- function(v) sum(v[!is.na(v)])

# Variables for both hypothesis tests.
u0 <- 6.5

# Good Friday
sFri <- apply(Fri, 1, sum.na)

x  <- mean(sFri)
s  <- sd(sFri)
n  <- length(sFri)

zcrit <- qnorm(a/2, lower.tail=FALSE)
z     <- (x - u0)/(s/sqrt(n))

if (z > zcrit) {
  print("Mean claim (for Good Friday): we reject the Null Hypothesis in the upper-tail!")
  rejects <- rejects + 1
} else if (z < -zcrit) {
  print("Mean claim (for Good Friday): we reject the Null Hypothesis in the lower tail!")
  rejects <- rejects + 1
} else {
  print("Mean claim (for Good Friday): we do not reject the Null Hypothesis.")
}

# Easter Sunday
sEas <- apply(Eas, 1, sum.na)

x  <- mean(sEas)
s  <- sd(sEas)
n  <- length(sEas)

zcrit <- qnorm(a/2, lower.tail=FALSE)
z     <- (x - u0)/(s/sqrt(n))

if (z > zcrit) {
  print("Mean claim (for Easter Sunday): we reject the Null Hypothesis in the upper-tail!")
  rejects <- rejects + 1
} else if (z < -zcrit) {
  print("Mean claim (for Easter Sunday): we reject the Null Hypothesis in the lower tail!")
  rejects <- rejects + 1
} else {
  print("Mean claim (for Easter Sunday): we do not reject the Null Hypothesis.")
}



#########
# 3. Standard Deviation claim, tested with confidence interval analysis
#
# Claim: in addition to considering the average rainy hours in a day, there is
#   the question of standard deviation. We wish to demonstrate that 
#   standard deviation of rainy hours is not 2, that the weather on
#   Good Friday and Easter Sunday respectively varies either greater or less
#   than approximately ±2 hours about the average.
#
# Notes:
#   - Again, we are actually running two hypothesis test here; one considering the
#     hourly weather samples for Easter Sunday, and one considering the samples
#     for Good Friday.
#   - We don't know the distribution of weather, but since we have 50 samples
#     in each case, we can exploit the Central Limit Theorem and use a
#     large-sample test statistic.


# Variables for both hypothesis tests.
o0 <- 2
v0 <- o0^2

# Good Friday
x  <- mean(sFri)
s  <- sd(sFri)
n  <- length(sFri)

upper <- (n-1)*s^2/qchisq(a/2, n-1, lower.tail=TRUE)
lower <- (n-1)*s^2/qchisq(a/2, n-1, lower.tail=FALSE)
z     <- (n-1)*s^2/v0

oupper <- sqrt(upper)
olower <- sqrt(lower)
oz     <- sqrt(z)

if (oz > oupper) {
  print("Standard Deviation claim (for Good Friday): we reject the Null Hypothesis in the upper-tail!")
  rejects <- rejects + 1
} else if (oz < olower) {
  print("Standard Deviation claim (for Good Friday): we reject the Null Hypothesis in the lower tail!")
  rejects <- rejects + 1
} else {
  print("Standard Deviation claim (for Good Friday): we do not reject the Null Hypothesis.")
}

# Easter Sunday
x  <- mean(sEas)
s  <- sd(sEas)
n  <- length(sEas)

upper <- (n-1)*s^2/qchisq(a/2, n-1, lower.tail=TRUE)
lower <- (n-1)*s^2/qchisq(a/2, n-1, lower.tail=FALSE)
z     <- (n-1)*s^2/v0

oupper <- sqrt(upper)
olower <- sqrt(lower)
oz     <- sqrt(z)

if (oz > oupper) {
  print("Standard Deviation claim (for Easter Sunday): we reject the Null Hypothesis in the upper-tail!")
  rejects <- rejects + 1
} else if (oz < olower) {
  print("Standard Deviation claim (for Easter Sunday): we reject the Null Hypothesis in the lower tail!")
  rejects <- rejects + 1
} else {
  print("Standard Deviation claim (for Easter Sunday): we do not reject the Null Hypothesis.")
}



###############################################################################
#                             Further Comments                                #

# Multiple Hypothesis Error Probability
cat(
  rejects,
  "tests rejected the null hypothesis. The probability that at least one type I error occurred is:",
  1-(1-a)^rejects,
  "\n"
)
