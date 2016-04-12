#!/bin/R -f
# vim: textwidth=80 autoindent

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

###############################################################################
#                                Hypothesis Test                              #

n    <- length(bP)
s    <- sd(bP)
pHat <- mean(bP)

p0   <- 0.25
a    <- 0.05

# pp stands for p' (p-prime)
# The value of β (and hence, the power) is calculated at p = pHat
pp   <- pHat
B    <- pnorm(
          (p0 - pp + qnorm(a, lower.tail=FALSE)*
	   sqrt(p0*(1-p0)/n))/sqrt(pp*(1-pp)/n),
          lower.tail=TRUE
        )
power <- 1-B

# Compute the test statistic. This is an upper-tailed test.
z      <- (pHat - p0)/sqrt(p0*(1-p0)/n)
pValue <- pnorm(z, lower.tail=FALSE)

if (pValue < a) {
  print("We reject the Null Hypothesis!")
} else {
  print("We do not reject the Null Hypothesis.")
}


# Necessary sample size for specified power
power <- 0.05
B     <- 1-power
n     <- ( (qnorm(a, lower.tail=FALSE)*sqrt(p0*(1-p0)) +
            qnorm(B, lower.tail=FALSE)*sqrt(pp*(1-pp))) / (pp-p0)
	 )^2

print(n)
