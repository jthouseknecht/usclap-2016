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
s <- 0.5

bFri <- as.numeric(apply(Fri, 1, mean) > s) # numeric() casts the logical value into {0,1}
bEas <- as.numeric(apply(Eas, 1, mean) > s)

# Now, transform the values into our test statititc:

#    /  1 if it is raining on Friday, and Sunny on Easter
#  p {
#    \  0 otherwise
bP   <- as.numeric(bFri & !bEas)

# Now, join those values into the combined data frame
data <- data.frame(Loc$City, bP)
