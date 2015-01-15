# Let's create an easier new line printing
println <- function(...) {
  cat(..., "\n")
}

# Read red wines data set to a table
redWines <- read.csv(file="data_set/red_wines.csv", sep=";")
whiteWines <- read.csv(file="data_set/red_wines.csv", sep=";")

# Data set for the script. Red or white wines
data = redWines
#data = whiteWines

# 1-11 attributes are the main values for classification
dataValues = data[,1:11]

# The 12th attribute is our targeted quality value
dataTarget = data[,12]

println("Number of attributes: ", ncol(dataValues))

# Check if there are any missing valuse
if (0 == sum(is.na(dataValues)))
  println("There is no missing values")
else 
  println("Missing values:", sum(is.na(dataValues)))

# Print attributes summary
println("Data summary: ")

for(i in 1:ncol(dataValues)) {
  println("Attribute:", names(dataValues[i]))
  println("  Mode:", mode(dataValues[,i]))
  println("  Mean:", mean(dataValues[,i]))
  println("  Median:", median(dataValues[,i]))
  println("  Min:", min(dataValues[,i]))
  println("  Max:", max(dataValues[,i]))
  println("  Max - min:", max(dataValues[,i]) - min(dataValues[,i]))
  println("  Standard deviation:", sd(dataValues[,i]))
  println();
}

boxplot(dataValues[,1:11])
plot(dataValues[,1:11])