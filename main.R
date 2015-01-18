# Let's create an easier new line printing
println <- function(...) {
  cat(..., "\n")
}

# Read red wines data set to a table
redWines = read.csv(file="data_set/red_wines.csv", sep=";")
whiteWines = read.csv(file="data_set/red_wines.csv", sep=";")

# Data set for the script. Red or white wines
data = redWines
#data = whiteWines

println("Number of attributes: ", ncol(data))

# Check if there are any missing valuse
if (0 == sum(is.na(data)))
  println("There is no missing values")
else 
  println("Missing values:", sum(is.na(data)))

# Print attributes summary
println("Data summary:")

for(i in 1:ncol(dataValues)) {
  println("Attribute:", names(data[i]))
  println("  Mode:", mode(data[,i]))
  println("  Mean:", mean(data[,i]))
  println("  Median:", median(data[,i]))
  println("  Min:", min(data[,i]))
  println("  Max:", max(data[,i]))
  println("  Max - min:", max(data[,i]) - min(data[,i]))
  println("  Standard deviation:", sd(data[,i]))
  println();
}


# Let's check if there any dupliocated entries
anyDuplicated(data)

# Count how many duplicants exists
duplicats = duplicated(data, nmax=ncol(data))
table(duplicats)

# Leave only unique entries, included one original from the duplicated
data = unique(data)

# Is there still any duplicants?
anyDuplicated(data)

# 1-11 attributes are the main values for classification
dataValues = data[,1:11]

# The 12th attribute is our targeted quality value
dataTarget = data[,12]

# Margins to make the labes visible
par(mar=c(8,3,1,1))
# Box-and-Whisker plot
boxplot(dataValues, las=2)

# Let's investigate data correlation
plot(dataValues)
round(cor(dataValues), digits=3)

# Covariance
round(cov(dataValues), digits=3)

# Standard Deviation Matrix
std = vector(length = ncol(dataValues))

for (i in 1:ncol(dataValues))
  std[i] = sd(dataValues[,i])

# There there are only weak correlation between 
# fixed.acidity and citric.acid 0.672
# fixed.acidity and density 0.668
# fixed.acidity and pH -0.683

# install.packages('e1071', dependencies = TRUE)
library(e1071)
svm <- svm(dataValues)

res = predict(svm, dataValues[,1:11])
cm_iris = table(res, dataValues[,12])

correct = 0
for (i in 1:ncol(data))
  correct = correct + cm_iris[i, i]

accuracy = correct / sum(dataTarget)
