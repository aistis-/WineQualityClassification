# Let's create an easier new line printing
println <- function(...) {
  cat(..., "\n")
}

# Read red wines data set to a table
data = read.csv(file="data_set/white_wines.csv", sep=";")
#data = read.csv(file="data_set/red_wines.csv", sep=";")

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

# Margins to make the labes visible
par(mar=c(8,3,1,1))
# Box-and-Whisker plot
boxplot(data[,1:11], las=2)

# Let's investigate data correlation
plot(data[,1:11])
round(cor(data[,1:11]), digits=3)

# Covariance
round(cov(data[,1:11]), digits=3)

# install.packages('e1071', dependencies = TRUE)
library(e1071)

trainPercentage = 50

println(trainPercentage, "percent of total", nrow(data), "rows will be used for train data")

dataTrain = sample(1:nrow(data), round(nrow(data) * 30 / 100))
dataTest = setdiff(1:nrow(data), dataTrain)

SVM = svm(data[dataTrain, 1:11], data[dataTrain, 12])

result = predict(SVM, data[dataTest, 1:11])
result = round(result)

processedData = table(factor(result, levels = 1:10), factor(data[dataTest, 12], levels = 1:10))

correct = 0
for (i in 1:ncol(processedData))
  correct = correct + processedData[i, i]

accuracy = correct / sum(processedData)

println("Calculated accuracy with SVM is", accuracy)

bayes = naiveBayes(data[dataTrain, 1:11], data[dataTrain, 12])

result = predict(bayes, data[dataTest, 1:11])

# compute accuracy
processedData = table(factor(result, levels = 1:10), factor(data[dataTest, 12], levels = 1:10))


correct = 0
for (i in 1:ncol(processedData))
  correct = correct + processedData[i, i]

accuracy = correct / sum(processedData)

println("Calculated accuracy with Bayes is", accuracy)
