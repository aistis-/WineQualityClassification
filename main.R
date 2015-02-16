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

for(i in 1:ncol(data)) {
  println("Attribute:", names(data[i]))
  println("  Mode:", mode(data[,i]))
  println("  Mean:", round(mean(data[,i]), digits=3))
  println("  Median:", median(data[,i]))
  println("  Min:", min(data[,i]))
  println("  Max:", max(data[,i]))
  println("  Max - min:", max(data[,i]) - min(data[,i]))
  println("  Standard deviation:", round(sd(data[,i]), digits=3))
  println();
}

# Margins to make the labes visible
par(mar=c(8,3,1,1))
# Box-and-Whisker plot
boxplot(data[,1:11], las=2)

# Let's investigate data correlation
plot(data[,1:11])
round(cor(data[,1:11]), digits=3)

# Let's check if there any duplicated entries
anyDuplicated(data)

# Count how many duplicants exists
duplicats = duplicated(data, nmax=ncol(data))
table(duplicats)

# Leave only unique entries, included one original from the duplicated
data = unique(data)

# Is there still any duplicants?
anyDuplicated(data)

# install.packages('e1071', dependencies = TRUE)
library(e1071)
library(matrixStats)

# dataStandardized = scale(as.matrix(data), TRUE, TRUE)
# dataStandardized = sweep(dataStandardized, 2, colSds(as.matrix(data[1:12])), "*")
# dataStandardized = sweep(dataStandardized, 2, colMeans(data[1:12]), "+")
# data = dataStandardized

confusionMatrix = matrix(0, 10, 10)

# The prediction will be repeated 5 times with randomly diferent training and testing data
for (k in 1:5) {
  from = round(nrow(data) / 5 * (k-1)) + 1
  to = round(nrow(data) / 5 * k)
  
  SVM = svm(data[-(from:to), 1:11], data[-(from:to), 12], type = "C-classification")
  
  result = predict(SVM, data[from:to, 1:11])
  
  confusionMatrix = confusionMatrix + table(factor(result, levels = 1:10), factor(data[from:to, 12], levels = 1:10))
}

correct = 0
for (i in 1:ncol(confusionMatrix))
  correct = correct + confusionMatrix[i, i]

# Strict accuracy
accuracyStrict = correct / sum(confusionMatrix)

# Actual test set quality - predicted quality
mismatch = data[from:to, 12] - strtoi(result)

# Count accuracy taking into account that +- 1 is still acceptable
accuracyNeighbotStrict = length(which(abs(mismatch) <= 1)) / length(mismatch)

# Mean Absolute Error
mae = sum(abs(data[from:to, 12] - strtoi(result)))/length(mismatch)

println("Calculated strict accuracy with SVM is", accuracyStrict)
println("Calculated one-neighbor accuracy with SVM is", accuracyNeighbotStrict)
println("Calculated MAE with SVM is", mae)

confusionMatrix

confusionMatrix = matrix(0, 10, 10)

# The prediction will be repeated 5 times with randomly diferent training and testing data
for (k in 1:5) {
  from = round(nrow(data) / 5 * (k-1)) + 1
  to = round(nrow(data) / 5 * k)
  
  bayes = naiveBayes(data[-(from:to), 1:11], as.factor(data[-(from:to), 12]))
  
  result = predict(bayes, data[from:to, 1:11])
  
  confusionMatrix = confusionMatrix + table(factor(result, levels = 1:10), factor(data[from:to, 12], levels = 1:10))
}

correct = 0
for (i in 1:ncol(confusionMatrix))
  correct = correct + confusionMatrix[i, i]

# Strict accuracy
accuracyStrict = correct / sum(confusionMatrix)

# Actual test set quality - predicted quality
mismatch = data[from:to, 12] - strtoi(result)

# Count accuracy taking into account that +- 1 is still acceptable
accuracyNeighbotStrict = length(which(abs(mismatch) <= 1)) / length(mismatch)

# Mean Absolute Error
mae = sum(abs(data[from:to, 12] - strtoi(result)))/length(mismatch)

println("Calculated strict accuracy with Bayes is", accuracyStrict)
println("Calculated one-neighbor accuracy with Bayes is", accuracyNeighbotStrict)
println("Calculated MAE with Bayes is", mae)

confusionMatrix