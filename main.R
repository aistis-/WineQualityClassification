# Let's create an easier new line printing
println <- function(...) {
  cat(..., "\n")
}

# Read red wines data set to a table
redWinesFull <- read.table(file="data_set/red_wines.csv", header=TRUE, quote="\"", sep=";")

# 1-11 attributes are the main values for classification
redWinesValues = redWinesFull[,1:11]

# The 12th attribute is our targeted quality value
redWinesTarget = redWinesFull[,12]

println("Number of attributes: ", ncol(redWinesValues))

# Check if there are any missing valuse
if (0 == sum(is.na(redWinesValues)))
  println("There is no missing values")
else 
  println("Missing values:", sum(is.na(redWinesValues)))

# Print attributes summary
println("Data summary: ")

for(i in 1:ncol(redWinesValues)) {
  println("Attribute:", names(redWinesValues[i]))
  println("  Mode:", mode(redWinesValues[,i]))
  println("  Mean:", mean(redWinesValues[,i]))
  println("  Median:", median(redWinesValues[,i]))
  println("  Min:", min(redWinesValues[,i]))
  printlnt("  Max:", max(redWinesValues[,i]))
  println("  Max - min:", max(redWinesValues[,i]) - min(redWinesValues[,i]))
  println();
}