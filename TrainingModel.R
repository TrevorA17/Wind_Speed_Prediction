# Load the dataset
WeatherData <- read.csv("data/wind_dataset.csv", colClasses = c(
  IND = "factor",
  RAIN = "numeric",
  IND_1 = "factor",
  T_MAX = "numeric",
  IND_2 = "factor",
  T_MIN = "numeric",
  WIND = "numeric"
))

# Display structure to verify data types
str(WeatherData)

# Display first few rows to ensure data is loaded correctly
head(WeatherData)

# Load necessary library
library(caret)

# Set seed for reproducibility
set.seed(123)

# Split the data: 70% training and 30% testing
trainIndex <- createDataPartition(WeatherData$WIND, p = 0.7, list = FALSE)
trainData <- WeatherData[trainIndex, ]
testData <- WeatherData[-trainIndex, ]

cat("Training Set Size:", nrow(trainData), "\n")
cat("Testing Set Size:", nrow(testData), "\n")

# Set seed for reproducibility
set.seed(123)

# Perform bootstrapping: 100 bootstrap samples
bootstrap_samples <- lapply(1:100, function(i) {
  WeatherData[sample(1:nrow(WeatherData), replace = TRUE), ]
})

cat("Bootstrap Samples Generated:", length(bootstrap_samples), "\n")
# Example: Display the first bootstrap sample
head(bootstrap_samples[[1]])
