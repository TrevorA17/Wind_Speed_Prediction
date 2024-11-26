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

# Set seed for reproducibility
set.seed(123)

# Perform 10-fold cross-validation
cvIndex <- createFolds(WeatherData$WIND, k = 10, list = TRUE)

cat("Number of Folds:", length(cvIndex), "\n")
# Example: Display indices of the first fold
print(cvIndex[[1]])

# Load necessary libraries
library(caret)
library(randomForest)
library(gbm)

# Load the cleaned dataset
CleanedWeatherData <- read.csv("data/cleaned_weather_data.csv")

# Set seed for reproducibility
set.seed(123)

# Split the cleaned dataset into training and testing sets
trainIndex <- createDataPartition(CleanedWeatherData$WIND, p = 0.7, list = FALSE)
trainData <- CleanedWeatherData[trainIndex, ]
testData <- CleanedWeatherData[-trainIndex, ]

# 1. Linear Regression Model
lm_model <- train(
  WIND ~ .,                   # Predict WIND using all other variables
  data = trainData,
  method = "lm",              # Linear Regression
  trControl = trainControl(method = "cv", number = 10) # 10-fold cross-validation
)

# 2. Random Forest Regression Model
rf_model <- train(
  WIND ~ .,                   # Predict WIND using all other variables
  data = trainData,
  method = "rf",              # Random Forest Regression
  trControl = trainControl(method = "cv", number = 10) # 10-fold cross-validation
)

# 3. Gradient Boosting Machine (GBM) Model
gbm_model <- train(
  WIND ~ .,                   # Predict WIND using all other variables
  data = trainData,
  method = "gbm",             # Gradient Boosting Machine
  trControl = trainControl(method = "cv", number = 10), # 10-fold cross-validation
  verbose = FALSE             # Suppress verbose output from the GBM model
)

# Display model summaries
cat("Linear Regression Model:\n")
print(lm_model)

cat("\nRandom Forest Model:\n")
print(rf_model)

cat("\nGradient Boosting Machine Model:\n")
print(gbm_model)

# 4. Make predictions on the test set for all models
lm_predictions <- predict(lm_model, newdata = testData)
rf_predictions <- predict(rf_model, newdata = testData)
gbm_predictions <- predict(gbm_model, newdata = testData)

# 5. Evaluate model performance
actuals <- testData$WIND

# RMSE (Root Mean Squared Error)
lm_rmse <- sqrt(mean((lm_predictions - actuals)^2))
rf_rmse <- sqrt(mean((rf_predictions - actuals)^2))
gbm_rmse <- sqrt(mean((gbm_predictions - actuals)^2))

# R-squared (R²)
lm_r2 <- cor(lm_predictions, actuals)^2
rf_r2 <- cor(rf_predictions, actuals)^2
gbm_r2 <- cor(gbm_predictions, actuals)^2

cat("\nModel Performance on Test Set:\n")
cat("Linear Regression RMSE:", lm_rmse, "R²:", lm_r2, "\n")
cat("Random Forest RMSE:", rf_rmse, "R²:", rf_r2, "\n")
cat("GBM RMSE:", gbm_rmse, "R²:", gbm_r2, "\n")

