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

# View the dataset in a spreadsheet-like interface (optional)
View(WeatherData)

# Load necessary libraries
library(psych)   # For descriptive statistics
library(corrplot) # For visualizing relationships

# Measures of Frequency
# Frequency distribution of categorical variables
freq_IND <- table(WeatherData$IND)
freq_IND_1 <- table(WeatherData$IND_1)
freq_IND_2 <- table(WeatherData$IND_2)

cat("Frequency distribution for IND:\n")
print(freq_IND)

cat("Frequency distribution for IND_1:\n")
print(freq_IND_1)

cat("Frequency distribution for IND_2:\n")
print(freq_IND_2)

# Measures of Central Tendency
# Mean, Median, and Mode (mode is approximated as the most frequent value for simplicity)
mean_RAIN <- mean(WeatherData$RAIN, na.rm = TRUE)
median_RAIN <- median(WeatherData$RAIN, na.rm = TRUE)
mode_RAIN <- as.numeric(names(sort(table(WeatherData$RAIN), decreasing = TRUE))[1])

cat("Mean of RAIN:", mean_RAIN, "\n")
cat("Median of RAIN:", median_RAIN, "\n")
cat("Mode of RAIN:", mode_RAIN, "\n")

# Measures of Distribution
# Standard deviation, variance, range
sd_RAIN <- sd(WeatherData$RAIN, na.rm = TRUE)
var_RAIN <- var(WeatherData$RAIN, na.rm = TRUE)
range_RAIN <- range(WeatherData$RAIN, na.rm = TRUE)

cat("Standard deviation of RAIN:", sd_RAIN, "\n")
cat("Variance of RAIN:", var_RAIN, "\n")
cat("Range of RAIN:", paste(range_RAIN, collapse = " - "), "\n")

# Measures of Relationship
# Correlation matrix for numeric variables
numeric_vars <- WeatherData[, sapply(WeatherData, is.numeric)]
cor_matrix <- cor(numeric_vars, use = "complete.obs")

cat("Correlation matrix:\n")
print(cor_matrix)

# Visualize the correlation matrix
corrplot(cor_matrix, method = "color", type = "upper", tl.col = "black", tl.srt = 45)
