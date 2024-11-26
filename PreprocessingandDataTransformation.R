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

# Check for missing values in the entire dataset
missing_values <- colSums(is.na(WeatherData))

# Display the count of missing values for each column
cat("Missing values in each column:\n")
print(missing_values)

# Check if the dataset has any missing values at all
if (sum(missing_values) > 0) {
  cat("Total missing values in the dataset:", sum(missing_values), "\n")
} else {
  cat("No missing values in the dataset.\n")
}

# Optionally, display rows with missing values
rows_with_na <- WeatherData[!complete.cases(WeatherData), ]
cat("Rows with missing values:\n")
print(rows_with_na)


# View the dataset in a spreadsheet-like interface (optional)
View(WeatherData)

# Remove rows with missing values
CleanedWeatherData <- na.omit(WeatherData)

# Save the cleaned dataset to a CSV file
write.csv(CleanedWeatherData, "data/cleaned_weather_data.csv", row.names = FALSE)

# Verify that there are no missing values in the cleaned dataset
missing_values_cleaned <- colSums(is.na(CleanedWeatherData))

cat("Missing values in the cleaned dataset:\n")
print(missing_values_cleaned)

if (sum(missing_values_cleaned) == 0) {
  cat("All missing values have been successfully removed.\n")
} else {
  cat("There are still missing values in the cleaned dataset.\n")
}
