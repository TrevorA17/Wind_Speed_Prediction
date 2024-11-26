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

# Perform ANOVA on WIND by IND
anova_result <- aov(WIND ~ IND, data = WeatherData)

# Display ANOVA summary
summary(anova_result)

# Post-hoc test to identify pairwise differences (if applicable)
# Using Tukey's HSD test for multiple comparisons
tukey_result <- TukeyHSD(anova_result)

# Display Tukey's HSD results
print(tukey_result)

# Visualize the differences
plot(tukey_result)

# Optionally perform ANOVA for another categorical variable (e.g., IND_1)
anova_result_2 <- aov(WIND ~ IND_1, data = WeatherData)
summary(anova_result_2)

# Load necessary libraries
library(ggplot2)

# Univariate Plots

# Histogram for a numeric variable (e.g., WIND)
ggplot(WeatherData, aes(x = WIND)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  labs(title = "Distribution of Wind Speed", x = "Wind Speed", y = "Frequency") +
  theme_minimal()

# Boxplot for a numeric variable (e.g., WIND)
ggplot(WeatherData, aes(y = WIND)) +
  geom_boxplot(fill = "cyan", color = "black") +
  labs(title = "Boxplot of Wind Speed", y = "Wind Speed") +
  theme_minimal()

# Bar chart for a categorical variable (e.g., IND)
ggplot(WeatherData, aes(x = IND)) +
  geom_bar(fill = "orange", color = "black") +
  labs(title = "Frequency of IND Categories", x = "IND", y = "Count") +
  theme_minimal()

# Multivariate Plots

# Scatter plot of WIND vs. T_MAX, colored by IND
ggplot(WeatherData, aes(x = T_MAX, y = WIND, color = IND)) +
  geom_point() +
  labs(title = "Scatter Plot of Wind Speed vs. Max Temperature", 
       x = "Max Temperature", y = "Wind Speed") +
  theme_minimal()

# Boxplot of WIND grouped by IND
ggplot(WeatherData, aes(x = IND, y = WIND)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(title = "Wind Speed by IND Categories", x = "IND", y = "Wind Speed") +
  theme_minimal()

# Correlation heatmap for numeric variables
library(reshape2)
numeric_vars <- WeatherData[, sapply(WeatherData, is.numeric)]
cor_matrix <- cor(numeric_vars, use = "complete.obs")
cor_melted <- melt(cor_matrix)

ggplot(cor_melted, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0) +
  labs(title = "Correlation Heatmap", x = "Variables", y = "Variables") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

