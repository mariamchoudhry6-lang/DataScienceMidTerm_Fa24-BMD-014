
# MIDTERM ANALYSIS - AIR QUALITY (ISLAMABAD)

# Data Import 

# Load necessary packages
library(tidyverse)

# Read the dataset
data <- read.csv(file.choose()) 
head(data)

# View structure and summary
str(data)
summary(data)

# Rows and columns
cat("Rows:", nrow(data), "  Columns:", ncol(data), "\n")

# Data types
sapply(data, class)

# Missing values per column
colSums(is.na(data))


# Data Cleaning 

# Rename columns for clarity
data <- data %>%
  rename(
    location = location_name,
    pollutant = parameter,
    value = value,
    datetime_utc = datetimeUtc,
    datetime_local = datetimeLocal
  )

# Convert datetime to proper format
data$datetime_local <- as.POSIXct(data$datetime_local, format="%Y-%m-%dT%H:%M:%S%z")

# Convert numeric columns properly
data$value <- as.numeric(data$value)
data$latitude <- as.numeric(data$latitude)
data$longitude <- as.numeric(data$longitude)

# Remove duplicates (if any)
data <- distinct(data)

# Handle missing values (remove rows with NA in important columns)
data <- data %>% drop_na(value)

# ---- Handle Outliers (IQR method) ----
Q1 <- quantile(data$value, 0.25)
Q3 <- quantile(data$value, 0.75)
IQR <- Q3 - Q1
lower <- Q1 - 1.5 * IQR
upper <- Q3 + 1.5 * IQR

# Replace extreme outliers with boundary values
data$value[data$value < lower] <- lower
data$value[data$value > upper] <- upper

# Exploratory Data Analysis 

# a) Descriptive Statistics
mean_val <- mean(data$value)
median_val <- median(data$value)
var_val <- var(data$value)
sd_val <- sd(data$value)

cat("\n--- Descriptive Statistics ---\n")
cat("Mean:", mean_val, "\n")
cat("Median:", median_val, "\n")
cat("Variance:", var_val, "\n")
cat("SD:", sd_val, "\n")

# b) Visualizations
library(ggplot2)

# Histogram
ggplot(data, aes(x = value)) +
  geom_histogram(fill = "steelblue", bins = 10) +
  labs(title = "PM2.5 Distribution (Islamabad)", x = "PM2.5 (µg/m³)", y = "Count")

# Boxplot
ggplot(data, aes(y = value)) +
  geom_boxplot(fill = "tomato") +
  labs(title = "PM2.5 Boxplot (Islamabad)", y = "PM2.5 (µg/m³)")

# Time-series plot about PM2.5 levels over time
ggplot(data, aes(x = datetime_local, y = value)) +
  geom_line(color = "darkgreen") +
  labs(title = "PM2.5 Levels Over Time (Islamabad)", x = "Date", y = "PM2.5 (µg/m³)") +
  theme_minimal()

# c) Correlation heatmap using ggcorrplot or similar package. 
library(ggcorrplot) 
# Select numeric columns only (ensure truly numeric)
numeric_data <- data %>% select(where(is.numeric))

# Check if we have at least 2 numeric columns
if (ncol(numeric_data) >= 2) {
  
  # Compute correlation matrix using complete observations
  cor_matrix <- cor(numeric_data, use = "complete.obs")
  
  # Plot correlation heatmap
  ggcorrplot(cor_matrix, lab = TRUE, title = "Correlation Heatmap", colors = c("red", "white", "blue"))
  
} else {
  warning("Not enough numeric columns for correlation heatmap!")
}

#🌍 Insight 1: PM2.5 Levels Frequently Exceed Safe Limits

#The hourly PM2.5 concentration in Islamabad ranged from 59 to 159 µg/m³, which is well above the WHO safe limit of 25 µg/m³ for 24-hour exposure.
#➡️ This indicates that the city is experiencing unhealthy air quality throughout the recorded period.

#⏰ Insight 2: Evening Hours Show the Highest Pollution Levels

#From the time-series plot, PM2.5 values gradually increase from morning to evening, peaking between 4:00 PM and 6:00 PM.
#➡️ This may be due to traffic congestion, industrial activity, and reduced wind dispersion during those hours.

#🌡️ Insight 3: Short-Term Variability is Significant

#The boxplot and descriptive statistics show large variation (high standard deviation) in hourly PM2.5 readings.
#➡️ This suggests fluctuating local pollution sources or changing weather conditions (like temperature inversions) that trap pollutants.