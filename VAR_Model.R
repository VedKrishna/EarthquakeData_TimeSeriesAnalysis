library("data.table")
library("stats")
library("forecast")
library("dplyr")

# Importing VAR
library("vars")

file_path <- "C:\\Users\\pvedk\\pvedk\\Desktop\\programming\\MinorProject2\\datasets\\combined_eq_california_timeseries.csv"
df_eq <- read.csv(file_path)

df_eq <- df_eq[, c("time", "mag", "sig", "longitude", "latitude", "depth")]

df_eq$time <- as.POSIXct(df_eq$time)
df_eq$timestamps <- df_eq$time

df_eq

str(df_eq)

duplicated_rows <- df_eq[duplicated(df_eq$timestamps), ]

# Dropping duplicate rows
df_eq <- df_eq[!duplicated(df_eq$timestamps), ]

# Calculating difference between consecutive timestamps
timestamps_diff <- diff(df_eq$timestamps)
summary(timestamps_diff)
str(df_eq)

# Number 1: Time intervals between consecutive earthquakes
df_eq$time_diff <- c(NA, diff(df_eq$timestamps))
df_eq$time_diff_float <- as.numeric(df_eq$time_diff, units = "secs")

# Load the zoo package for rollapplyr function
library(zoo)

# Number 2: Rolling mean of magnitudes from the last 10 earthquakes
df_eq$mag_roll_10 <- rollapplyr(df_eq$mag, width = 10, FUN = mean, fill = NA, align = "right")

# Drop rows with NA values
df_eq <- na.omit(df_eq)
summary(df_eq)


# Filter rows based on the condition time_diff_float > 86400 (which means time difference greater than 86400 seconds)
filtered_rows <- df_eq[df_eq$time_diff_float > 86400, ]

# Count the number of rows in the filtered dataframe
num_filtered_rows <- nrow(filtered_rows)

# Print the number of rows
print(num_filtered_rows)

# Filter rows based on the condition time_diff_float > 86400 (which means time difference greater than 86400 seconds)
filtered_rows <- subset(df_eq, time_diff_float > 86400)

# View the filtered dataframe
print(filtered_rows)

# Find the minimum date from the "time" column
min_date <- min(df_eq$time)

# Extract the date part
min_date <- as.Date(min_date)

# Print the minimum date
print(min_date)

# Find the maximum date from the "time" column
max_date <- max(df_eq$time)

# Extract the date part
max_date <- as.Date(max_date)

# Print the maximum date
print(max_date)

# Find the maximum date from the "time" column
max_date <- max(df_eq$time)

# Extract the date part
max_date <- as.Date(max_date)

# Check the data type of max_date
print(typeof(max_date))


# Define the start date
start_date <- min(df_eq$time)

# Calculate the number of days between min_date and max_date
number_of_days <- as.integer(difftime(max(df_eq$time), min(df_eq$time), units = "days"))
print(paste("number_of_days:", number_of_days))

# Create a sequence of dates
date_list <- seq(start_date, by = "days", length.out = number_of_days)

# Print the second date in date_list and its data type
print(date_list[2])
print(class(date_list[2]))
# Define the start date
start_date <- min(df_eq$time)

# Calculate the number of days between min_date and max_date
number_of_days <- as.integer(difftime(max(df_eq$time), min(df_eq$time), units = "days"))
print(paste("number_of_days:", number_of_days))

# Create a sequence of dates
date_list <- seq(start_date, by = "days", length.out = number_of_days)

# Print the second date in date_list and its data type
print(date_list[2])
print(class(date_list[2]))
df_eq

library(data.table)

# Convert data.frame to data.table
setDT(df_eq)

# Set the 'time' column as the key
setkey(df_eq, time)


rownames(df_eq) <- df_eq$time

# Assuming df_eq is already a data.table
setkey(df_eq, time)

library(data.table)
library(zoo)

# Assuming df_eq is already a data.table with a time column in POSIXct format
# Convert df_eq to data.table if it's not already
setDT(df_eq)

# Aggregate data by day
df_eq[, date := as.IDate(time)]
df_daily <- df_eq[, .(
  mag_max = max(mag, na.rm = TRUE),
  event_count = .N,
  mag_mean = mean(mag, na.rm = TRUE),
  mag_sum = sum(mag, na.rm = TRUE),
  mag_scatter = sd(mag, na.rm = TRUE),
  longitude_mean = mean(longitude, na.rm = TRUE),
  longitude_std = sd(longitude, na.rm = TRUE),
  latitude_mean = mean(latitude, na.rm = TRUE),
  latitude_std = sd(latitude, na.rm = TRUE),
  depth_mean = mean(depth, na.rm = TRUE),
  depth_std = sd(depth, na.rm = TRUE),
  time_diff_float_mean = mean(time_diff_float, na.rm = TRUE),
  time_diff_float_std = sd(time_diff_float, na.rm = TRUE)
), by = date]

# Calculate the rolling mean for the mag_mean column with a window of 10 days
df_daily[, mag_roll_10 := rollapply(mag_mean, width = 10, FUN = mean, fill = NA, align = "right")]

df_daily


# Load necessary libraries
library(data.table)
library(ggplot2)
library(naniar)

# Assuming df_daily is your data.table from the previous example
# Ensure df_daily is a data.frame
df_daily <- as.data.frame(df_daily)

# Check for missing values and visualize
ggplot_missing <- vis_miss(df_daily) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(title = "Heatmap of Missing Values")

# Print the plot
print(ggplot_missing)


# Load necessary libraries
library(data.table)

# Assuming df_daily is already a data.table
# Convert df_daily to data.table if necessary
setDT(df_daily)

# Filter the data to include only rows where the date is greater than "1972-01-01"
df_daily_clean <- df_daily[date > as.IDate("1972-01-01")]

# Drop the column "mag_roll_10"
df_daily_clean[, mag_roll_10 := NULL]

# Display the resulting data table
print(df_daily_clean)


# Load necessary libraries
library(data.table)
library(ggplot2)
library(naniar)

# Assuming df_daily_clean is your data.table from the previous example
# Ensure df_daily_clean is a data.frame
df_daily_clean <- as.data.frame(df_daily_clean)

# Check for missing values and visualize
ggplot_missing <- vis_miss(df_daily_clean) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(title = "Heatmap of Missing Values")

# Print the plot
print(ggplot_missing)

# Load necessary library
library(data.table)

# Ensure df_daily_clean is a data.table or data.frame
setDT(df_daily_clean)

# Display structure of the data frame
str(df_daily_clean)

# Display summary of the data frame
summary(df_daily_clean)


library(data.table)
library(zoo)

# Assuming df_daily_clean is your data.table or data.frame
# Convert data.table to data.frame if necessary
df_daily_clean <- as.data.frame(df_daily_clean)

# Convert the date column to POSIXct format if needed
df_daily_clean$date <- as.POSIXct(df_daily_clean$date)

# Sort the data by date if not already sorted
df_daily_clean <- df_daily_clean[order(df_daily_clean$date), ]

# Specify columns for interpolation
columns_to_interpolate <- c("mag_max", "event_count", "mag_mean", "mag_sum", "mag_scatter", 
                            "longitude_mean", "longitude_std", "latitude_mean", 
                            "latitude_std", "depth_mean", "depth_std", 
                            "time_diff_float_mean", "time_diff_float_std")

# Perform linear interpolation on specified columns
for (col in columns_to_interpolate) {
  df_daily_clean[[col]] <- na.approx(df_daily_clean[[col]], rule = 2)  # rule = 2 handles leading/trailing NAs
}

# Convert date column back to Date format if needed
df_daily_clean$date <- as.Date(df_daily_clean$date)

# Convert data.frame back to data.table if needed
setDT(df_daily_clean)

# Check if there are any remaining NAs
if (any(is.na(df_daily_clean))) {
  print("Warning: There are still NA values in df_daily_clean after interpolation.")
}

# Display the interpolated data.table
print(df_daily_clean)

# Optionally, remove any remaining rows with NAs if interpolation could not handle all cases
df_daily_clean <- df_daily_clean[complete.cases(df_daily_clean), ]

# Display the final cleaned data.table
print(df_daily_clean)

# Assuming df_daily_clean is your data.table or data.frame
# Convert data.table to data.frame if necessary
df_daily_clean <- as.data.frame(df_daily_clean)

# Display the structure and summary information
str(df_daily_clean)

# Assuming df_daily_clean is your data.table or data.frame
# Assign df_daily_clean to df_eq
df_eq <- df_daily_clean

# Get the dimensions of df_eq
df_eq_shape <- dim(df_eq)

# Print the dimensions
print(df_eq_shape)

str(df_eq)


# Assuming df_eq is your data.table or data.frame

# Initialize an empty vector for labels
label <- vector("integer", length = nrow(df_eq))
cnt <- 0

# Loop through each row and assign labels based on magnitude
for (i in 1:nrow(df_eq)) {
  if (df_eq$mag_max[i] > 5.5) {
    cnt <- cnt + 1
    label[i] <- as.integer(cnt)
  } else {
    label[i] <- 0
  }
}

# Add the "large_eq_label" column to df_eq
df_eq$large_eq_label <- label

# Print the updated data.table or data.frame
print(df_eq)

# Assuming df_eq is your data.table or data.frame
# Convert data.table to data.frame if necessary
df_eq <- as.data.frame(df_eq)

# Get the summary statistics for each column
summary_stats <- summary(df_eq)

# Transpose the summary for better readability
summary_stats_transposed <- t(summary_stats)

# Print the transposed summary
print(summary_stats_transposed)

str(df_eq)


# Load necessary libraries
library(ggplot2)
library(gridExtra)

# Assuming df_eq is your data.table or data.frame
# Convert data.table to data.frame if necessary
df_eq <- as.data.frame(df_eq)

# Create plots for each column
plots <- lapply(names(df_eq), function(col) {
  ggplot(df_eq, aes_string(x = col)) +
    geom_histogram() +  # Example: Use geom_histogram() for histograms
    labs(title = col)  # Set title to column name
})

# Arrange plots in a grid layout
grid.arrange(grobs = plots, ncol = 3)  # Example: Arrange in 3 columns

str(df_eq)


library(corrplot)
# Exclude the "date" column from df_eq
df_numeric <- df_eq[, !names(df_eq) %in% "date"]

# Calculate the correlation matrix
df_corr <- cor(df_numeric)

# Plot the correlation heatmap using corrplot
corrplot(df_corr, method = "color", type = "upper", col = colorRampPalette(c("blue", "white", "red"))(100),
         tl.col = "black", tl.srt = 45, diag = FALSE)

df_eq <- df_daily_clean

# Display the first 2 rows of df_eq
print(head(df_eq, 2))


interpret_dftest <- function(dftest) {
  # Create a named list with the relevant elements
  dfoutput <- list(
    "Test Statistic" = dftest$statistic,
    "p-value" = dftest$p.value,
    "Lag Used" = dftest$parameter
  )
  
  return(dfoutput)
}
# Load necessary library
library(tseries)

# Assuming df_eq is a data frame or data table with a 'mag_max' column
# Perform Dickey-Fuller test
dftest <- adf.test(df_eq$mag_max)

# Use the interpret_dftest function to interpret the test results
result <- interpret_dftest(dftest)
print(result)


# Assuming df_eq is your data frame

# Calculate the number of rows for the test set based on test_size
test_size <- 0.25
num_test_rows <- round(nrow(df_eq) * test_size)

# Generate random indices for the test set without replacement
test_indices <- sample(seq_len(nrow(df_eq)), size = num_test_rows, replace = FALSE)

# Create the test set using the sampled indices
test <- df_eq[test_indices, ]

# Create the train set by excluding the rows in the test set
train <- df_eq[-test_indices, ]

# Load the vars package
library(vars)

# Assuming train is your data frame with multiple time series variables

# Convert train to a time series object
train_ts <- ts(train, frequency = 1)

# Instantiate a VAR model
model <- VAR(train_ts)

# Fit a VAR model with specified parameters
ts_model <- VAR(train_ts, p = 60, type = "const", ic = "aic")


# Assuming ts_model is your fitted VAR model object
ts_model.k_ar <- ts_model$p

print(ts_model.k_ar)

# Generate forecasts for 3 time steps
forecast_steps <- 3
forecast <- predict(ts_model, n.ahead = forecast_steps)

# Assuming the necessary packages are already installed and loaded
library(forecast)

# Assuming ts_model is a fitted VAR model and we have forecasted values
# Generate the forecast
forecast_horizon <- nrow(test)
forecast_result <- predict(ts_model, n.ahead = forecast_horizon)

# Extract the forecasted values for all variables
forecast_values <- forecast_result$fcst

# Create a data frame for the forecasted values
forecast_df <- data.frame(lapply(forecast_values, function(x) x[, "fcst"]))

# Ensure the column names of forecast_df match those of test
names(forecast_df) <- names(test)

# Identify numeric columns in test data
numeric_columns <- sapply(test, is.numeric)

# Filter numeric columns from test and forecast_df
test_numeric <- test[, numeric_columns]
forecast_numeric <- forecast_df[, numeric_columns]

# Ensure column names match between test_numeric and forecast_numeric
names(forecast_numeric) <- names(test_numeric)

# Identify numeric columns in the test data
numeric_columns <- sapply(test, is.numeric)

# Filter numeric columns from test and forecast_df
test_numeric <- test[, numeric_columns]
forecast_numeric <- forecast_df[, numeric_columns]

# Ensure column names match between test_numeric and forecast_numeric
names(forecast_numeric) <- names(test_numeric)

# Calculate and print MSE for each numeric column in the test data
for (i in seq_along(test_numeric)) {
  mse_value <- mean((test_numeric[[i]] - forecast_numeric[[i]])^2, na.rm = TRUE)
  cat(sprintf("The test MSE on the %s data is: %.4f\n", names(test_numeric)[i], mse_value))
}


# Load the necessary libraries
library(ggplot2)

# Create a data frame with the test and forecasted values
plot_data <- data.frame(
  Index = 1:length(test_numeric[, 1]),  # Assuming the index represents time or observations
  True = test_numeric[, 1],  # Assuming the first column is the "True" values
  Predicted = forecast_numeric[, 1]  # Assuming the first column in forecast_df corresponds to "Predicted" values
)

# Plot the data using ggplot2
ggplot(plot_data, aes(x = Index)) +
  geom_line(aes(y = True, color = "True"), size = 1) +
  geom_point(aes(y = True, color = "True"), size = 1, shape = 19) +
  geom_line(aes(y = Predicted, color = "Predicted"), size = 1, linetype = "dashed") +
  geom_point(aes(y = Predicted, color = "Predicted"), size = 1, shape = 19) +
  scale_color_manual(values = c(True = "blue", Predicted = "red")) +
  labs(
    x = "Index",
    y = "Moment magnitude",
    title = "Maximum daily amplitude time-series",
    color = "Legend"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.title = element_blank(),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14, face = "bold"),
    plot.title = element_text(size = 18, face = "bold")
  )


# Load necessary libraries
library(ggplot2)

# Calculate time_days column in df_eq_plot (assuming df_eq is already loaded)
df_eq_plot <- df_eq
df_eq_plot$time_days <- df_eq_plot$time_diff_float_mean / (24 * 60 * 60)  # Convert seconds to days

# Create the horizontal histogram plot using ggplot2
ggplot(df_eq_plot, aes(y = time_days)) +
  geom_histogram(binwidth = 1, color = 'black', fill = 'red', alpha = 0.7) +
  coord_flip() +  # Flip coordinates to make it horizontal
  labs(
    title = '50 years of California earthquakes',
    y = 'Time interval (days)',
    x = 'Frequency of earthquake occurrence'
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 24, face = "bold"),
    axis.title.y = element_text(size = 22),
    axis.title.x = element_text(size = 22),
    axis.text.y = element_text(size = 18),
    axis.text.x = element_text(size = 18)
  ) +
  scale_x_log10()  # Log scale on the x-axis


# Assuming df_eq_model is your DataFrame and you want to drop the "large_eq_label" column
df_eq_model <- df_eq[, !(names(df_eq) %in% c("large_eq_label"))]

# Calculate sizes for train and test sets
train_size <- round(nrow(df_eq_model) * 0.9)
test_size <- nrow(df_eq_model) - train_size

# Split the DataFrame into train and test sets
train <- df_eq_model[1:train_size, ]
test <- df_eq_model[(train_size + 1):nrow(df_eq_model), ]

# Print the shapes of train and test sets
print(paste("Train shape:", nrow(train), "rows,", ncol(train), "columns"))
print(paste("Test shape:", nrow(test), "rows,", ncol(test), "columns"))


# Load necessary library
library(dplyr)

# Define the feature columns
f_columns <- c('event_count', 'mag_mean', 'mag_sum', 'mag_scatter',
               'longitude_mean', 'longitude_std', 'latitude_mean', 'latitude_std',
               'depth_mean', 'depth_std', 'time_diff_float_mean',
               'time_diff_float_std')

# Define the columns to be scaled
train_scaled <- train
train_scaled[, f_columns] <- scale(train_scaled[, f_columns])

# Define the column to be scaled separately (mag_max)
train_scaled$mag_max <- scale(train_scaled$mag_max)

# Print the scaled DataFrame
print(head(train_scaled))

