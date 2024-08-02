# Import necessary libraries
library(tidyverse)
library(naniar)
library(lubridate)
library(ggplot2)
library(caret)
library(keras)
library(tensorflow)
library(dplyr)
library(magrittr)
library(zoo)
# For machine learning preprocessing and evaluation
library(scales)
library(Metrics)

df <- df_eq %>%
  select(time, mag, sig, longitude, latitude, depth)

df$timestamps <- df$time

head(df)
summary(df)

df <- df %>% distinct(timestamps, .keep_all = TRUE)
# Calculate the differences between consecutive 'timestamps'
df$timestamps <- as.POSIXct(df$timestamps, format="%Y-%m-%d %H:%M:%S") # Ensure 'timestamps' is in POSIXct format
time_diff <- diff(df$timestamps)
df <- df %>%
  arrange(timestamps) %>%
  mutate(time_diff = difftime(timestamps, lag(timestamps), units = "secs"))
# Convert time_diff to seconds (numeric)
df <- df %>%
  mutate(time_diff_float = as.numeric(time_diff))
# Calculate the rolling mean of magnitudes for the last 10 earthquakes
df <- df %>%
  mutate(mag_roll_10 = zoo::rollmean(mag, 10, fill = NA, align = "right"))
df <- df %>%
  drop_na()

head(df)

#filtered_df <- df %>%
#  filter(time_diff_float > 86400)
#print(filtered_df)

df_daily <- df %>%
  group_by(date = as.Date(timestamps)) %>%
  summarise(
    mag_max = max(mag, na.rm = TRUE),
    event_count = n(),
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
  ) %>%
  ungroup()
# Calculate the rolling mean of magnitudes for the last 10 days
df_daily <- df_daily %>%
  mutate(mag_roll_10 = zoo::rollmean(mag_mean, 10, fill = NA, align = "right"))
# Print the resulting dataframe to check the results
summary(df_daily)

df <- df_daily
dim(df)

#Adding a column to identify big earthquakes
label <- rep(0, nrow(df))
cnt <- 0
for (i in 1:nrow(df)) {
  if (df$mag_max[i] > 5.5) {
    cnt <- cnt + 1
    label[i] <- cnt
  } else {
    label[i] <- 0
  }
}

# Add the labels to the data frame
df$large_eq_label <- label
summary(df$large_eq_label)

df_reshaped <- data.frame(x = df$date,                            
                          y = c(df$mag_max, df$mag_mean, df$mag_scatter, df$longitude_mean, df$latitude_mean, df$depth_mean, df$large_eq_label),
                          group = c(rep("mag_max", nrow(df)),
                                    rep("mag_mean", nrow(df)),
                                    rep("mag_scatter", nrow(df)),
                                    rep("latitude_mean", nrow(df)),
                                    rep("longitude_mean", nrow(df)),
                                    rep("depth_mean", nrow(df)),
                                    rep("large_eq_label", nrow(df))))

ggplot(df_reshaped, aes(x, y, col = group)) +  geom_line()+ facet_grid(group ~ .)

df_model <- df[, !(names(df) %in% c("large_eq_label"))]
# Calculate train and test sizes
train_size <- as.integer(nrow(df_model) * 0.9)
test_size <- nrow(df_model) - train_size
# Split the dataframe into train and test sets
train <- df_model[1:train_size, ]
test <- df_model[(train_size + 1):nrow(df_model), ]
print(paste("Train shape:", nrow(train), "rows,", ncol(train), "columns"))
print(paste("Test shape:", nrow(test), "rows,", ncol(test), "columns"))


f_columns <- c('event_count', 'mag_mean', 'mag_sum', 'mag_scatter',
               'longitude_mean', 'longitude_std', 'latitude_mean', 'latitude_std',
               'depth_mean', 'depth_std', 'time_diff_float_mean',
               'time_diff_float_std')

# Define the transformer functions
f_transformer <- function(x) {
  center <- quantile(x, probs = 0.5, na.rm = TRUE)
  scale <- IQR(x, na.rm = TRUE)
  (x - center) / scale
}
mag_transformer <- function(x) {
  center <- quantile(x, probs = 0.5, na.rm = TRUE)
  scale <- IQR(x, na.rm = TRUE)
  (x - center) / scale
}

# Fit the transformers
train_scaled <- as.data.frame(lapply(train[, f_columns], f_transformer))
mag_scaled <- as.data.frame(lapply(train[, "mag_max"], mag_transformer))

feature_center <- colMeans(train[, f_columns])
feature_scale <- apply(train[, f_columns], 2, sd)
# Parameters for the magnitude transformer
mag_center <- mean(train[["mag_max"]])
mag_scale <- sd(train[["mag_max"]])
# Print the parameters
print("Feature transformer parameters:")
print(feature_center)
print(feature_scale)
print("Magnitude transformer parameters:")
print(mag_center)
print(mag_scale)


train_scaled <- as.data.frame(lapply(train[, f_columns], function(x) (x - feature_center) / feature_scale))
train_scaled$mag_max <- (train[["mag_max"]] - mag_center) / mag_scale
# Apply transformations to the test set
test_scaled <- as.data.frame(lapply(test[, f_columns], function(x) (x - feature_center) / feature_scale))
test_scaled$mag_max <- (test[["mag_max"]] - mag_center) / mag_scale


##PREPARING THE MODEL:

#use_condaenv("keras-tf", required = T)

create_dataset <- function(X, y, time_steps = 1) {
  Xs <- list()
  ys <- list()
  
  for (i in 1:(nrow(X) - time_steps + 1)) {
    v <- X[i:(i + time_steps - 1), ]  # Get the subset of data for this time step
    Xs[i] <- v  # Assign the features to Xs list
    ys[i] <- y[i:(i + time_steps - 1)]  # Assign the label to ys list
  }
  
  Xs <- array(Xs, dim = c(length(Xs), dim(v)))
  ys <- array(ys, dim = c(length(ys), c(time_steps,1)))
  
  return(list(Xs, ys))
}

TIME_STEPS <- 120

train_data <- create_dataset(train[, f_columns], train[["mag_max"]], time_steps = TIME_STEPS)
X_train <- train_data[[1]]
y_train <- train_data[[2]]

test_data <- create_dataset(test[, f_columns], test[["mag_max"]], time_steps = TIME_STEPS)
X_test <- test_data[[1]]
y_test <- test_data[[2]]

model <- keras_model_sequential() %>%
  layer_lstm(units = 128, input_shape = c(TIME_STEPS, ncol(X_train)),
             return_sequences = TRUE) %>%
  layer_leaky_relu(alpha = 0.5) %>%
  layer_lstm(units = 128, return_sequences = TRUE) %>%
  layer_leaky_relu(alpha = 0.5) %>%
  layer_dropout(rate = 0.3) %>%
  layer_lstm(units = 64, return_sequences = FALSE) %>%
  layer_dropout(rate = 0.3) %>%
  layer_dense(units = 1)
