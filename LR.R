library(ggplot2)
library(corrplot)
library(Metrics)

#df_eq <- df_eq[-which(names(df_eq) %in% c("place", "status", "tsunami", "net", "nst", "type", "magType", "name"))]
#df_eq <- df_eq[, !names(df_eq) %in% c("index")]

df_eq$time_seconds <- as.numeric(df_eq$time)
failure_event <- df_eq[df_eq$mag == max(df_eq$mag), ]
df_eq$time_to_failure_sec <- as.numeric(failure_event$time_seconds) - df_eq$time_seconds
#df_eq <- df_eq[, !names(df_eq) %in% c("time", "time_seconds")]
head(df_eq, 5)

failure_event <- df_eq[df_eq$mag == max(df_eq$mag), ]
print(failure_event)

ggplot(df_eq_large, aes(x = longitude, y = latitude, size = mag)) +
  geom_point(alpha = 0.4) +
  scale_size_continuous(range = c(1, 10)) +
  labs(title = "Earthquake Distribution", x = "Longitude", y = "Latitude", size = "Magnitude") +
  theme_minimal()

##LINEAR REGRESSION##

X <- df_eq[, c("mag", "depth", "longitude", "latitude")]
y <- df_eq$sig

# Step 2: Perform linear regression
model <- lm(y ~ ., data = X)
# Step 3: Make predictions
y_pred <- predict(model, newdata = X)

plot(y_pred, y)
abline(lm(y ~ y_pred), col = "red")

# Create residuals
resids <- abs(y - y_pred)

max(resids)
print(mean(resids))
