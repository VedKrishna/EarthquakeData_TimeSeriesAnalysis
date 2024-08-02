library(maps)
library(mapdata)
library(ggplot2)
library(dplyr)

cor.test(df_eq$mag, df_eq$sig, method = "pearson")

# Plot original magnitudes and moving average
ggplot(data = df_eq, aes(x = mag)) +
  geom_point(aes(y = sig), color = "red") +
  labs(x = "mag", y = "sig", title = "Significance Vs Magnitude")


#ggplot(df_eq, aes(x = mag, y = sig)) +
#  geom_point(color = "blue", size = 3) +
#  labs(title = "Significance and magnitude", x = "Magnitude", y = "Significance") +
#  theme_minimal()
# Filter rows where mag is greater than 6
df_eq_large <- subset(df_eq, mag > 6)
#85 huge earthquakes have occured in the past 50 years
nrow(df_eq_large)

ggplot(df_eq_large, aes(x = seq_along(mag), y = mag)) +
  geom_point(color = "red") +
  geom_line() +
  labs(title = "Magnitude", y = "Magnitude") +
  theme_minimal()

# Create a new plot
plot(x = df_eq_large$longitude, y = df_eq_large$latitude, xlim = c(-700, 500), ylim = c(-500, 5000), type = "n")

#map("state", regions="california", col="lightblue", fill=TRUE, xlim=c(-130, -110), ylim=c(32, 43))
map("state", col="lightblue", fill=TRUE, xlim=c(-133, -113), ylim=c(32, 50))

# Scatter plot of earthquakes
points(df_eq_large$longitude, df_eq_large$latitude, col = "red", pch = 16, cex = df_eq_large$mag*0.15, alpha = 0.5)

# Add legend
legend("bottomleft", legend = "Magnitude", pch = 19, pt.cex = 2, cex = 0.8, col = "red", title = "Magnitude")



#30 days prior the biggest earthquake
df_copy <- df_combined_clean
df_copy$time <- as.Date(df_copy$time)
df_eq_lp <- df_copy %>%
  filter(time >= as.Date('1989-09-19') & time <= as.Date('1989-10-19'))

head(df_eq_lp)

nrow(df_eq_lp)


plot(x = df_eq_large$longitude, y = df_eq_large$latitude, xlim = c(-700, 500), ylim = c(-500, 5000), type = "n")

#map("state", regions="california", col="lightblue", fill=TRUE, xlim=c(-130, -110), ylim=c(32, 43))
map("state", col="lightblue", fill=TRUE, xlim=c(-133, -113), ylim=c(32, 50))

# Scatter plot of earthquakes
points(df_eq_lp$longitude, df_eq_lp$latitude, col = "red", pch = 16, cex = df_eq_lp$mag*0.15, alpha = 0.5)
# Add legend
legend("bottomleft", legend = "Magnitude", pch = 19, pt.cex = 2, cex = 0.8, col = "red", title = "Magnitude")

