# Load required libraries
library(httr)
library(jsonlite)
library(tidyverse) # for data manipulation
library(ggplot2)   # for plotting
library(folium)    # for interactive maps
library(leaflet)   # for interactive maps
library(rgdal)     # for spatial data
library(sp)        # for spatial data
library(rgeos)     # for spatial data
library(cluster)   # for clustering


file_path <- paste0("C:\\Users\\Akhil_kamuni\\OneDrive\\Desktop\\MP_2\\MP_2\\datasets\\combined_eq_california_timeseries.csv")

# Read the CSV file into a data frame
df_eq <- read.csv(file_path)
df_eq$time <- as.Date(df_eq$time)  # Assuming "time" is in date format
df_eq_ts <- ts(df_eq, start = 1, frequency = 1)  # Assumi
head(df_eq_ts, 3)

str(df_eq)

# Set the figure size (optional)
options(repr.plot.width=7, repr.plot.height=4)

# Create a histogram plot of earthquake magnitudes
hist(log(df_eq$mag), main = "Histogram of EQ Magnitude", xlab = "EQ Magnitude", ylab = "Frequency", col = "lightblue")


# Set the figure size (optional)
options(repr.plot.width=7, repr.plot.height=4)

# Create a histogram plot of earthquake depths
hist(log(df_eq$depth), main = "Histogram of EQ Depth", xlab = "EQ Depth", ylab = "Frequency", col = "lightblue")


# Set the figure size (optional)
options(repr.plot.width=7, repr.plot.height=4)

# Create a histogram plot of earthquake latitudes
hist(log(df_eq$latitude), main = "Histogram of EQ Latitude", xlab = "EQ Latitude", ylab = "Frequency", col = "lightblue")


# Filter the data frame to include earthquakes with magnitude greater than 6
df_eq_large <- subset(df_eq, mag > 6)

# Convert the "time" column to datetime format
df_eq_large$time <- as.POSIXct(df_eq_large$time)

# Calculate the time difference in days
df_eq_large$time_diff_day <- c(0, diff(df_eq_large$time))

# Display the first 5 rows of the processed data frame
head(df_eq_large, 5)


# Get information about the data frame
str(df_eq_large)

# Get the number of rows in the data frame
num_rows <- nrow(df_eq_large)

# Print the number of rows
print(num_rows)


# Get a summary of the "time_diff_day" column
summary(df_eq_large$time_diff_day)


# Calculate specific percentiles for the "time_diff_day" column
quantile(df_eq_large$time_diff_day, probs = c(0.25, 0.5, 0.75))

# Set the figure size (optional)
options(repr.plot.width=7, repr.plot.height=4)

# Convert the time difference to seconds
df_eq_large$time_diff_sec <- as.numeric(df_eq_large$time_diff_day, units = "secs")

# Create a histogram plot of the time difference in seconds
hist(df_eq_large$time_diff_sec, main = "Histogram of EQ Time Difference (Seconds)", xlab = "EQ Time Difference (Seconds)", ylab = "Frequency", col = "lightblue")


# Set the figure size (optional)
options(repr.plot.width=9, repr.plot.height=5)

# Create a plot of magnitudes
plot(df_eq_large$mag, type = "o", col = "red", pch = 16, xlab = "Index", ylab = "Magnitude", main = "Magnitude")


# Set the figure size (optional)
options(repr.plot.width=7, repr.plot.height=4)

# Create a scatter plot of EQ magnitude vs. time difference in seconds
plot(df_eq_large$time_diff_sec, df_eq_large$mag, pch = 16, col = "blue", xlab = "EQ Time Difference (Seconds)", ylab = "EQ Magnitude")


# Set the figure size (optional)
options(repr.plot.width=10, repr.plot.height=7)

# Create a scatter plot of EQ longitude vs. latitude with marker size based on magnitude
plot(df_eq_large$longitude, df_eq_large$latitude, pch = 16, col = "blue", cex = df_eq_large$mag/0.05, 
     xlab = "EQ Longitude", ylab = "EQ Latitude", main = "Earthquake Locations (Size based on Magnitude)")

# Add legend for marker size (magnitude)
legend("topleft", legend = "Magnitude", pch = 16, col = "blue", pt.cex = df_eq_large$mag/0.05)

# Adjust plot margins
par(mar = c(5, 5, 4, 2) + 0.1)

# Add labels and title
title(main = "Earthquake Locations (Size based on Magnitude)", xlab = "EQ Longitude", ylab = "EQ Latitude")


# Convert the list of datetime objects to date objects
date_list <- as.Date(list(df_eq_large$time))

# Check the type of the first element
typeof(date_list[[1]])

library(leaflet)
library(htmltools)

# Create a leaflet map
m <- leaflet() %>%
  setView(lng = -120, lat = 38, zoom = 5) %>%
  addTiles()  # Add default OpenStreetMap tiles

# Create a marker cluster group for the earthquake markers
markerCluster <- leaflet::markerClusterOptions()

# Add earthquake markers to the map
for (i in 1:nrow(df_eq_large)) {
  m <- addCircleMarkers(map = m,
                        lng = df_eq_large[i, "longitude"],
                        lat = df_eq_large[i, "latitude"],
                        radius = df_eq_large[i, "mag"] * 10,  # Adjust radius based on magnitude
                        color = "red",
                        fillOpacity = 0.5,
                        clusterOptions = markerCluster)
}

# Print the map
m


# Install and load necessary packages
install.packages("leaflet")
install.packages("htmltools")
library(leaflet)
library(htmltools)

# Define city and get location coordinates
city <- "California"
location <- geocode(city)
cat("Location:", location$lat, location$lon, "\n")

# Define data and preprocessing steps
df_eq_large <- read.csv("path_to_your_file.csv")  # Load your data
data <- df_eq_large  # Create a copy of the data frame

# Create color column
data$color <- "black"
lst_colors <- c("black")
lst_elements <- sort(unique(data$color))

# Create size column (scaled)
data$size <- scale(data$mag, center = FALSE, scale = c(3, 15))

# Initialize the leaflet map
map_ <- leaflet() %>%
  setView(lng = location$lon, lat = location$lat, zoom = 7) %>%
  addTiles("cartodbpositron")

# Add points to the map
for (i in 1:nrow(data)) {
  map_ <- addCircleMarkers(map = map_,
                           lng = data[i, "longitude"],
                           lat = data[i, "latitude"],
                           popup = data[i, "name"],
                           color = data[i, "color"],
                           radius = data[i, "size"],
                           fillOpacity = 0.7)
}

# Create HTML legend
legend_html <- paste0(
  '<div style="position:fixed; bottom:10px; left:10px; border:2px solid black; z-index:9999; font-size:14px;">',
  '<b>', color, ':</b><br>'
)

for (i in lst_elements) {
  legend_html <- paste0(legend_html, '<i class="fa fa-circle fa-1x" style="color:', lst_colors[lst_elements == i], '"></i>&nbsp;', i, '<br>')
}

legend_html <- paste0(legend_html, '</div>')

# Add legend to the map
map_ <- htmlwidgets::prependContent(map_, htmltools::HTML(legend_html))

# Print the map
map_



