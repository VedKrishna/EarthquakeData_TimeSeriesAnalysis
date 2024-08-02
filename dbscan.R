library(httr)
library(jsonlite)
library(dplyr)
library(ggplot2)
library(pickle)
library(leaflet)  # Equivalent to folium for interactive maps
library(geosphere)  # For geospatial calculations
library(maps)  # For basic maps
library(mapproj)  # For map projections
# Import required packages
library(caret)  # For preprocessing
library(cluster)  # For clustering algorithms
library(datasets)  # For loading example datasets
library(dbscan)  # For DBSCAN clustering
library(ggplot2)  # For plotting
library(Rtsne)  # For t-SNE dimensionality reduction
library(randomForest)
library(caret)  # For model evaluation and metrics
library(IRdisplay)  # For inline display in RMarkdown or Jupyter
library(repr)  # For plot size control in RMarkdown or Jupyter
library(xts)
options(repr.plot.width = 6, repr.plot.height = 4)  # Adjust width and height as needed



# Define file path
file_path <- "C:\\Users\\pvedk\\pvedk\\Desktop\\programming\\MinorProject2\\datasets\\combined_eq_california_timeseries.csv"

# Read CSV file into a data frame
df_eq <- read.csv(file_path)

# Convert the index column to a time column
df_eq$time <- as.POSIXct(df_eq$time, format = "%Y-%m-%d %H:%M:%S")

# Show the first 3 rows of the data frame
head(df_eq, 3)


# Filter the DataFrame based on the "mag" column condition
df_eq_large <- subset(df_eq, mag > 6)

# Convert the "time" column to POSIXct date-time format
df_eq_large$time <- as.POSIXct(df_eq_large$time, format = "%Y-%m-%d %H:%M:%S")

# Calculate time differences in days
df_eq_large$time_diff_day <- c(NA, diff(df_eq_large$time)) / (24 * 60 * 60)  # Assuming time difference in seconds

# Display the first few rows of the filtered DataFrame
head(df_eq_large, 2)



# Convert 'time' column to Date format (assuming 'time' is already a column in POSIXct format)
df_eq$time <- as.Date(df_eq$time)

# Select rows within the specified date range
start_date <- as.Date("1989-09-19")
end_date <- as.Date("1989-10-19")
df_eq_sel_range <- subset(df_eq, time >= start_date & time <= end_date)

# Show the first 2 rows of the selected range
head(df_eq_sel_range, 2)



# Set up the basemap
map <- leaflet() %>%
  addTiles() %>%
  setView(lng = -120, lat = 38, zoom = 5)

# Scatter plot
map <- map %>%
  addCircleMarkers(data = df_eq_sel_range,
                   lng = ~longitude,
                   lat = ~latitude,
                   radius = ~mag/5,
                   color = "red",
                   opacity = 0.5)

# Display the map
map



# Convert time column to POSIXct if it's not already in that format
df_eq_sel_range$time <- as.POSIXct(df_eq_sel_range$time)

# Create a line plot of longitude over time
ggplot(data = df_eq_sel_range, aes(x = time, y = longitude)) +
  geom_line(color = "red") +
  geom_point(color = "red") +
  labs(title = "Longitude", x = "Time", y = "Longitude") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Create data frame for clustering
df <- df_eq_sel_range[, c("longitude", "latitude", "depth")]

# Standardize the data
ss <- scale(df)

# Set parameters for DBSCAN
eps_param <- 0.33
min_sample <- 7
# Perform DBSCAN clustering
dbscan <- dbscan::dbscan(ss, eps = eps_param, minPts = min_sample)

# Add cluster labels to the data frame
df$cluster <- dbscan$cluster

# Identify core samples
core_samples_mask <- dbscan$borderPoints

# Count clusters and noise points
n_clusters <- length(unique(dbscan$cluster)) - (1 %in% dbscan$cluster)
n_noise <- sum(dbscan$cluster == 0)

# Print cluster information
cat("Total number of points:", nrow(df), "\n")
cat("Estimated number of clusters:", n_clusters, "\n")
cat("Estimated number of noise points:", n_noise, "\n")
#cat("Silhouette Coefficient:", silhouette(ss, dbscan$cluster), "\n")

# Count the occurrences of each cluster label
cluster_counts <- table(df$cluster)
# Print the counts
print(cluster_counts)

# Get the top cluster
top_cluster <- names(sort(table(df$cluster), decreasing = TRUE))[1]
cat("Top cluster is:", top_cluster, "\n")

# Plotting the clusters
if (top_cluster == "-1") {
  cluster_mask <- names(sort(table(df$cluster), decreasing = TRUE))[2]
} else {
  cluster_mask <- top_cluster
}

df_filtered <- df[df$cluster != -1, ]

# Plot the clusters
plot <- ggplot(df_filtered, aes(x = longitude, y = latitude, color = factor(cluster))) +
  geom_point(size = 3) +
  scale_color_manual(values = c("gray", "blue", "green", "red", "orange","pink","black")) +
  labs(title = "Clusters", x = "Longitude", y = "Latitude") +
  theme_minimal()

plot

# Print the first 10 elements of the core samples mask
print(head(core_samples_mask, 10))


# Filter df_eq_sel_range for the top cluster
df_top_cluster <- df_eq_sel_range[df$cluster == cluster_mask, ]

palette <- colorFactor(palette = "viridis", domain = df$cluster)
map <- leaflet() %>%
  addTiles() %>%
  setView(lng = -120, lat = 38, zoom = 5)
# Scatter plot
map <- map %>%
  addCircleMarkers(data = df,
                   lng = ~longitude,
                   lat = ~latitude,
                   radius = 5,
                   color = ~palette(cluster),
                   opacity = 0.5)
# Display the map
map


# Create a data frame with cluster labels

# Get cluster labels from the result
labels <- dbscan$cluster
df$cluster_labels <- factor(labels)

# Plot the scatterplot with different colors for each cluster
plot <- ggplot(df, aes(x = longitude, y = latitude, color = palette(cluster))) +
  geom_point(size = 3) +
  scale_color_manual(values = c("gray", "blue", "green", "red", "orange", "yellow", "brown")) +
  labs(title = "Clusters", x = "Longitude", y = "Latitude") +
  theme_minimal()

# Display the legend
plot + guides(color = guide_legend(title = "Cluster"))


# Filter the data frame to exclude rows where cluster equals -1
df_filtered <- df[df$cluster != -1, ]

# Get the unique values of the "cluster" column in the filtered data frame
unique_clusters <- unique(df_filtered$cluster)

# Print the unique cluster values
print(unique_clusters)


# Define eps_param
eps_param <- 0.33

# Access the value of eps_param
print(eps_param)


#Using KNN to plot the elbow curve for the data
# Load required libraries
library(FNN)  # For k-nearest neighbors
# Fit k-nearest neighbors
nn <- get.knn(ss, k = min_sample)

# Calculate distances
distances <- nn$nn.dist[, min_sample]

# Sort distances
sorted_distances <- sort(distances)

# Identify the knee point using the inflection package
knee_point <- findiplist(1:length(sorted_distances), sorted_distances, 1)
# Ensure the knee point is a single numeric value
knee_point_value <- knee_point[[2]]
# Plot the knee point
ggplot(data = data.frame(i = 1:length(sorted_distances), distances = sorted_distances), aes(x = i, y = distances)) +
  geom_line() +
  geom_vline(xintercept = knee_point_value, linetype = "dashed", color = "red") +
  labs(x = "Points", y = "Distance", title = "Knee Point Detection") +
  theme_minimal()

# Print the distance at the knee point
print(sorted_distances[knee_point_value])

