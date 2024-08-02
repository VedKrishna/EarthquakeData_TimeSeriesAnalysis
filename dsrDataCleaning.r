library(data.table)
library(dplyr)

path <- 'C:\\Users\\pvedk\\pvedk\\Desktop\\programming\\MinorProject2\\datasets\\imported_usgs_api'
my_files <- list.files(path, pattern = "raw.csv", full.names = TRUE)


df_combined <- rbindlist(lapply(my_files, fread))

rownames(df_combined) <- NULL

dim(df_combined)

head(df_combined, 5)

# Define a function to clean the data frame
clean_df <- function(df) {
  # Getting only the useful columns
  df_clean <- df[, c('type', 'time', 'coordinates', 'mag', 'place', 'status', 'tsunami', 'sig', 'net', 
                     'nst', 'dmin', 'rms', 'gap', 'magType'), drop = FALSE]
  
  # Remove square brackets from coordinates column
  df_clean$coordinates <- gsub("\\[|\\]", "", df_clean$coordinates)
  
  # Split coordinates into separate columns
  coordinates_split <- strsplit(df_clean$coordinates, ",")
  
  # Convert list of character vectors to a matrix of numeric values
  coordinates_matrix <- do.call(rbind, lapply(coordinates_split, as.numeric))
  
  # Ensure the matrix has the correct dimensions
  if (ncol(coordinates_matrix) == 3) {
    df_clean$longitude <- coordinates_matrix[, 1]
    df_clean$latitude <- coordinates_matrix[, 2]
    df_clean$depth <- coordinates_matrix[, 3]
  } else {
    stop("Unexpected number of dimensions in the coordinates column.")
  }
  
  # Fixing time
  df_clean$time <- as.POSIXct(as.numeric(df_clean$time) / 1000, origin = "1970-01-01")
  
  # Dropping useless coordinate column
  df_clean$coordinates <- NULL
  
  return(df_clean)
}

# Clean the data frame
df_combined_clean <- clean_df(df_combined)

df_combined_clean$depth[is.na(df_combined_clean$depth)] <- mean(df_combined_clean$depth, na.rm = TRUE)

# View the first few rows of the cleaned data frame
head(df_combined_clean)


df_eq <- df_combined_clean
# Read the CSV file into a DataFrame
df_eq <- read.csv("C:\\Users\\pvedk\\pvedk\\Desktop\\programming\\MinorProject2\\datasets\\combined_eq_california_clean.csv")

df_eq <- df_eq[ ,-1]

# Fix the time column datatype to datetime
df_eq$time <- strptime(df_eq$time, format = "%H:%M.%OS")
df_eq$time <- as.numeric(difftime(df_eq$time, as.POSIXct("1970-01-01 00:00:00")))

# Add a name column for Folium map pop-ups
df_eq$name_mag <- paste("M:", df_eq$mag, "/", sep=" ")
df_eq$name_date <- paste(as.Date(df_eq$time), "/", sep=" ")
df_eq$name <- paste(df_eq$name_mag, df_eq$name_date, df_eq$place, sep="")
df_eq <- df_eq %>% select(-name_mag, -name_date)

# Sort the DataFrame with respect to time
df_eq <- df_eq[order(df_eq$time), ]

# Set the DataFrame index to the "time" column
df_eq <- arrange(df_eq, time)

# Impute missing depth values with the mean depth
df_eq$depth[is.na(df_eq$depth)] <- mean(df_eq$depth, na.rm = TRUE)
df_eq$nst[is.na(df_eq$nst)] <- mean(df)
#dep_normalized <- (df_eq$depth - min(df_eq$depth)) / (max(df_eq$depth) - min(df_eq$depth))
#df_eq$depth <- dep_normalized
# Display the first 5 rows of the DataFrame
print(head(df_eq, 5))
summary(df_eq$mag)

