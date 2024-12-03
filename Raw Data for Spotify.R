#--------------------------------------------------------
# Load Libraries
#--------------------------------------------------------
library(dplyr)

#--------------------------------------------------------
# Load Data
#--------------------------------------------------------
spotify_data <- read.csv("/Users/junheau/Documents/Classes/FA24/MGSC_310/datasets/spotifydataset.csv")

#--------------------------------------------------------
# Dataset Summary
#--------------------------------------------------------

# Display the structure of the dataset
cat("Structure of the dataset:\n")
str(spotify_data)

# Display the first few rows of the dataset
cat("\nFirst few rows of the dataset:\n")
head(spotify_data)

# Display the summary of each column
cat("\nSummary statistics of the dataset:\n")
summary(spotify_data)

# Check for missing values in each column
cat("\nMissing values in each column:\n")
colSums(is.na(spotify_data))

# Check the number of unique values in key columns
cat("\nNumber of unique values in key columns:\n")
spotify_data %>%
  summarise(
    Unique_Tracks = n_distinct(track_name),
    Unique_Artists = n_distinct(artists),
    Unique_Albums = n_distinct(album_name),
    Unique_Genres = if ("track_genre" %in% colnames(spotify_data)) n_distinct(track_genre) else NA
  )

# Count total rows and columns
cat("\nNumber of rows and columns in the dataset:\n")
cat("Rows:", nrow(spotify_data), "\nColumns:", ncol(spotify_data), "\n")

#--------------------------------------------------------
# Visualize Data Types and Key Insights
#--------------------------------------------------------

# Count of each data type
cat("\nData types of columns:\n")
sapply(spotify_data, class)

# Display distinct genres if the column exists
if ("track_genre" %in% colnames(spotify_data)) {
  cat("\nSample genres:\n")
  print(unique(spotify_data$track_genre))
}

# Distribution of popularity scores
cat("\nSummary of popularity scores:\n")
summary(spotify_data$popularity)

# Top 5 most popular tracks
cat("\nTop 5 most popular tracks:\n")
spotify_data %>%
  arrange(desc(popularity)) %>%
  select(track_name, artists, popularity) %>%
  head(5)