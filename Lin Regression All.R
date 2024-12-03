#--------------------------------------------------------
# Load Libraries
#--------------------------------------------------------
library(dplyr)
library(ggplot2)
library(rsample)

#--------------------------------------------------------
# Load Data
#--------------------------------------------------------
spotify_data <- read.csv("/Users/junheau/Documents/Classes/FA24/MGSC_310/datasets/spotifydataset.csv")

#--------------------------------------------------------
# Data Preparation: Use Entire Dataset and Select Specified Variables
#--------------------------------------------------------
spotify_data <- spotify_data %>%
  select(
    duration_ms, danceability, energy, loudness, speechiness,
    acousticness, instrumentalness, liveness, valence, tempo, popularity
  )  # Use only the specified variables

# Split data into training and testing sets
set.seed(1818)
spotify_split <- initial_split(spotify_data, prop = 0.8)
spotify_train <- training(spotify_split)
spotify_test <- testing(spotify_split)

#--------------------------------------------------------
# Linear Regression: Duration (duration_ms)
#--------------------------------------------------------
mod_duration <- lm(popularity ~ duration_ms, data = spotify_train)
summary(mod_duration)

ggplot(spotify_train, aes(x = duration_ms, y = popularity)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(
    title = "Popularity vs Duration (ms)",
    x = "Duration (ms)",
    y = "Popularity"
  ) +
  theme_minimal(base_size = 14)

#--------------------------------------------------------
# Linear Regression: Danceability
#--------------------------------------------------------
mod_danceability <- lm(popularity ~ danceability, data = spotify_train)
summary(mod_danceability)

ggplot(spotify_train, aes(x = danceability, y = popularity)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(
    title = "Popularity vs Danceability",
    x = "Danceability",
    y = "Popularity"
  ) +
  theme_minimal(base_size = 14)

#--------------------------------------------------------
# Linear Regression: Energy
#--------------------------------------------------------
mod_energy <- lm(popularity ~ energy, data = spotify_train)
summary(mod_energy)

ggplot(spotify_train, aes(x = energy, y = popularity)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(
    title = "Popularity vs Energy",
    x = "Energy",
    y = "Popularity"
  ) +
  theme_minimal(base_size = 14)

#--------------------------------------------------------
# Linear Regression: Loudness
#--------------------------------------------------------
mod_loudness <- lm(popularity ~ loudness, data = spotify_train)
summary(mod_loudness)

ggplot(spotify_train, aes(x = loudness, y = popularity)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(
    title = "Popularity vs Loudness",
    x = "Loudness",
    y = "Popularity"
  ) +
  theme_minimal(base_size = 14)

#--------------------------------------------------------
# Linear Regression: Speechiness
#--------------------------------------------------------
mod_speechiness <- lm(popularity ~ speechiness, data = spotify_train)
summary(mod_speechiness)

ggplot(spotify_train, aes(x = speechiness, y = popularity)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(
    title = "Popularity vs Speechiness",
    x = "Speechiness",
    y = "Popularity"
  ) +
  theme_minimal(base_size = 14)

#--------------------------------------------------------
# Linear Regression: Acousticness
#--------------------------------------------------------
mod_acousticness <- lm(popularity ~ acousticness, data = spotify_train)
summary(mod_acousticness)

ggplot(spotify_train, aes(x = acousticness, y = popularity)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(
    title = "Popularity vs Acousticness",
    x = "Acousticness",
    y = "Popularity"
  ) +
  theme_minimal(base_size = 14)

#--------------------------------------------------------
# Linear Regression: Instrumentalness
#--------------------------------------------------------
mod_instrumentalness <- lm(popularity ~ instrumentalness, data = spotify_train)
summary(mod_instrumentalness)

ggplot(spotify_train, aes(x = instrumentalness, y = popularity)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(
    title = "Popularity vs Instrumentalness",
    x = "Instrumentalness",
    y = "Popularity"
  ) +
  theme_minimal(base_size = 14)

#--------------------------------------------------------
# Linear Regression: Liveness
#--------------------------------------------------------
mod_liveness <- lm(popularity ~ liveness, data = spotify_train)
summary(mod_liveness)

ggplot(spotify_train, aes(x = liveness, y = popularity)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(
    title = "Popularity vs Liveness",
    x = "Liveness",
    y = "Popularity"
  ) +
  theme_minimal(base_size = 14)

#--------------------------------------------------------
# Linear Regression: Valence
#--------------------------------------------------------
mod_valence <- lm(popularity ~ valence, data = spotify_train)
summary(mod_valence)

ggplot(spotify_train, aes(x = valence, y = popularity)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(
    title = "Popularity vs Valence",
    x = "Valence",
    y = "Popularity"
  ) +
  theme_minimal(base_size = 14)

#--------------------------------------------------------
# Linear Regression: Tempo
#--------------------------------------------------------
mod_tempo <- lm(popularity ~ tempo, data = spotify_train)
summary(mod_tempo)

ggplot(spotify_train, aes(x = tempo, y = popularity)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(
    title = "Popularity vs Tempo",
    x = "Tempo",
    y = "Popularity"
  ) +
  theme_minimal(base_size = 14)