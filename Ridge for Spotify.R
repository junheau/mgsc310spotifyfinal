#--------------------------------------------------------
# Load Libraries
#--------------------------------------------------------
library(glmnet)
library(dplyr)
library(ggplot2)

#--------------------------------------------------------
# Load Data
#--------------------------------------------------------
spotify_data <- read.csv("/Users/junheau/Documents/Classes/FA24/MGSC_310/datasets/spotifydataset.csv")

#--------------------------------------------------------
# Data Preparation: Filter for Popularity >= 50
#--------------------------------------------------------
spotify_data <- spotify_data %>%
  mutate(
    explicit = as.numeric(ifelse(explicit == "TRUE", 1, 0)),  # Convert to binary
    mode = as.numeric(ifelse(mode == "1", 1, 0))              # Convert to binary
  ) %>%
  filter(popularity >= 50)  # Keep only songs with popularity >= 50

# Select relevant numerical features and target variable
features <- c(
  "duration_ms", "danceability", "energy", "loudness", 
  "acousticness", "instrumentalness", "liveness", 
  "valence", "tempo", "explicit", "mode"
)
spotify_data <- spotify_data %>% select(all_of(features), popularity)

# Split data into training and testing sets
set.seed(1818)
train_indices <- sample(1:nrow(spotify_data), size = 0.8 * nrow(spotify_data))
spotify_train <- spotify_data[train_indices, ]
spotify_test <- spotify_data[-train_indices, ]

# Prepare matrices for glmnet
X_train <- as.matrix(spotify_train %>% select(-popularity))  # Features
y_train <- spotify_train$popularity                         # Target
X_test <- as.matrix(spotify_test %>% select(-popularity))   # Features
y_test <- spotify_test$popularity                          # Target

#--------------------------------------------------------
# Fit Ridge Regression Model with Cross-Validation
#--------------------------------------------------------
set.seed(1818)
ridge_cv <- cv.glmnet(X_train, y_train, alpha = 0)  # Ridge regression with alpha = 0
best_lambda <- ridge_cv$lambda.min  # Optimal penalty parameter

# Print the optimal lambda
cat("Optimal lambda for Ridge Regression:", best_lambda, "\n")

# Fit the final Ridge model using the optimal lambda
ridge_model <- glmnet(X_train, y_train, alpha = 0, lambda = best_lambda)

#--------------------------------------------------------
# Interpret Ridge Results
#--------------------------------------------------------
# Extract coefficients of the final Ridge model
ridge_coefficients <- coef(ridge_model)

# Convert coefficients to a matrix for interpretation
coeff_matrix <- as.matrix(ridge_coefficients)

# Print all coefficients
cat("Ridge Regression Coefficients:\n")
print(coeff_matrix)

#--------------------------------------------------------
# Evaluate Model Performance
#--------------------------------------------------------
# Predict on the test set
preds_ridge <- predict(ridge_model, s = best_lambda, newx = X_test)

# Calculate RMSE for test set
rmse_ridge <- sqrt(mean((y_test - preds_ridge)^2))
cat("Test RMSE for Ridge Regression:", rmse_ridge, "\n")

#--------------------------------------------------------
# Visualize Cross-Validation
#--------------------------------------------------------
# Plot cross-validation curve
plot(ridge_cv)
title("Cross-Validation Curve for Ridge Regression")