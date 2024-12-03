#--------------------------------------------------------
# Load Libraries
#--------------------------------------------------------
library(glmnet)
library(dplyr)

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
# Fit Elastic Net Model with Cross-Validation
#--------------------------------------------------------
set.seed(1818)
elastic_net_cv <- cv.glmnet(
  X_train, 
  y_train, 
  alpha = 0.5,         # Elastic Net penalty (0 = Ridge, 1 = LASSO)
  nfolds = 10          # 10-fold cross-validation
)

# Optimal lambda (penalty parameter)
best_lambda <- elastic_net_cv$lambda.min
cat("Optimal lambda for Elastic Net:", best_lambda, "\n")

# Fit the final Elastic Net model using the optimal lambda
elastic_net_model <- glmnet(X_train, y_train, alpha = 0.5, lambda = best_lambda)

#--------------------------------------------------------
# Extract Features to Keep
#--------------------------------------------------------
# Extract coefficients of the final Elastic Net model
elastic_net_coefficients <- coef(elastic_net_model)

# Convert coefficients to a matrix for interpretation
coeff_matrix <- as.matrix(elastic_net_coefficients)

# Identify features with non-zero coefficients
selected_features <- rownames(coeff_matrix)[coeff_matrix[, 1] != 0]
cat("Selected Features by Elastic Net:\n")
print(selected_features)

#--------------------------------------------------------
# Evaluate Model Performance
#--------------------------------------------------------
# Predict on the test set
preds_en <- predict(elastic_net_model, s = best_lambda, newx = X_test)

# Calculate RMSE for test set
rmse_en <- sqrt(mean((y_test - preds_en)^2))
cat("Test RMSE for Elastic Net Regression:", rmse_en, "\n")

#--------------------------------------------------------
# Visualize Cross-Validation Curve
#--------------------------------------------------------
plot(elastic_net_cv)
title("Cross-Validation Curve for Elastic Net")