# Arjun Rao

#------------------------------------------------------------
# Load Data and Packages
#------------------------------------------------------------
options(scipen = 10)
set.seed(1818)

# Install and load required packages
if (!require("glmnet")) install.packages("glmnet")
if (!require("tidyverse")) install.packages("tidyverse")
library(glmnet)
library(tidyverse)

#------------------------------------------------------------
# Load and Preprocess Data
#------------------------------------------------------------
file_path <- "/Users/arjunrao/Desktop/MGSC_310/final_project/dataset.csv"  # Update with your file path
spotify_df <- read.csv(file_path) %>%
  as_tibble() %>%
  mutate(
    explicit = as.numeric(as.factor(explicit))  # Convert 'explicit' to numeric (0, 1)
  ) %>%
  select(popularity, duration_ms, explicit, danceability, energy) %>%
  na.omit()  # Remove rows with missing values

# Convert predictors to a matrix (required for glmnet)
X <- spotify_df %>%
  select(duration_ms, explicit, danceability, energy) %>%
  as.matrix()

# Response variable
y <- spotify_df$popularity

#------------------------------------------------------------
# Train-Test Split
#------------------------------------------------------------
set.seed(1818)
train_idx <- sample(1:nrow(spotify_df), size = 0.8 * nrow(spotify_df))
X_train <- X[train_idx, ]
y_train <- y[train_idx]
X_test <- X[-train_idx, ]
y_test <- y[-train_idx]

#------------------------------------------------------------
# Fit LASSO Model
#------------------------------------------------------------
# Perform cross-validated LASSO
cv_lasso <- cv.glmnet(X_train, y_train, alpha = 1)  # alpha = 1 specifies LASSO

# Optimal lambda
optimal_lambda <- cv_lasso$lambda.min
cat("Optimal Lambda:", optimal_lambda, "\n")

# Fit final LASSO model using optimal lambda
lasso_model <- glmnet(X_train, y_train, alpha = 1, lambda = optimal_lambda)

#------------------------------------------------------------
# Evaluate Model Performance
#------------------------------------------------------------
# Predict on test set
predictions <- predict(lasso_model, s = optimal_lambda, newx = X_test)

# Calculate MAE and R-squared
mae <- mean(abs(predictions - y_test))
r2 <- cor(predictions, y_test)^2

cat("Mean Absolute Error (MAE):", mae, "\n")
cat("R-squared:", r2, "\n")

#------------------------------------------------------------
# Extract and Visualize Coefficients
#------------------------------------------------------------
coef_df <- as.data.frame(as.matrix(coef(lasso_model, s = optimal_lambda)))
colnames(coef_df) <- c("Coefficient")
coef_df$Feature <- rownames(coef_df)
coef_df <- coef_df %>%
  filter(Coefficient != 0 & Feature != "(Intercept)") %>%
  arrange(desc(abs(Coefficient)))

# Plot the coefficients
ggplot(coef_df, aes(x = reorder(Feature, Coefficient), y = Coefficient)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "LASSO Feature Coefficients", x = "Features", y = "Coefficient") +
  theme_minimal()

# Dot plot for coefficients
ggplot(coef_df, aes(x = reorder(Feature, Coefficient), y = Coefficient)) +
  geom_point(size = 4, color = "darkred") +
  geom_segment(aes(x = Feature, xend = Feature, y = 0, yend = Coefficient), 
               color = "gray", linetype = "dashed") +
  coord_flip() +
  labs(title = "LASSO Coefficient Magnitudes",
       x = "Features",
       y = "Coefficient Value") +
  theme_minimal()

# Lollipop plot for coefficients
ggplot(coef_df, aes(x = reorder(Feature, Coefficient), y = Coefficient)) +
  geom_segment(aes(x = Feature, xend = Feature, y = 0, yend = Coefficient), 
               color = "steelblue", size = 1) +
  geom_point(size = 4, color = "darkblue") +
  coord_flip() +
  labs(title = "LASSO Feature Contributions (Lollipop Chart)",
       x = "Features",
       y = "Coefficient Value") +
  theme_minimal()



