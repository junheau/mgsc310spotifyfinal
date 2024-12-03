
if (!require("partykit")) install.packages("partykit", dependencies = TRUE)
if (!require("caret")) install.packages("caret", dependencies = TRUE)
library(partykit)
library(caret)

data <- read.csv("dataset.csv")


data_subset <- data[, c("popularity", "explicit", "danceability", "energy", "key", "loudness", "mode")]


data_subset <- na.omit(data_subset)

data_subset$explicit <- as.factor(data_subset$explicit)
data_subset$mode <- as.factor(data_subset$mode)

# Split the data into training and testing sets
set.seed(123) 
trainIndex <- createDataPartition(data_subset$popularity, p = 0.8, list = FALSE)
train_data <- data_subset[trainIndex, ]
test_data <- data_subset[-trainIndex, ]

control <- ctree_control(minbucket = 20, minsplit = 40, maxdepth = 5)

tree_model <- ctree(popularity ~ explicit + danceability + energy + key + loudness + mode, 
                    data = train_data, 
                    control = control)

plot(tree_model, main = "Regression Tree for Popularity")

predictions <- predict(tree_model, newdata = test_data)

r_squared <- 1 - sum((predictions - test_data$popularity)^2) / 
  sum((test_data$popularity - mean(test_data$popularity))^2)
mse <- mean((predictions - test_data$popularity)^2)

cat("R-squared: ", r_squared, "\n")
cat("Mean Squared Error (MSE): ", mse, "\n")

importance <- varimp(tree_model, conditional = TRUE)
cat("Feature Importance:\n")
print(importance)

