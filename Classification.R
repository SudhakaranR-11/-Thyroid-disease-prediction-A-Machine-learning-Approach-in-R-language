# Load required libraries
install.packages("randomForest")
install.packages("naivebayes")
install.packages("class")
install.packages("xgboost")
install.packages("e1071")
install.packages("ROSE")
install.packages("caret")
install.packages("ROCR")
install.packages("NeuralNetTools")
install.packages("ggplot2")
install.packages("keras")
install.packages("dplyr")
install.packages("glmnet")
install.packages("DescTools")
install.packages("mltools")
install.packages("pROC")
install.packages("superml")

library(superml)
library(pROC)
library(mltools)
library(DescTools)
library(randomForest)
library(naivebayes)
library(class)
library(xgboost)
library(e1071)
library(ROSE)
library(caret)
library(ROCR)
library(NeuralNetTools)
library(ggplot2)
library(keras)
library(dplyr)
library(glmnet)

# Load the dataset
df <- read.csv("C:/Users/SUDHAKARAN R/OneDrive/Desktop/New folder/trainedDataset.csv")

# Display the first 10 rows of the dataframe
head(df,10)

# Convert 'Sex' column to a factor
df$Sex <- factor(df$Sex)

# Convert 'Sex' column to binary format (F: 0, M: 1)
df$Sex <- as.integer(df$Sex == "M")
# Convert 'Result' column to factor
df$Result <- as.factor(df$Result)

# Apply label encoding
df$Result <- as.integer(df$Result)
print(df)

----------------------------------------------------------
# Convert 'Result' column to binary format
df$Result <- ifelse(tolower(trimws(df$Result)) %in% c("hypothyroidism", "primary hypothyroidism", "more diagnosis"), 1, 0)  
-----------------------------------------------------------


# Convert columns to numeric format
df$parameterT3 <- as.numeric(df$parameterT3)
df$parameterT4 <- as.numeric(df$parameterT4)
df$parameterTSH <- as.numeric(df$parameterTSH)
df$Result <- as.numeric(df$Result)

# Convert 'Result' column to factor with correct levels
df$Result <- factor(df$Result, levels = c("hypothyroidism", "hyperthyroidism", "primary hypothyroidism", "primary hyperthyroidism", "early hyperthyroidism", "more diagnosis", "pituitary gland abnormal", "OK"))

# Convert 'Sex' column to binary format
df$Sex <- as.integer(df$Sex == "M")

# Define features (X) and target variable (y)
X <- df[, c('Age', 'Sex', 'parameterT3', 'parameterT4', 'parameterTSH')]
y <- df[, 'Result']

# Ensure y is a factor
y <- as.factor(y)

# Split the dataset into training and testing sets
set.seed(42)
train_index <- createDataPartition(y, p = 0.75, list = FALSE)
X_train <- X[train_index, ]
y_train <- y[train_index]
X_test <- X[-train_index, ]
y_test <- y[-train_index]

# Train the Random Forest classifier
rf_model <- randomForest(x = X_train, y = y_train, ntree = 1000)

# Make predictions on the test set
y_pred <- predict(rf_model, newdata = X_test)

# Calculate accuracy
accuracy <- sum(y_pred == y_test) / length(y_test)
cat("Accuracy:", accuracy, "\n")

# Evaluate the model using other metrics
print(confusionMatrix(y_pred, y_test))
