# Load necessary libraries
library(e1071)
library(caret)
library(pROC)
library(ggplot2)

# Load the dataset
thyroid_data <- read.csv("C:/Users/SUDHAKARAN R/OneDrive/Desktop/New folder/trainedDataset.csv")

# Check for missing values
colSums(is.na(thyroid_data))

# Convert the target variable to a factor
thyroid_data$Result <- as.factor(thyroid_data$Result)

# Set random seed for reproducibility
set.seed(123)

# Split the data into training and testing sets
index <- createDataPartition(thyroid_data$Result, p = 0.7, list = FALSE)
train_data <- thyroid_data[index, ]
test_data <- thyroid_data[-index, ]

# Train the Naive Bayes model
nb_model <- naiveBayes(Result ~ ., data = train_data)

# Make predictions on the test set
predictions <- predict(nb_model, newdata = test_data)

# Evaluate performance using a confusion matrix
confusion_matrix <- confusionMatrix(predictions, test_data$Result)

# Print the confusion matrix
print(confusion_matrix)

# Calculate accuracy
accuracy <- confusion_matrix$overall['Accuracy']

# Print accuracy
print(paste("Accuracy:", accuracy))

# Calculate precision, recall, and F1 score manually
precision <- confusion_matrix$table[2,2] / sum(confusion_matrix$table[,2])
recall <- confusion_matrix$table[2,2] / sum(confusion_matrix$table[2,])
f1_score <- 2 * (precision * recall) / (precision + recall)

# Print the evaluation metrics
print("Precision:")
print(precision)
print("Recall:")
print(recall)
print("F1 Score:")
print(f1_score)

# Extract the confusion matrix table
cm_df <- as.data.frame(confusion_matrix$table)

# Rename the column names
colnames(cm_df) <- c("Predicted", "Actual", "Freq")

# Plot the confusion matrix as a heatmap
ggplot(data = cm_df, aes(x = Actual, y = Predicted, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = Freq), vjust = 1) +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(title = "Confusion Matrix", x = "Actual", y = "Predicted")

# Load necessary libraries
library(dplyr)

# Select only numeric columns from train_data
numeric_train_data <- select_if(train_data, is.numeric)

# Check if the selected columns look correct
head(numeric_train_data)

# Install and load the corrplot package
install.packages("corrplot")
library(corrplot)

# Calculate the correlation matrix
correlation_matrix <- cor(numeric_train_data)

# Print the correlation matrix
print(correlation_matrix)

# Plot the correlation matrix using corrplot
corrplot(correlation_matrix, method = "color", addCoef.col = "black")
