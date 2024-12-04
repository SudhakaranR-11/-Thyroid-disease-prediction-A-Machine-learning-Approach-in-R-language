# Load necessary libraries
library(ggplot2)
library(e1071)
library(caret)
library(pROC)
library(rpart)
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

# Train the decision tree model
tree_model <- rpart(Result ~ ., data = train_data, method = "class")

# Make predictions on the test set
predictions <- predict(tree_model, newdata = test_data, type = "class")

# Evaluate performance using a confusion matrix
confusion_matrix <- confusionMatrix(predictions, test_data$Result)

# Print the confusion matrix
print(confusion_matrix)

# Calculate accuracy
accuracy <- confusion_matrix$overall['Accuracy']

# Print accuracy
print(paste("Accuracy:", accuracy))

# Calculate evaluation metrics: precision, recall, and F1 score
precision <- diag(confusion_matrix$table) / rowSums(confusion_matrix$table)
recall <- diag(confusion_matrix$table) / colSums(confusion_matrix$table)
f1_score <- 2 * precision * recall / (precision + recall)

# Print the evaluation metrics
print("Precision:")
print(precision)
print("Recall:")
print(recall)
print("F1 Score:")
print(f1_score)

# Calculate overall precision, recall, and F1 score
overall_precision <- mean(precision, na.rm = TRUE)
overall_recall <- mean(recall, na.rm = TRUE)
overall_f1_score <- mean(f1_score, na.rm = TRUE)

print("Overall Precision:")
print(overall_precision)
print("Overall Recall:")
print(overall_recall)
print("Overall F1 Score:")
print(overall_f1_score)

# Extract the confusion matrix table
cm_df <- as.data.frame(confusion_matrix$table)

# Rename the column names
colnames(cm_df) <- c("Predicted", "Actual", "Freq")

# Plot the confusion matrix as a heatmap
library(ggplot2)
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
corrplot(correlation_matrix, method = "color",addCoef.col = "black")



