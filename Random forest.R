install.packages("caret")
install.packages("ROCR")
library(ROCR)
library(caret)
df <- read.csv("C:/Users/SUDHAKARAN R/OneDrive/Desktop/New folder/trainedDataset.csv")
sum(is.na(df))
# To achieve reproducible model; set the random seed number
set.seed(100)



# Performs stratified random split of the data set
TrainingIndex <- createDataPartition(df$Result, p=0.8, list = FALSE)
TrainingSet <- df[TrainingIndex,] # Training Set
TestingSet <- df[-TrainingIndex,] # Test Set

# Create histograms for 'Age' in the training set and testing set
hist_training <- ggplot(TrainingSet, aes(x = Age)) +
  geom_histogram(fill = "skyblue", color = "black") +
  labs(title = "Training Set - Age Distribution", x = "Age", y = "Frequency")

hist_testing <- ggplot(TestingSet, aes(x = Age)) +
  geom_histogram(fill = "lightgreen", color = "black") +
  labs(title = "Testing Set - Age Distribution", x = "Age", y = "Frequency")

# Arrange plots side by side
library(gridExtra)
grid.arrange(hist_training, hist_testing, ncol = 2)





# Check the unique values/classes in your target variable
unique_classes <- unique(df$Result)
# Convert Result to factor (if it's not already)
TrainingSet$Result <- as.factor(TrainingSet$Result)

# Convert the target variable to numeric

# Option 2: Using match()
df$Result <- match(df$Result, unique_classes)


# Print the unique numeric values/classes
unique(df$Result)
print(df)

# Load necessary packages
install.packages("randomForest")
library(randomForest)

# Assuming your thyroid dataset is already loaded and split into TrainingSet and TestingSet

# Train Random Forest model
rf_model <- randomForest(Result ~ ., data = TrainingSet)

# Make predictions on the testing set
predictions <- predict(rf_model, TestingSet)

# Evaluate model performance (e.g., accuracy)
accuracy <- mean(predictions == TestingSet$Result)
print(accuracy)
# Print the model summary
print(rf_model)



# Create a factor for TestingSet$Result with the same levels as predictions
TestingSet$Result <- factor(TestingSet$Result, levels = levels(predictions))

# Now, you can compute the confusion matrix
conf_matrix <- confusionMatrix(data = predictions, reference = TestingSet$Result)

# Print the confusion matrix
print(conf_matrix)

# Generate classification report
classification_report <- summary(conf_matrix)

# Print the classification report
print(classification_report)


# Assuming you have generated the confusion matrix and stored it in 'conf_matrix'

# Convert 'conf_matrix' to matrix if it's not already in matrix format
conf_matrix <- as.matrix(conf_matrix)

# Calculate precision
precision <- diag(conf_matrix) / colSums(conf_matrix)

# Calculate recall (sensitivity)
recall <- diag(conf_matrix) / rowSums(conf_matrix)

# Calculate F1-score
f1_score <- 2 * (precision * recall) / (precision + recall)

# Print precision, recall, and F1-score for each class
for (i in 1:nrow(conf_matrix)) {
  cat("Class:", rownames(conf_matrix)[i], "\n")
  cat("Precision:", precision[i], "\n")
  cat("Recall:", recall[i], "\n")
  cat("F1-score:", f1_score[i], "\n\n")
}

# Calculate micro-averaged precision, recall, and F1-score
micro_precision <- sum(diag(conf_matrix)) / sum(conf_matrix)
micro_recall <- sum(diag(conf_matrix)) / sum(conf_matrix)
micro_f1_score <- 2 * (micro_precision * micro_recall) / (micro_precision + micro_recall)

# Print micro-averaged precision, recall, and F1-score
cat("Micro-averaged Precision:", micro_precision, "\n")
cat("Micro-averaged Recall:", micro_recall, "\n")
cat("Micro-averaged F1-score:", micro_f1_score, "\n")

# Calculate macro-averaged precision, recall, and F1-score
macro_precision <- mean(precision, na.rm = TRUE)
macro_recall <- mean(recall, na.rm = TRUE)
macro_f1_score <- mean(f1_score, na.rm = TRUE)

# Print macro-averaged precision, recall, and F1-score
cat("\nMacro-averaged Precision:", macro_precision, "\n")
cat("Macro-averaged Recall:", macro_recall, "\n")
cat("Macro-averaged F1-score:", macro_f1_score, "\n")

dev.off()

# Install and load the pheatmap package
install.packages("pheatmap")
library(pheatmap)

# Create the clustered heatmap
pheatmap(conf_matrix,
         main = "Confusion Matrix Heatmap",
         cluster_rows = FALSE,  # Don't cluster rows
         cluster_cols = FALSE,  # Don't cluster columns
         color = colorRampPalette(c("white", "blue"))(100),  # Color scheme
         fontsize = 8,          # Font size
         cellwidth = 20,        # Cell width
         cellheight = 20)   


# Install and load the pheatmap package if not already installed
install.packages("pheatmap")
library(pheatmap)


# Create the clustered heatmap for the confusion matrix with numbers displayed
pheatmap(
  conf_matrix,
  main = "Confusion Matrix Heatmap with Numbers",
  cluster_rows = FALSE,       # Don't cluster rows
  cluster_cols = FALSE,       # Don't cluster columns
  color = colorRampPalette(c("white", "blue"))(100),  # Color scheme
  fontsize = 8,               # Font size
  cellwidth = 20,             # Cell width
  cellheight = 20,            # Cell height
  display_numbers = TRUE      # Display numeric values
)


# Install and load the corrplot package
install.packages("corrplot")
library(corrplot)

# Calculate correlation matrix
correlation_matrix <- cor(TrainingSet[, sapply(TrainingSet, is.numeric)])

# Visualize correlation matrix using heatmap
corrplot(correlation_matrix, method = "color", addCoef.col = "black")










