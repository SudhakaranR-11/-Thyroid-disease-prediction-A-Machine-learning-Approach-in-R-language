#Required packages
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
install.packages("SummarizeExperiment")
BiocManager::install("SummarizedExperiment")
BiocManager::install("limma")
BiocManager::install("ggplot2")
install.packages("limma")

#Essential library
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
library(tidyverse)
library(dplyr)
library(tidyr)
library(SummarizedExperiment)
library(limma)
#DATA DESCRIPTION
TD <- read.csv("C:/Users/SUDHAKARAN R/OneDrive/Desktop/New folder/trainedDataset.csv")
TD_HEAD <-head(TD,nrow(TD))

TD<-as.data.frame(TD)
# Display the modified dataframe
print(TD)

# Explore the structure of the dataset (columns, data types, missing values, etc.)
str(TD)
summary(TD)
colnames(colData(TD))
table(TD@colData$Sex)
colnames(TD)
table(TD$Sex)


#DATA MANIPULATION
Gender<-TD[,c(3)]
Gender<-as.data.frame(Gender)

T3<-TD[,c(4)]
T3<-as.data.frame(T3)


T4<-TD[,c(5)]
T4<-as.data.frame(T4)

TSH<-TD[,c(6)]
TSH<-as.data.frame(TSH)






# Subset the data for each combination of Sex and hormone
M_T3 <- TD %>%
  filter(Sex == "M") %>%
  select(parameterT3) %>%
  mutate(Trial = "M-T3")

F_T3 <- TD %>%
  select(parameterT3) %>%
  mutate(Trial = "F-T3")

M_T4 <- TD %>%
  filter(Sex == "M") %>%
  select(parameterT4) %>%
  mutate(Trial = "M-T4") %>%
  mutate(parameterT4 = parameterT4 * 10)

F_T4 <- TD %>%
  filter(Sex == "F") %>%
  select(parameterT4) %>%
  mutate(Trial = "F-T4") %>%
  mutate(parameterT4 = parameterT4 * 10)

M_TSH <- TD %>%
  filter(Sex == "M") %>%
  select(parameterTSH) %>%
  mutate(Trial = "M-TSH") %>%
  mutate(parameterTSH = parameterTSH * 10)

F_TSH <- TD %>%
  filter(Sex == "F") %>%
  select(parameterTSH) %>%
  mutate(Trial = "F-TSH") %>%
  mutate(parameterTSH = parameterTSH * 10)

# Combine the dataframes
cTD <- bind_rows(M_T3,F_T3,M_T4,F_T4,M_TSH,F_TSH)

# Melt the data
mTD <- cTD %>%
  pivot_longer(cols = c(parameterT3, parameterT4, parameterTSH), names_to = "Hormones", values_to = "value")

# Plotting
ggplot(mTD, aes(x = Trial, y = value, fill = Hormones)) +
  geom_boxplot() +
  ylim(0, 200) +
  labs(title = "Box Plot", x = "Trial", y = "Value")

#Density of T3
ggplot(TD, aes(x = parameterT3)) +
  geom_density(fill = "violet", alpha = 0.5) +
  labs(title = "Density Plot", x = "Parameter T3", y = "Density")

#Density of T4
ggplot(TD, aes(x = parameterT4)) +
  geom_density(fill = "darkviolet", alpha = 0.5) +
  labs(title = "Density Plot", x = "Parameter T4", y = "Density")

#Density of TSH
ggplot(TD, aes(x = parameterTSH)) +
  geom_density(fill = "purple", alpha = 0.5) +
  labs(title = "Density Plot", x = "Parameter TSH", y = "Density")


#Multivariate Analysis
# Get unique values in the 'Result' column
unique_values <- unique(TD$Result)
print(unique_values)

# Get counts of unique values in the 'Result' column
result_counts <- table(TD$Result)

# Create pie chart
pie(result_counts, main = "Result Distribution", col = rainbow(length(result_counts)), labels = NULL, cex = 1.3)

# Add legend
legend("top", legend = names(result_counts), fill = rainbow(length(result_counts)), 
       title = "Result", cex = 0.5,ncol = 2, xpd = TRUE)






# Assuming TD is your dataframe in R and 'Sex' is a column in it
gender_set <- unique(TD$Sex)

# Create an empty list to store the ggplot objects
plot_list <- list()

# Loop through each gender category
for (gender in gender_set) {
  # Subset the data for the current gender
  selected_data <- subset(TD, Sex == gender)
  
  # Create the boxplot for parameterTSH for the current gender
  plot_list[[length(plot_list) + 1]] <- ggplot(selected_data, aes(x = Sex, y = parameterTSH)) +
    geom_boxplot() +
    labs(title = paste("Boxplot for parameterTSH by gender:", gender))
  
  # Create the boxplot for parameterT4 for the current gender
  plot_list[[length(plot_list) + 1]] <- ggplot(selected_data, aes(x = Sex, y = parameterT4)) +
    geom_boxplot() +
    labs(title = paste("Boxplot for parameterT4 by gender:", gender))
}

# Combine and display the boxplots
multiplot <- do.call(gridExtra::grid.arrange, plot_list)
print(multiplot)


# Create the KDE plot
ggplot(df, aes(x = Age)) +
  geom_density() +
  labs(title = "Kernel Density Estimate of Age")



