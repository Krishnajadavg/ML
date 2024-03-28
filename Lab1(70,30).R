
# Load necessary libraries
library(tidyr)
library(caret)

# Read the dataset
data <- read.csv("C:/Users/dhara/Downloads/oulad-students.csv")

# Data preprocessing
# Modify variable selection as needed
selected_columns <- c("gender", "region", 
                      "highest_education", "imd_band", "age_band", "num_of_prev_attempts", 
                      "disability", "final_result")

data <- data[selected_columns]

# Convert categorical variables to factors
factor_columns <- c("gender", "region", 
                    "highest_education", "imd_band", "age_band", "num_of_prev_attempts", 
                    "disability", "final_result")

data[factor_columns] <- lapply(data[factor_columns], as.factor)

# Drop rows with missing values
data <- na.omit(data)

# Split the data into training and testing sets (70% training, 30% testing)
set.seed(123) # For reproducibility
train_index <- createDataPartition(data$final_result, p = 0.7, list = FALSE)

# Train and test datasets
train_data <- data[train_index, ]
test_data <- data[-train_index, ]

# Train the classification model (logistic regression)
model <- glm(final_result ~ ., data = train_data, family = "binomial")

# Make predictions on the test data (probabilities)
predictions <- predict(model, newdata = test_data, type = "response")

# Convert predicted probabilities to class labels
predicted_classes <- ifelse(predictions > 0.5, "Pass", "Fail")  # Adjust the threshold as needed

# Convert predicted_classes to factor with the same levels as test_data$final_result
predicted_classes <- factor(predicted_classes, levels = levels(test_data$final_result))

# Evaluate the model
confusionMatrix(data = predicted_classes, reference = test_data$final_result)
