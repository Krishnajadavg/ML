# Function to simulate student features
simulate_student_features <- function(n = 100) {
  set.seed(260923)  # Seed for reproducibility
  
  student_ids <- seq(1, n)  # Student IDs
  
  # Simulate engagement and performance
  student_engagement <- rnorm(n, mean = 50, sd = 10)
  student_performance <- rnorm(n, mean = 60, sd = 15)
  
  # Data frame creation
  student_features <- data.frame(
    student_id = student_ids,
    student_engagement = student_engagement,
    student_performance = student_performance
  )
  
  return(student_features)  # Return the data frame
}

# Simulate data for 100 students
student_features <- simulate_student_features(n = 100)

library(factoextra)
library(ggplot2)

# PCA on student features, excluding student_id
pca_results <- prcomp(student_features[, -1], scale. = TRUE)

# PCA visualization
fviz_pca_ind(pca_results,
             col.ind = "cos2", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)
library(cluster)

# Optimal cluster count via Elbow Method
fviz_nbclust(student_features[, -1], kmeans, method = "wss")

# KMeans clustering with 3 centers
set.seed(260923)  # Seed for reproducibility
kmeans_results <- kmeans(student_features[, -1], centers = 3)

# Cluster visualization
fviz_cluster(kmeans_results, data = student_features[, -1])

