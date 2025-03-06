# Assessment1 SmartWatch


################## SmartWatch CASE ###################

# Install necessary packages if not already installed
install.packages("readxl")
install.packages("tidyverse")
install.packages("cluster")
install.packages("openxlsx")


# Load the required libraries
library(readxl)      # For reading Excel files
library(tidyverse)   # For data manipulation and visualisation
library(cluster)     # For clustering methods
library(openxlsx)    # For exporting data to Excel


################## PREPARATION ###################

# IMPORTING DATA FROM EXCEL 
SmartWatch <- read_excel(file.choose())
# View the imported dataset (opens in a separate viewer)
View(SmartWatch)


# INITIAL DATA EXPLORATION 
# Display column names of the dataset
names(SmartWatch)
# Display basic summary statistics (e.g., min, max, mean, etc.) for each variable
summary(SmartWatch)


################### SEGMENTATION STEP ###################

# CALCULATE EUCLIDEAN DISTANCE
# "dist()" calculates pairwise distances between rows of a dataset.
distance <- dist(SmartWatch, method = 'euclidean')
distance_matrix <- as.matrix(distance)
print(distance_matrix)

# CLUSTER DENDROGRAM 
# Perform hierarchical clustering
hc.w <- hclust(distance, method = 'ward.D')
# Plot the dendrogram to visualise the clustering
plot(hc.w, main = "Cluster Dendrogram", xlab = "Observations", ylab = "Height")


# DETERMINE THE OPTIMAL NUMBER OF CLUSTERS
# Use the elbow method to decide the number of clusters.
x <- c(1:10)
sort_height <- sort(hc.w$height, decreasing = TRUE)
y <- sort_height[1:10]

# Plot elbow plot
plot(x, y, type = "b", main = "Elbow Plot", xlab = "Number of Clusters", ylab = "Height") 
lines(x, y, col = "blue")
# After cluster=3,  adding more clusters isn’t significantly improving the fit within the clusters. 
# We can consider 3 as the optimal number of clusters.
# Display 3 clusters on dendrogram
plot(hc.w, main = "Cluster Dendrogram", xlab = "Observations", ylab = "Height")
rect.hclust(hc.w, k = 3, border = 2:5)

# CUT DENDROGRAM INTO 3 CLUSTERS
# "cutree()" assigns each observation to a cluster.
cluster <- cutree(hc.w, k = 3)
# Create a frequency table to see the size of each cluster
table(cluster)
# Add cluster assignments back to the original data
SmartWatch_with_clusters <- cbind(SmartWatch, cluster)
# Save the new dataset with clusters to a new CSV file
write.csv(SmartWatch_with_clusters, "SmartWatch_with_clusters.csv", row.names = FALSE)
# Check the updated dataset
View(SmartWatch_with_clusters)


################### DESCRIPTION STEP ###################

# CALCULATE SEGMENT SIZES
proportions <- table(SmartWatch_with_clusters$cluster) / nrow(SmartWatch_with_clusters)
percentages <- proportions * 100
# Display segment sizes in percentages
print(percentages)

# PERCENTAGE ANALYSIS BY CLUSTER (For categorical variables AmznP, Female, Degree, and Income)
# 1. Percentage for AmznP
amznp_percentage_by_cluster <- SmartWatch_with_clusters %>%
  group_by(cluster) %>%
  summarise(amznp_percentage = mean(AmznP) * 100)
print("Amazon Prime Account Percentage by Cluster:")
print(amznp_percentage_by_cluster)

# 2. Percentage for Female (Gender)
female_percentage_by_cluster <- SmartWatch_with_clusters %>%
  group_by(cluster) %>%
  summarise(female_percentage = mean(Female) * 100)
print("Female Percentage by Cluster:")
print(female_percentage_by_cluster)

# 3. Percentage for Degree (Education Level)
degree_percentage_by_cluster <- SmartWatch_with_clusters %>%
  group_by(cluster) %>%
  summarise(degree_percentage = mean(Degree == 2 | Degree == 3) * 100)
print("Degree Percentage by Cluster:")
print(degree_percentage_by_cluster)

# SAVE PERCENTAGE ANALYSIS TO EXCEL
percentage_results <- list(
  "AmznP Percentage" = amznp_percentage_by_cluster,
  "Female Percentage" = female_percentage_by_cluster,
  "Degree Percentage" = degree_percentage_by_cluster)

# Write the percentage analysis to Excel as a single workbook
write.xlsx(percentage_results, 'SmartWatch_Segments percentage.xlsx')

# EXPLORE MEAN VALUES OF VARIABLES IN EACH CLUSTER (Excluding AmznP, Female, Degree, Income)
SmartWatch_without_categoricals <- SmartWatch_with_clusters %>% 
  select(-AmznP, -Female, -Degree)
segments <- 
  SmartWatch_without_categoricals %>% 
  group_by(cluster) %>% 
  summarise(across(where(is.numeric), mean, .names = "{col}_mean"))

# Display the calculated means
segments

# SAVE MEAN TABLE TO EXCEL 
write.xlsx(segments, 'SmartWatch_Segments mean.xlsx')
