#### CODE FOR THE TELECOM CHURN PROJECT, FIRST TYPE ERROR GROUP ####

# The code below contains all the elaborations and analysis we have implemented in the project
-----------------------------------------------------------------------------------------------------------

# Load necessary libraries
library(tidyverse)
library(ggplot2)
library(GGally)
library(corrplot)
library(forcats)
library(gridExtra)
library(caret)
library(pROC)
library(randomForest)
library(e1071)
library(glmnet)
library(fpc)
library(cluster)
library(purrr)
library(cowplot)
library(tidyverse)
library(gridExtra)
library(tidyverse)
library(gridExtra)
library(factoextra)
library(tidyverse)
library(gridExtra)
library(dplyr)

-----------------------------------------------------------------------------------------------------------
  
  
####EDA####
# Load data
data <- read.csv("C:/TelecomChurn.csv")

# Rename columns
colnames(data) <- c("State", "Account.Length", "Area.Code", "International.Plan", "Voice.Mail.Plan", "Number.Vmail.Messages", "Total.Day.Minutes", "Total.Day.Calls", "Total.Day.Charge", "Total.Eve.Minutes", "Total.Eve.Calls", "Total.Eve.Charge", "Total.Night.Minutes", "Total.Night.Calls", "Total.Night.Charge", "Total.Intl.Minutes", "Total.Intl.Calls", "Total.Intl.Charge", "Number.Customer.Service.Calls", "Churn")

# Convert the categorical variables to factors
data$State <- as.factor(data$State)
data$International.Plan <- as.factor(data$International.Plan)
data$Voice.Mail.Plan <- as.factor(data$Voice.Mail.Plan)
data$Churn <- as.factor(data$Churn)

## Summary of the data
#summary(data)

## Plotting the distribution of numeric variables
data %>%
  select_if(is.numeric) %>%
  gather(key = "variable", value = "value") %>%
  ggplot(aes(value)) +
  geom_histogram(bins = 30) +
  facet_wrap(~ variable, scales = "free") +
  theme_minimal()

## Plotting categorical variables
data %>%
  select_if(is.factor) %>%
  gather(key = "variable", value = "value") %>%
  ggplot(aes(value, fill = value)) +
  geom_bar() +
  facet_wrap(~ variable, scales = "free") +
  theme_minimal() +
  theme(legend.position = "none", 
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# Calculate correlation matrix
cor_matrix <- cor(data[, c("Account.Length", "Number.Vmail.Messages", "Total.Day.Minutes", "Total.Day.Calls", "Total.Day.Charge", "Total.Eve.Minutes", "Total.Eve.Calls", "Total.Eve.Charge", "Total.Night.Minutes", "Total.Night.Calls", "Total.Night.Charge", "Total.Intl.Minutes", "Total.Intl.Calls", "Total.Intl.Charge", "Number.Customer.Service.Calls")])


# Convert correlation matrix to data frame
cor_data <- as.data.frame(as.table(cor_matrix))
colnames(cor_data) <- c("Variable1", "Variable2", "Correlation")

# Create correlation plot with rotated x-axis labels
ggplot(cor_data, aes(x = Variable1, y = Variable2, fill = Correlation)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0, limits = c(-1, 1)) +
  theme_minimal() +
  labs(title = "Correlation Plot") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


## Pairwise scatter plots with GGally
ggpairs(data[, c("Total.Day.Minutes", "Total.Day.Calls", "Total.Day.Charge", "Total.Eve.Minutes", "Total.Eve.Calls", "Total.Eve.Charge", "Total.Night.Minutes", "Total.Night.Calls", "Total.Night.Charge", "Total.Intl.Minutes", "Total.Intl.Calls", "Total.Intl.Charge", "Churn")], aes(color = Churn))

-----------------------------------------------------------------------------------------------------------
  
## Churn by state
plot1 <- data %>%
  ggplot(aes(State, fill = Churn)) +
  geom_bar(position = "fill") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

## Top 5 States with Highest Churn Rate
top_churn_states <- data %>%
  group_by(State) %>%
  summarise(Churned = sum(Churn == "True"), Total = n()) %>%
  mutate(ChurnRate = Churned / Total) %>%
  arrange(desc(ChurnRate)) %>%
  slice_head(n = 5) %>%
  mutate(State = factor(State, levels = rev(State)))  # Reorder states for correct sorting

plot2 <- ggplot(top_churn_states, aes(x = State, fill = ChurnRate)) +
  geom_bar() +
  scale_fill_gradient(low = "blue", high = "red") +  # Change colors here
  theme_minimal() +
  labs(title = "Top 5 States with Highest Churn Rate", x = "State", y = "Churn Rate") +
  coord_flip()  # Flip the coordinates to display bars horizontally

## Churn by international plan and voicemail plan
plot3 <- data %>%
  ggplot(aes(interaction(International.Plan, Voice.Mail.Plan), fill = Churn)) +
  geom_bar(position = "fill") +
  theme_minimal() +
  labs(x = "International.Plan:Voice.Mail.Plan")

## Number of customer service calls by churn
plot4 <- data %>%
  ggplot(aes(Number.Customer.Service.Calls, fill = Churn)) +
  geom_bar(position = "fill") +
  theme_minimal()

## Churn rate by area code
plot5 <- data %>%
  ggplot(aes(as.factor(Area.Code), fill = Churn)) +
  geom_bar(position = "fill") +
  theme_minimal() +
  labs(x = "Area Code")

## Churn rate by the number of international calls
plot6 <- data %>%
  ggplot(aes(Total.Intl.Calls, fill = Churn)) +
  geom_bar(position = "fill") +
  theme_minimal()

## Churn rate by account length
plot7 <- data %>%
  ggplot(aes(Account.Length, fill = Churn)) +
  geom_histogram(bins = 30, position = "fill") +
  theme_minimal()

## Arrange the plots in a grid
grid.arrange(plot1, plot2, plot3, ncol = 2)
grid.arrange( plot4, plot5, plot6, plot7, ncol=2)

-----------------------------------------------------------------------------------------------------------
  
  
####DATA CLEANING####


# Check for duplicates
if (any(duplicated(data))) {
  print("Duplicates found.")
} else {
  print("No duplicates found.")
}

# Check for NaN columns
nan_columns <- colnames(data)[colSums(is.na(data)) > 0]
if (length(nan_columns) > 0) {
  print("NaN values found in the following columns:")
  print(nan_columns)
} else {
  print("No NaN values found in any column.")
}

#checking for outliers
numeric_columns <- colnames(data)[sapply(data, is.numeric)]

for (col in numeric_columns) {
  Q1 <- quantile(data[[col]], 0.25)
  Q3 <- quantile(data[[col]], 0.75)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  outliers <- data[[col]] < lower_bound | data[[col]] > upper_bound
  #cat(col, ":", sum(outliers), "\n")
}

#plotting to see outliers
numeric_data <- data %>%
  select_if(is.numeric) %>%
  select(-c("Area.Code")) %>%
  gather(key = "variable", value = "value")

ggplot(numeric_data, aes(x = variable, y = value)) +
  geom_boxplot() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Boxplots of Numeric Variables")

# Cleaning outliers for Total.Intl.Calls only
Q1 <- quantile(data$Total.Intl.Calls, 0.25)
Q3 <- quantile(data$Total.Intl.Calls, 0.75)
IQR <- Q3 - Q1
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR
data$Total.Intl.Calls <- ifelse(data$Total.Intl.Calls < lower_bound, lower_bound, data$Total.Intl.Calls)
data$Total.Intl.Calls <- ifelse(data$Total.Intl.Calls > upper_bound, upper_bound, data$Total.Intl.Calls)

# Gathering and plotting cleaned numeric variables
cleaned_numeric_data <- data %>%
  select(one_of(numeric_columns)) %>%
  gather(key = "variable", value = "value")

ggplot(cleaned_numeric_data, aes(x = variable, y = value)) +
  geom_boxplot() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Boxplots of Cleaned Numeric Variables")

-----------------------------------------------------------------------------------------------------------
  

####MODEL IMPLEMENTATION####
#LASSO LOGISTIC REGRESSION WITH NUMBER OF CUSTOMER SERVICE CALLS#

# Split the data into training and testing sets
set.seed(3456)
index <- createDataPartition(data$Churn, p = 0.8, list = FALSE)
train_set <- data[index, ]
test_set <- data[-index, ]
# Create model matrix
x_train <- model.matrix(Churn ~ . - 1, data = train_set)
y_train <- train_set$Churn

x_test <- model.matrix(Churn ~ . - 1, data = test_set)
y_test <- test_set$Churn
# Set seed for reproducibility
set.seed(123)

# Fit the Lasso regression model
cv.lasso <- cv.glmnet(x_train, y_train, family = "binomial", alpha = 1, nfolds = 10)

# Use BIC to choose lambda
lambda.bic <- cv.lasso$lambda.1se
# Predict on the test set
predictions <- predict(cv.lasso, newx = x_test, type = "response", s = lambda.bic)
# Convert probabilities to class labels
predicted_labels <- ifelse(predictions > 0.5, "1", "0")

# Convert to factors
predicted_labels <- as.factor(predicted_labels)
y_test <- as.factor(y_test)

# Ensure they have the same levels
levels(predicted_labels) <- levels(y_test)

# Compute confusion matrix and accuracy
#confusionMatrix(predicted_labels, y_test)
# Compute AUC-ROC
roc_obj <- roc(y_test, as.numeric(predictions))
#auc(roc_obj)
# Accuracy
accuracy <- sum(predicted_labels == y_test) / nrow(test_set)


# Precision, Recall, and F1 Score
conf_mat <- confusionMatrix(predicted_labels, y_test)$table
precision <- conf_mat[2,2] / sum(conf_mat[2,])
recall <- conf_mat[2,2] / sum(conf_mat[,2])
f1_score <- 2 * ((precision*recall) / (precision + recall))

-----------------------------------------------------------------------------------------------------------
  
  
#LASSO LOGISTIC REGRESSION WITHOUT NUMBER OF CUSTOMER SERVICE CALLS#



# Split the data into training and testing sets
set.seed(3456)
# Create model matrix excluding "Number.Customer.Service.Calls"
x_train2 <- model.matrix(Churn ~ . - 1 - Number.Customer.Service.Calls, data = train_set)
y_train2 <- train_set$Churn

x_test2 <- model.matrix(Churn ~ . - 1 - Number.Customer.Service.Calls, data = test_set)
y_test2 <- test_set$Churn
# Set seed for reproducibility
set.seed(123)

# Fit the Lasso regression model
cv.lasso <- cv.glmnet(x_train2, y_train2, family = "binomial", alpha = 1, nfolds = 10)

# Use BIC to choose lambda
lambda.bic <- cv.lasso$lambda.1se
# Predict on the test set
predictions2 <- predict(cv.lasso, newx = x_test2, type = "response", s = lambda.bic)
# Convert probabilities to class labels
predicted_labels2 <- ifelse(predictions2 > 0.5, "1", "0")

# Convert to factors
predicted_labels2 <- as.factor(predicted_labels2)
y_test2 <- as.factor(y_test2)

# Ensure they have the same levels
levels(predicted_labels2) <- levels(y_test2)

# Compute confusion matrix and accuracy
#confusionMatrix(predicted_labels2, y_test2)
# Compute AUC-ROC
roc_obj2 <- roc(y_test2, as.numeric(predictions2))
#auc(roc_obj2)
# Accuracy
accuracy2 <- sum(predicted_labels2 == y_test2) / nrow(test_set)


# Precision, Recall, and F1 Score
conf_mat2 <- confusionMatrix(predicted_labels2, y_test2)$table
precision2 <- conf_mat2[2,2] / sum(conf_mat2[2,])
recall2 <- conf_mat2[2,2] / sum(conf_mat2[,2])
f1_score2 <- 2 * ((precision2*recall2) / (precision2 + recall2))



#df to store the model performance indicators
performance_df <- data.frame(Model = character(), RMSE = numeric(), R2 = numeric(), stringsAsFactors = FALSE)
performance_df <- rbind(performance_df, c("LLR without Customer Service Calls", precision2,auc(roc_obj2),f1_score2, recall2, accuracy2))
performance_df <- rbind(performance_df, c("LLR with Cutomer Service Calls", precision,auc(roc_obj),f1_score, recall, accuracy))
colnames(performance_df) <- c("Model", "Precision", "AUC","F1","Recall","Accuracy")
performance_df

-----------------------------------------------------------------------------------------------------------
  
  
#NON LINEAR MODEL#
#RANDOM FOREST WITH CROSS VALIDATION AND NUMBER OF CUSTOMER SERVICE CALLS#

set.seed(123)

# folds
k <- 10

#  ntree values to try
ntree_values <- seq(100, 1000, by = 50)

#  store accuracy results
accuracy_results <- vector("double", length(ntree_values))

# Perform k-fold cross validation
folds <- createFolds(train_set$Churn, k = k)

for (i in seq_along(ntree_values)) {
  accuracy <- vector("double", k)
  
  for (j in seq_len(k)) {
    # Define training and validation sets
    training_data <- train_set[-folds[[j]],]
    validation_data <- train_set[folds[[j]],]
    
    # Fit the model
    model <- randomForest(Churn ~ ., data = training_data, ntree = ntree_values[i])
    
    # Predict on the validation set
    predictions <- predict(model, newdata = validation_data)
    
    # Compute accuracy
    accuracy[j] <- sum(predictions == validation_data$Churn) / nrow(validation_data)
  }
  
  # Store mean accuracy across all folds
  accuracy_results[i] <- mean(accuracy)
}

# Print number of trees that resulted in the highest accuracy
best_ntree <- ntree_values[which.max(accuracy_results)]
print(best_ntree)


-----------------------------------------------------------------------------------------------------------

#PREDICTIONS RANDOM FOREST
#Using random forest for classification
# Train the Random Forest model
set.seed(123)
rf_model <- randomForest(Churn ~ ., data = train_set, importance = TRUE, ntree = 700)


# Predict on the test set
rf_predictions <- predict(rf_model, newdata = test_set)

# Convert to factors
rf_predicted_labels <- as.factor(rf_predictions)
levels(rf_predicted_labels) <- levels(test_set$Churn)

# Compute confusion matrix and print it
rf_confusionMatrix <- confusionMatrix(rf_predicted_labels, test_set$Churn)


# Compute Accuracy
rf_accuracy <- sum(rf_predicted_labels == test_set$Churn) / nrow(test_set)


# Precision, Recall, and F1 Score
rf_conf_mat <- rf_confusionMatrix$table
rf_precision <- rf_conf_mat[2,2] / sum(rf_conf_mat[2,])
rf_recall <- rf_conf_mat[2,2] / sum(rf_conf_mat[,2])
rf_f1_score <- 2 * ((rf_precision*rf_recall) / (rf_precision + rf_recall))


# Predict probabilities on the test 
rf_probabilities <- predict(rf_model, newdata = test_set, type = "prob")

# Compute AUC-ROC
roc_obj_rf <- roc(test_set$Churn, rf_probabilities[, "True"])
auc_rf <- auc(roc_obj_rf)

---------------------------------------------------------------------------------
  
#RANDOM FOREST WITHOUT NUMBER OF CUSTOMER SERVICE CALLS#

exclude_feature <- "Number.Customer.Service.Calls"
train_set2_filtered <- train_set[, !colnames(train_set) %in% exclude_feature]
test_set2_filtered <- test_set[, !colnames(test_set) %in% exclude_feature]



# Build the random forest model
rf_model2 <- randomForest(Churn ~ ., data = train_set2_filtered, importance = TRUE, ntree = 700)

# Predict on the test set
rf_predictions2 <- predict(rf_model2, newdata = test_set2_filtered)

# Convert to factors
rf_predicted_labels2 <- as.factor(rf_predictions2)
levels(rf_predicted_labels2) <- levels(test_set$Churn)

# Compute confusion matrix and print it
rf_confusionMatrix2 <- confusionMatrix(rf_predicted_labels2, test_set2_filtered$Churn)


# Compute Accuracy
rf_accuracy2 <- sum(rf_predicted_labels2 == test_set2_filtered$Churn) / nrow(test_set2_filtered)

# Precision, Recall, and F1 Score
rf_conf_mat2 <- rf_confusionMatrix2$table
rf_precision2 <- rf_conf_mat2[2,2] / sum(rf_conf_mat2[2,])
rf_recall2 <- rf_conf_mat2[2,2] / sum(rf_conf_mat2[,2])
rf_f1_score2 <- 2 * ((rf_precision2*rf_recall2) / (rf_precision2 + rf_recall2))


# Predict probabilities on the test 
rf_probabilities2 <- predict(rf_model2, newdata = test_set2_filtered, type = "prob")

# Compute AUC-ROC
roc_obj_rf2 <- roc(test_set2_filtered$Churn, rf_probabilities2[, "True"])
auc_rf2 <- auc(roc_obj_rf2)

#Model with the Number of Customer Service Calls
varImpPlot(rf_model)

#Model without the Number of Customer Service Calls
varImpPlot(rf_model2)

#Let us append the new model to the performance_df
performance_df <- rbind(performance_df, c("Random Forest with customer service calls", rf_precision,auc(roc_obj_rf),rf_f1_score,rf_recall,rf_precision ))
performance_df <- rbind(performance_df, c("Random Forest without customer service calls", rf_precision2,auc(roc_obj_rf2),rf_f1_score2,rf_recall2,rf_precision2 ))
performance_df

-----------------------------------------------------------------------------------------------------------
  
  
####CLUSTERING####
#K-MEANS#


# Select relevant columns
library(dplyr)
data_2 <- data %>% select(Account.Length, Number.Vmail.Messages, Total.Day.Minutes, Total.Day.Calls, Total.Day.Charge, Total.Eve.Minutes, Total.Eve.Calls, Total.Eve.Charge, Total.Night.Minutes, Total.Night.Calls, Total.Night.Charge, Total.Intl.Minutes, Total.Intl.Calls, Total.Intl.Charge, Number.Customer.Service.Calls)

#we now scale the numeric data
data_2 <- data_2 %>%
  mutate(across(where(is.numeric), scale))

#elbow method
k_values <- 1:10
withinss <- numeric(length(k_values))

for (i in seq_along(k_values)) {
  k <- k_values[i]
  kmeans_result <- kmeans(data_2, centers = k)
  withinss[i] <- kmeans_result$tot.withinss
}

elb <- plot(k_values, withinss, type = "b", pch = 19, frame = FALSE,
            xlab = "Number of Clusters (k)", ylab = "Within-cluster Sum of Squares")


#silhouette score by k
silk <- fviz_nbclust(data_2, kmeans, method='silhouette')
grid.arrange(elb, silk, ncol = 1)

# Run k-means clustering for k=2 to k=6
# K-means clustering with different values of k (data without the 5 outliers)
kmeans_plots <- lapply(2:6, function(k) {
  set.seed(123)
  kmeans_model <- kmeans(data_2, centers = k, nstart = 25)
  fviz_cluster(kmeans_model, data = scale(data_2), geom = "point", 
               main = paste("K-Means Clustering(k =", k, ")"))
})

# Arrange plots in a grid
plot_grid(plotlist = kmeans_plots, align = "h", axis = "tb")


-----------------------------------------------------------------------------------------------------------
  
  
#HIERARCHICAL CLUSTERING#


# Compute hierarchical clustering with different dissimilarity measures
data_2_scaled  <- data_2

#euclidean
dist_euc <- dist(data_2_scaled, method = "euclidean")
hc_euclidean <- hclust(dist_euc, method = "ward.D2")

#1- row correlation
dist_cor <- as.dist(1 - cor(t(data_2_scaled)))
hc_correlation <- hclust(dist_cor, method = "ward.D2")


# Plot dendograms
plot(hc_euclidean, cex = 0.6, main = "Dendrogram (Euclidean distance)")
plot(hc_correlation, cex = 0.6, main = "Dendrogram (1 - row correlation)")

# Initialize vectors for storing silhouette scores
silhouette_euclidean <- numeric(5)
silhouette_correlation <- numeric(5)

# Iterate over different values of k
for (k in 2:6) {
  
  # Compute cluster assignments for euclidean distance
  hc_euclidean_k <- cutree(hc_euclidean, k = k)
  
  # Compute silhouette score for euclidean distance
  silhouette_euclidean[k - 1] <- mean(silhouette(hc_euclidean_k, dist_euc)[, "sil_width"])
  
  # Compute cluster assignments for 1 - row correlation
  hc_correlation_k <- cutree(hc_correlation, k = k)
  
  # Compute silhouette score for 1 - row correlation
  silhouette_correlation[k - 1] <- mean(silhouette(hc_correlation_k, dist_cor)[, "sil_width"])
  
}


cat("Silhouette scores for euclidean distance:", silhouette_euclidean, "\n")
cat("Silhouette scores for 1 - row correlation:", silhouette_correlation, "\n")

plot_list3 <- list()  # initialize list for euclidean distance plots
plot_list4 <- list()  # initialize list for 1 - row correlation plots

# Iterate over different values of k
for (k in 2:3) {
  
  # Compute cluster assignments for euclidean distance
  hc_euclidean_k <- cutree(hc_euclidean, k = k)
  
  # Plot clusters for euclidean distance
  plot_title <- paste0("Clusters (Euclidean distance) for k =", k)
  plot_title <- gsub(" ", "_", plot_title)
  plot_list3[[k-1]] <- fviz_cluster(list(data = data_2_scaled, cluster = hc_euclidean_k), 
                                    geom = "point", 
                                    palette = "jco", 
                                    main = plot_title) + theme_bw()
  
  # Compute cluster assignments for 1 - row correlation
  hc_correlation_k <- cutree(hc_correlation, k = k)
  
  # Plot clusters for 1 - row correlation
  plot_title <- paste0("Clusters (1 - row correlation) for k =", k)
  plot_title <- gsub(" ", "_", plot_title)
  plot_list4[[k-1]] <- fviz_cluster(list(data = data_2_scaled, cluster = hc_correlation_k), 
                                    geom = "point", 
                                    palette = "jco", 
                                    main = plot_title) + theme_bw()
}

# Combine plots into grid of subplots
grid.arrange(grobs = plot_list3, nrow = 3, ncol = 2, top = "Clusters (Euclidean distance)") 
grid.arrange(grobs = plot_list4, nrow = 3, ncol = 2, top = "Clusters (1 - row correlation)")

data_new <- cleaned_numeric_data
kmeans_results <- kmeans(data_2,centers=2, nstart=25)
data$clusters2 <- kmeans_results$cluster
-----------------------------------------------------------------------------------------------------------
  

#CLUSTER CHURN RATES

# Run k-means clustering with k=2
kmeans_model <- kmeans(data_2, centers = 2, nstart = 25)

# Retrieve cluster assignments
cluster_assignments <- kmeans_model$cluster

# Create a new data frame with cluster assignments and Churn variable
cluster_churn <- data.frame(cluster = cluster_assignments, Churn = data$Churn)

# Calculate churn rate for each cluster
churn_rates <- cluster_churn %>%
  group_by(cluster) %>%
  summarize(churn_rate = mean(Churn == "True"))

# Print the churn rate
print(churn_rates)
-----------------------------------------------------------------------------------------------------------
  
  

####DO CLUSTERS REGROUP WELL?####

# Create the boxplot for  with regrouped clusters2
bar1 <- ggplot(data, aes(x = interaction(clusters2, data$International.Plan))) +
  geom_bar(fill = "lightgray", color = "black") +
  labs(x = "clusters2", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("International Plan")

# By account length
bar2 <- ggplot(data, aes(x = interaction(clusters2, data$clusters2), y = data$Account.Length)) +
  stat_summary(fun = "mean", geom = "bar", position = "dodge") +
  labs(x = "clusters2", y = "Average Value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("AVG Account Length")

# Average value by clusters2 - Total Day Minutes
bar3 <- ggplot(data, aes(x = interaction(clusters2, data$clusters2), y = data$Total.Day.Minutes)) +
  stat_summary(fun = "mean", geom = "bar", position = "dodge") +
  labs(x = "clusters2", y = "Average Value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("AVG. Total Day Minutes")

# Average value by clusters2 - Total Day Charge
bar4 <- ggplot(data, aes(x = interaction(clusters2, data$clusters2), y = data$Total.Night.Minutes)) + 
  stat_summary(fun = "mean", geom = "bar", position = "dodge") +
  labs(x = "clusters2", y = "Average Value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("AVG Total Night Minutes")

bar5 <- ggplot(data, aes(x = interaction(clusters2, data$clusters2), y = data$Total.Eve.Minutes)) + 
  stat_summary(fun = "mean", geom = "bar", position = "dodge") +
  labs(x = "clusters2", y = "Average Value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("Avg. Total Eve Minutes")


grid.arrange(bar1, bar2, bar3, bar4, bar5, ncol = 3)

-----------------------------------------------------------------------------------------------------------
  

## ANALYSIS OF PRINCIPAL COMPONENTS ##
pca2 <- prcomp(data_2)

#loadings(weights)
loadings <- data.frame(variable = colnames(data_2),  PC2 = pca2$rotation[, 2])
print(loadings)

night_cost <- ifelse(data$Total.Night.Minutes !=0, data$Total.Night.Minutes/data$Total.Night.Charge,0) #cost of 22 on average
eve_cost <- ifelse(data$Total.Eve.Charge != 0, data$Total.Eve.Minutes / data$Total.Eve.Charge, 0)#cost of 11 on average
day_cost <- ifelse(data$Total.Day.Charge != 0, data$Total.Day.Minutes / data$Total.Day.Charge, 0) #cost of 5 on average

cat(mean(night_cost), "for Night calls", "\n")
cat(mean(eve_cost), "for Evening calls", "\n")
cat(mean(day_cost), "for Day calls", "\n")


#Churn rate for different features
churn_rate_account_lenght <- mean(data$Churn[data$Account.Length] == "True")
churn_rate_totaldayminutes <- mean(data$Churn[data$Total.Day.Minutes] == "True")
churn_rate_totaleveminutes <- mean(data$Churn[data$Total.Eve.Minutes] == "True")
churn_rate_totalnightminutes <- mean(data$Churn[data$Total.Night.Minutes] == "True")
churn_rate_internationalplan <- mean(data$Churn[data$International.Plan] == "True")

#Print churn rates
cat(churn_rate_totaldayminutes, "Churn rate for Day calls", "\n")
cat(churn_rate_totaleveminutes, "Churn rate for Evening calls", "\n")
cat(churn_rate_totalnightminutes, "Churn rate for Night calls", "\n")


  
  