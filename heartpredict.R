Sys.getlocale()
Sys.setlocale(locale = "tr_TR.UTF-8")

#reading data
data<-read.csv("C:\\Users\\dilar\\OneDrive\\MasaC<stC<\\R Programming\\Proje1\\hearth.csv",encoding = "UTF-8")
# first six data
head(data)

str(data)

# dataframe control
is.data.frame(data)

# View basic statistical information
summary(data)

#correlation matrix

install.packages("corrplot")
library(corrplot)

cor_matrix <- cor(data[, c("age", "trestbps", "chol", "thalach", "oldpeak")])
print(cor_matrix)

# Correlation matrix visualization
corrplot(cor_matrix, method = "color")



# data visualization ->--- do data visualization with the help of tables to understand the data.
#Compare the target variable with other data.

# Comparison of numerical data and categorical data will be used to compare age with target.

#BOX PLOT
boxplot(data$age ~ data$target, col = c("lightblue", "lightgreen"),
        main = "Heart disease by age")

# BAR PLOT
barplot(tapply(data$age, data$target, mean), col = c("lightblue", "lightgreen", "lightpink", "lightyellow"),
        ylim = c(0, max(data$age)), 
        main = "heart disease by age",
        xlab = "target", ylab = "age")

#It is a little difficult to make a comment based on age based on these two graphs.


# Distribution of age characteristics

install.packages("ggplot2")
library(ggplot2)
ggplot(data, aes(x = age)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Age Distribution", x = "Age", y = "Frequency")


# Comparison between categorical variable "cp" and numerical variable "trestbps"
plot(density(data$age[data$target == 1]), col = "blue", main = "Age range chart of patients",
     xlab = "Age", ylab = "Intensity", ylim = c(0, 0.025))
lines(density(data$age[data$target == 0]), col = "red")
lines(density(data$age[data$target == 1]), col = "green")

legend("topright", legend = c("0", "1", "2"), fill = c("blue", "red", "green"))

# Comparison of two categorical data, cp and target

# BAR PLOT
cross_tab <- table(data$cp, data$target)


barplot(cross_tab, beside = TRUE, legend = TRUE,
        col = c("lightblue", "lightgreen", "lightpink", "lightyellow"),
        main = "Heart Disease Distribution by Chest Pain Type",
        xlab = "Chest Pain Type", ylab = "Frekans")

#As can be seen from the table, those with CP 0 are not sick.

#Mosaic Plot
install.packages("vcd")
library(vcd)

mosaicplot(table(data$cp, data$target), color = c("lightblue", "lightgreen", "lightpink", "lightyellow"),
           main = "Heart Disease Distribution by Chest Pain Type",
           shade = TRUE)


# target comparison by gender

#Bar graph of two categorical variables

table_plot <- table(data$target, data$sex)
barplot(table_plot, beside = TRUE, legend = TRUE,
        col = c("lightblue", "lightgreen"), 
        main = "gender distribution according to disease status",
        xlab = "target", ylab = "Frekans")
#to see more detail

# Mosaic Plot
install.packages("vcd")
library(vcd)

mosaicplot(table(data$target, data$sex), color = c("lightblue", "lightgreen"),
           main = "gender distribution according to disease status",
           shade = TRUE)




# trestbps and target 

# Comparison between categorical variable "cp" and numerical variable "trestbps"
boxplot(data$trestbps ~ as.factor(data$target), col = c("lightblue", "lightgreen", "lightpink", "lightyellow"),
        main = "blood pressure according to disease",
        xlab = "target", ylab = "Resting Blood Pressure (mm Hg)")

# BAR PLOT
cross_tab <- table(data$trestbps, data$target)

barplot(cross_tab, beside = TRUE, legend = TRUE,
        col = c("lightblue", "lightgreen"),
        main = "Heart Disease Distribution by Resting Blood Pressure",
        xlab = "Resting Blood Pressure", ylab = "Frekans")

# DENSITY GRAPH
plot(density(data$chol[data$target == 0]), col = "blue", main = "Serum Cholesterol Density Chart Without Heart Disease",
     xlab = "Serum Cholesterol Level", ylab = "density", ylim = c(0, 0.04))
lines(density(data$chol[data$target == 1]), col = "red")
legend("topright", legend = c("0 - there is no", "1 - there is"), fill = c("blue", "red"))


#target According to fbs
# BAR PLOT
barplot(table(data$fbs, data$target), beside = TRUE, legend = TRUE,
        col = c("lightblue", "lightgreen"),
        main = "Heart Disease Distribution According to Fasting Blood Sugar",
        xlab = "A??l??k Kan ??ekeri (fbs)", ylab = "Frekans")



#Electrocardiographic Results According to the Presence of Heart Disease
# restecg and target
cross_tab <- table(data$restecg, data$target)
barplot(cross_tab, beside = TRUE, legend = TRUE,
        col = c("lightblue", "lightgreen", "lightpink"),
        main = "Electrocardiographic Results According to the Presence of Heart Disease",
        xlab = "restecg", ylab = "Frekans")



#thalach and target
# BOX PLOT
boxplot(thalach ~ target, data = data, col = c("lightblue", "lightgreen"),
        main = "Maximum Heart Rate According to the Presence of Heart Disease",
        xlab = "Presence of Heart Disease", ylab = "Maximum Heart Rate")

# bar chart
barplot(table(data$thalach, data$target), beside = TRUE, legend = TRUE,
        col = c("lightblue", "lightgreen"),
        main = "Heart Disease Distribution According to Maximum Heart Rate",
        xlab = "Maximum Heart Rate (thalach)", ylab = "Frekans")

# Installing the ggplot2 library
install.packages("ggplot2")
library(ggplot2)

# ggplot showing the presence of heart disease according to maximum heart rate
ggplot(data, aes(x = factor(target), y = thalach, fill = factor(target))) +
  geom_boxplot() +
  labs(title = "Maximum Heart Rate According to the Presence of Heart Disease",
       x = "Presence of Heart Disease", y = "Maximum Heart Rate") +
  scale_fill_manual(values = c("lightblue", "lightgreen"))


#----------- Change null values to random.

set.seed(123)
data_with_missing<-data
#  5 data in trestbps field were made null
data_with_missing[sample(1:nrow(data),5),"trestbps"]<-NA

# 10 data in thalach field were made null
data_with_missing[sample(1:nrow(data),10),"thalach"]<-NA

# should give the number of missing data as 15
print(sum(is.na(data_with_missing)))


#Write on the screen how many missing data are in which columns.
count_missing_data <- colSums(is.na(data_with_missing))

print(count_missing_data)


#replace null values with average values


for (col in names(data_with_missing)) {
  if (anyNA(data_with_missing[[col]])) {
    mean_val <- mean(data_with_missing[[col]], na.rm = TRUE)
    data_with_missing[[col]][is.na(data_with_missing[[col]])] <- mean_val
  }
}
colSums(is.na(data_with_missing))
#----
#ADD NEW FEATURE -> Cardiovascular Risk Score calculation function
# Cardiovascular Risk Score calculation function
calculate_cardiovascular_risk <- function(data_with_missing) {
  risk_score <- numeric(nrow(data_with_missing))
  
  # weight for age
  risk_score <- risk_score + 0.5 * data_with_missing$age
  
  # Weight by gender
  risk_score <- ifelse(data_with_missing$sex == "1", risk_score + 5, risk_score)
  
  # Weight relative to resting blood pressure
  risk_score <- risk_score + 0.1 * data_with_missing$trestbps
  
  #Weight according to cholesterol level
  risk_score <- risk_score + 0.2 * data_with_missing$chol
  
  return(risk_score)
}
# Calculate Cardiovascular Risk Score
data_with_missing$cardiovascular_risk_score <- calculate_cardiovascular_risk(data_with_missing)

# Show results
head(data_with_missing)


# swap columns.
#-----
library(data.table)

data_with_missing <- as.data.table(data_with_missing)
data_with_missing[, c("target", "cardiovascular_risk_score") := .(cardiovascular_risk_score,target)]



# Changing column names
setnames(data_with_missing, old = "target", new = "cardiovascular_riskscore")

# Changing column names
setnames(data_with_missing, old = "cardiovascular_risk_score", new = "target")


# Show data

print(head(data_with_missing))

#--- clear duplicate data

#duplicate data check
duplicates <-data_with_missing[duplicated(data_with_missing) | duplicated(data_with_missing, fromLast = TRUE), ]

# Write duplicate data to the screen
print(duplicates)

#Remove duplicate data
clean_data_set = data_with_missing[!duplicated(data_with_missing), ]

# Print cleaned data set to screen
head(clean_data_set)

# RANDOM ORDER DATA
data<- clean_data_set[sample(nrow(clean_data_set)),]
head(data)





#------------ replacement of contradictory data

# Finding outliers with the IQR Method
find_outliers_iqr <- function(x) {
  q <- quantile(x, c(0.25, 0.75))
  iqr <- q[2] - q[1]
  lower_bound <- q[1] - 1.5 * iqr
  upper_bound <- q[2] + 1.5 * iqr
  return(x[x < lower_bound | x > upper_bound])
}
# Find outliers in each column
outliers_iqr <- lapply(data, find_outliers_iqr)
print("Outliers (IQR Method):")
print(outliers_iqr)


#----------------

#trestbps
# Replace outliers with maximum non-outlier value in trestbps column
replace_outliers_trestbps <- function(x) {
  q <- quantile(x, c(0.25, 0.75))
  iqr <- q[2] - q[1]
  lower_bound <- q[1] - 1.5 * iqr
  upper_bound <- q[2] + 1.5 * iqr
  
  # Finding outliers
  outliers <- x[x < lower_bound | x > upper_bound]
  
  # Find the maximum non-outlier value
  max_non_outlier <- max(x[!(x %in% outliers)], na.rm = TRUE)
  
  # Replace outliers with the maximum non-outlier value of the column
  x[x %in% outliers] <- max_non_outlier
  
  return(x)
}

# Find outliers and replace them with max value
data$trestbps <- replace_outliers_trestbps(data$trestbps)

# CHOL
# Replace outliers with maximum non-outlier value in column chol
replace_outliers_chol <- function(x) {
  q <- quantile(x, c(0.25, 0.75))
  iqr <- q[2] - q[1]
  lower_bound <- q[1] - 1.5 * iqr
  upper_bound <- q[2] + 1.5 * iqr
  
  # Finding outliers
  outliers <- x[x < lower_bound | x > upper_bound]
  
  # Find the maximum non-outlier value
  max_non_outlier <- max(x[!(x %in% outliers)], na.rm = TRUE)
  
  # Replace outliers with the maximum non-outlier value of the column
  x[x %in% outliers] <- max_non_outlier
  
  return(x)
}

# Find outliers and replace them with max value
data$chol <- replace_outliers_chol(data$chol)



# FBS
# Replace outliers with maximum non-outlier value in fbs column
replace_outliers_fbs <- function(x) {
  q <- quantile(x, c(0.25, 0.75))
  iqr <- q[2] - q[1]
  lower_bound <- q[1] - 1.5 * iqr
  upper_bound <- q[2] + 1.5 * iqr
  
  # Finding outliers
  outliers <- x[x < lower_bound | x > upper_bound]
  
  # Find the maximum non-outlier value
  max_non_outlier <- max(x[!(x %in% outliers)], na.rm = TRUE)
  
  # Replace outliers with the maximum non-outlier value of the column
  x[x %in% outliers] <- max_non_outlier
  
  return(x)
}

# Find outliers and replace them with max value
data$fbs <- replace_outliers_fbs(data$fbs)


# THALACH

# Replace outliers with maximum non-outlier value in thalach column
replace_outliers_thalach <- function(x) {
  q <- quantile(x, c(0.25, 0.75))
  iqr <- q[2] - q[1]
  lower_bound <- q[1] - 1.5 * iqr
  upper_bound <- q[2] + 1.5 * iqr
  
  # Finding outliers
  outliers <- x[x < lower_bound | x > upper_bound]
  
  # Find the maximum non-outlier value
  max_non_outlier <- max(x[!(x %in% outliers)], na.rm = TRUE)
  
  # Replace outliers with the maximum non-outlier value of the column
  x[x %in% outliers] <- max_non_outlier
  
  return(x)
}

# Find outliers and replace them with max value
data$thalach <- replace_outliers_thalach(data$thalach)



#OLDPEAK 

# Replace outliers with maximum non-outlier value in oldpeak column
replace_outliers_oldpeak <- function(x) {
  q <- quantile(x, c(0.25, 0.75))
  iqr <- q[2] - q[1]
  lower_bound <- q[1] - 1.5 * iqr
  upper_bound <- q[2] + 1.5 * iqr
  
  # Finding outliers
  outliers <- x[x < lower_bound | x > upper_bound]
  
  # Find the maximum non-outlier value
  max_non_outlier <- max(x[!(x %in% outliers)], na.rm = TRUE)
  
  # Replace outliers with the maximum non-outlier value of the column
  x[x %in% outliers] <- max_non_outlier
  
  return(x)
}

# Find outliers and replace them with max value
data$oldpeak <- replace_outliers_oldpeak(data$oldpeak)




#CA
# replace outliers with non-outlier maximum values in ca column
replace_outliers_ca <- function(x) {
  q <- quantile(x, c(0.25, 0.75))
  iqr <- q[2] - q[1]
  lower_bound <- q[1] - 1.5 * iqr
  upper_bound <- q[2] + 1.5 * iqr
  
  # Finding outliers
  outliers <- x[x < lower_bound | x > upper_bound]
  
  # Find the maximum non-outlier value
  max_non_outlier <- max(x[!(x %in% outliers)], na.rm = TRUE)
  
  # Replace outliers with the maximum non-outlier value of the column
  x[x %in% outliers] <- max_non_outlier
  
  return(x)
}

# Find outliers and replace them with max value
data$ca <- replace_outliers_ca(data$ca)


# THAL
# replace outliers with non-outlier maximum values in thal column
replace_outliers_thal <- function(x) {
  q <- quantile(x, c(0.25, 0.75))
  iqr <- q[2] - q[1]
  lower_bound <- q[1] - 1.5 * iqr
  upper_bound <- q[2] + 1.5 * iqr
  
  # Finding outliers
  outliers <- x[x < lower_bound | x > upper_bound]
  
  # Find the maximum non-outlier value
  max_non_outlier <- max(x[!(x %in% outliers)], na.rm = TRUE)
  
  # Replace outliers with the maximum non-outlier value of the column
  x[x %in% outliers] <- max_non_outlier
  
  return(x)
}
# Find outliers and replace them with max value
data$thal <- replace_outliers_thal(data$thal)


#cardiovascular_risk_score
# replace outliers with maximum non-outlier values in the cardiovascular risk score column
replace_outliers_cardiovascular_riskscore <- function(x) {
  q <- quantile(x, c(0.25, 0.75))
  iqr <- q[2] - q[1]
  lower_bound <- q[1] - 1.5 * iqr
  upper_bound <- q[2] + 1.5 * iqr
  
  # Finding outliers
  outliers <- x[x < lower_bound | x > upper_bound]
  
  # Find the maximum non-outlier value
  max_non_outlier <- max(x[!(x %in% outliers)], na.rm = TRUE)
  
  # Replace outliers with the maximum non-outlier value of the column
  x[x %in% outliers] <- max_non_outlier
  
  return(x)
}

# Find outliers and replace them with max value
data$cardiovascular_riskscore <- replace_outliers_cardiovascular_riskscore(data$cardiovascular_riskscore)

print(data)

#-- control 
#------------ replacement of contradictory data
# Finding outliers with the IQR Method
find_outliers_iqr <- function(x) {
  q <- quantile(x, c(0.25, 0.75))
  iqr <- q[2] - q[1]
  lower_bound <- q[1] - 1.5 * iqr
  upper_bound <- q[2] + 1.5 * iqr
  return(x[x < lower_bound | x > upper_bound])
}
# Find outliers in each column
outliers_iqr <- lapply(data, find_outliers_iqr)
print("Outliers (IQR Method):")
print(outliers_iqr)

# -----final state of non-normalized 
head(data)

normalized_df<-data
head(normalized_df)

#----------------VAL, TRAIN AND TEST DIFFERENCE OF "NON-NORMALIZED" DATA.

library(caret)
# Split the data into 70% test, 15% train and 15% validation
set.seed(42)

# Let's use the createDataPartition function twice to make the total p equal to 1
index_train_val <- createDataPartition(data$target, p = 0.7, list = FALSE)
index_train <- index_train_val
index_validation <- createDataPartition(data$target[-index_train_val], p = 0.5, list = FALSE)

# Create test indexes
index_test <- setdiff(seq_len(nrow(data)), c(index_train, index_validation))

# Generate train, validation and test data
train_data <- data[index_train, ]
validation_data <- data[index_validation, ]
test_data <- data[index_test, ]


#results
print("Train Data:")
print(head(train_data))

print("Validation Data:")
print(head(validation_data))

print("Test Data:")
print(head(test_data))



# ----CLASSIFY DATA

# Converting the "target" variable in the data set into a factor
train_data$target <- as.factor(train_data$target)
validation_data$target <- as.factor(validation_data$target)
test_data$target <- as.factor(test_data$target)


# Checking factor levels in the training dataset
levels(train_data$target)

#Equalizing factor levels across different data sets
validation_data$target <- factor(validation_data$target, levels = levels(train_data$target))
test_data$target <- factor(test_data$target, levels = levels(train_data$target))

# ---- DECISION TREE

# ---- DECISION TREE WITH NON-NORMALIZED DATA

install.packages("rpart")
library(rpart)

# Creating the decision tree model
decision_tree_model <- rpart(target ~ ., data = train_data, method = "class")

# Visualize the created model
plot(decision_tree_model)
text(decision_tree_model, pretty = 0)


# Classification on train data
predictions_train <- predict(decision_tree_model, newdata = train_data, type = "class")

# Classification on test data
predictions_test <- predict(decision_tree_model, newdata = test_data, type = "class")

conf_matrix_train <- confusionMatrix(predictions_train, train_data$target)
conf_matrix_test <- confusionMatrix(predictions_test, test_data$target)

# Print the results
print("Train Data Results:")
print(conf_matrix_train)

print("Test Data Results:")
print(conf_matrix_test)

library(caret)

# Sensitivity, Specificity, F1-score and Accuracy calculation function
calculate_metrics <- function(conf_matrix) {
  sensitivity <- conf_matrix[["byClass"]]["Sensitivity"]
  specificity <- conf_matrix[["byClass"]]["Specificity"]
  f1_score <- conf_matrix[["byClass"]]["F1"]
  accuracy <- conf_matrix[["overall"]]["Accuracy"]
  
  return(c(Sensitivity = sensitivity, Specificity = specificity, F1_Score = f1_score, Accuracy = accuracy))
}

# Calculate metrics
metrics_train <- calculate_metrics(conf_matrix_train)
metrics_test <- calculate_metrics(conf_matrix_test)

# Print results
cat("Train Data Results:\n")
print(conf_matrix_train)
print(metrics_train)

cat("\nTest Data Results:\n")
print(conf_matrix_test)
print(metrics_test)

#Visualize the classifier results.
install.packages("rpart.plot")

library(rpart.plot)

# Visualize the decision tree in a more meaningful wayprp(decision_tree_model, extra = 1, main = "Decision Tree Visualization")

#----  Desicion tree non-normalize  0.8773585 

#---------- KNN 

# NON-Normalize -> KNN 
install.packages(c("class", "caret"))
library(class)
library(caret)

k_values <- seq(1,29,by=2)
accuracy_values<- numeric(length (k_values))

for (k in k_values){
  predictions <- knn(train=train_data[,1:14],
                     test=test_data[,1:14],
                     cl=train_data$target,
                     k=k)
  accuracy_values[which(k_values==k)] <-
    sum(predictions==test_data$target)/ length(test_data$target)
}
accuracy_df <- data.frame(k=k_values,
                          accuracy=accuracy_values)

print (accuracy_df)

ggplot(accuracy_df,aes(x=k,y=accuracy))+
  geom_line()+
  geom_point()+
  labs(title = "knn model accuracy",
       x="k values",
       y="accuracy")

best_k <- k_values[which.max(accuracy_values)]
print(best_k)

final_model <- knn(train=train_data[,1:14],
                   test=test_data[,1:14],
                   cl=train_data$target,
                   k=best_k)

# we will find the error matrix and accuracy.
# error matrix is found with ->Table. 
confusion_matrix <- table (final_model, test_data$target)
print(confusion_matrix)

# Sensitivity, Specificity, F1-score and Accuracy calculation function
calculate_metrics <- function(predictions, true_labels) {
  conf_matrix <- confusionMatrix(predictions, true_labels)
  sensitivity <- conf_matrix[["byClass"]]["Sensitivity"]
  specificity <- conf_matrix[["byClass"]]["Specificity"]
  f1_score <- conf_matrix[["byClass"]]["F1"]
  accuracy <- conf_matrix[["overall"]]["Accuracy"]
  
  return(c(Sensitivity = sensitivity, Specificity = specificity, F1_Score = f1_score, Accuracy = accuracy))
}

# Find the best k value
best_k <- k_values[which.max(accuracy_values)]

# Classification on train data
predictions_train <- knn(train = train_data[, 1:14],
                         test = train_data[, 1:14],
                         cl = train_data$target,
                         k = best_k)

# Classification on test data
predictions_test <- knn(train = train_data[, 1:14],
                        test = test_data[, 1:14],
                        cl = train_data$target,
                        k = best_k)

# Calculate Sensitivity, Specificity, F1-score and Accuracy
metrics_train <- calculate_metrics(predictions_train, train_data$target)
metrics_test <- calculate_metrics(predictions_test, test_data$target)

# Print results
print("Train Data Results:")
print(metrics_train)

print("\nTest Data Results:")
print(metrics_test)
# NON-Normalize -> KNN accuracy 0.7594

#------- 


#  RANDOM FOREST-> NON-NORMALIZED
install.packages("randomForest")
library(randomForest)
library(caret)
train_data$target <- as.factor(train_data$target)
test_data$target <- as.factor(test_data$target)

# Set parameters
ntree_values <- c(50, 100, 150)
mtry_values <- c(3, 5, 7)

# Evaluating parameter combinations with grid search
results <- list()

for (ntree in ntree_values) {
  for (mtry in mtry_values) {
    model_rf <- randomForest(target ~ ., data = train_data, ntree = ntree, mtry = mtry)
    predictions <- predict(model_rf, newdata = validation_data)
    accuracy <- sum(predictions == validation_data$target) / length(validation_data$target)
    results[[paste("ntree", ntree, "mtry", mtry)]] <- accuracy
  }
}

# Show results
print(results)

# Determine best parameter values
ntree_best <- 150
mtry_best <- 7

# Train the final model
final_model_rf <- randomForest(target ~ ., data = train_data, ntree = ntree_best, mtry = mtry_best)

# Evaluate the model
predictions <- predict(final_model_rf, newdata = train_data)

# Confusion matrix and accuracy
confusion_matrix <- table(predictions, train_data$target)
print(confusion_matrix)

accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
print(paste("Accuracy: ", accuracy * 100))

# Sensitivity 
sensitivity <- confusion_matrix[2, 2] / sum(confusion_matrix[2, ])
print(paste("Sensitivity: ", sensitivity))

# Specificity  
specificity <- confusion_matrix[1, 1] / sum(confusion_matrix[1, ])
print(paste("Specificity: ", specificity))

# Precision  
precision <- confusion_matrix[2, 2] / sum(confusion_matrix[, 2])
print(paste("Precision: ", precision))

# Recall  
recall <- sensitivity
print(paste("Recall: ", recall))

# F1-Score
f1_score <- 2 * (precision * recall) / (precision + recall)
print(paste("F1-Score: ", f1_score))
# RANDOM FOREST-> NON-NORMALIZED Accuracy:  79.7297297297297


#SVM-> NON-NORMALIZE 
install.packages("e1071")
library(e1071)
# Train the SVM model
svm_model <- svm(target ~ ., data = train_data, kernel = "linear")

# Classification on validation data
predictions_train <- predict(svm_model, newdata = train_data)

# Classification on test data
predictions_test <- predict(svm_model, newdata = test_data)

conf_matrix_train <- confusionMatrix(predictions_train, train_data$target)
conf_matrix_test <- confusionMatrix(predictions_test, test_data$target)

# Print the results
print("Train Data Results:")
print(conf_matrix_train)

print("Test Data Results:")
print(conf_matrix_test)

conf_matrix_train <- confusionMatrix(predictions_train, train_data$target)

# (Accuracy) 
accuracy_train <- conf_matrix_train$overall["Accuracy"]
print(paste("Train Verisi Accuracy:", accuracy_train))

# Sensitivity 
sensitivity <- confusion_matrix[2, 2] / sum(confusion_matrix[2, ])
print(paste("Sensitivity: ", sensitivity))

# Specificity 
specificity <- confusion_matrix[1, 1] / sum(confusion_matrix[1, ])
print(paste("Specificity: ", specificity))

# Precision
precision <- confusion_matrix[2, 2] / sum(confusion_matrix[, 2])
print(paste("Precision: ", precision))

# Recall 
recall <- sensitivity
print(paste("Recall: ", recall))

# F1-Score
f1_score <- 2 * (precision * recall) / (precision + recall)
print(paste("F1-Score: ", f1_score))

# Confusion matrix
conf_matrix_test <- confusionMatrix(predictions_test, test_data$target)$table

# Visualization
heatmap(conf_matrix_test, 
        col = c("white", "blue"), 
        main = "Confusion Matrix - Test Data")

#----SVM-> NON-NORMALIZE Train Data Accuracy: 0.863207547169811


# LOGISTIC REGRESSION->NON-NORMALIZE 
library(caret)

# Train logistic regression model
logistic_model <- glm(target ~ ., data = train_data, family = binomial)

# View model summary
#summary(logistic_model)

# make predictions on the train set
predictions <- predict(logistic_model, newdata = train_data, type = "response")
predicted_classes <- ifelse(predictions > 0.5, 1, 0)

# Confusion matrix and accuracy
confusion_matrix <- table(predicted_classes, train_data$target)
print(confusion_matrix)

accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
print(paste("Accuracy: ", accuracy ))

# Sensitivity 
sensitivity <- confusion_matrix[2, 2] / sum(confusion_matrix[2, ])
print(paste("Sensitivity: ", sensitivity))

# Specificity 
specificity <- confusion_matrix[1, 1] / sum(confusion_matrix[1, ])
print(paste("Specificity: ", specificity))

# Precision 
precision <- confusion_matrix[2, 2] / sum(confusion_matrix[, 2])
print(paste("Precision: ", precision))

# Recall 
recall <- sensitivity
print(paste("Recall: ", recall))

# F1-Score
f1_score <- 2 * (precision * recall) / (precision + recall)
print(paste("F1-Score: ", f1_score))


metrics_df <- data.frame(Metric = c("Accuracy", "Sensitivity", "Specificity", "Precision", "Recall", "F1-Score"),
                         Value = c(accuracy, sensitivity, specificity, precision, recall, f1_score))

# Creating a line chart using ggplot2
library(ggplot2)

ggplot(metrics_df, aes(x = Metric, y = Value)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Logistic Regression Model Metrics",
       y = "Value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# ----- LOGISTIC REGRESSION->NON-NORMALIZE Accuracy:  0.867924528301887



# --- NORMALIZED DATA ---------


#----- NORMALIZATION ----
#Min-Max normalization 

# Function for Min-Max normalization
normalize<-function(x){
  a<-x
  if (max(a)-min(a)==0){
    a<-x
  }else{
    a<-(x-min(x))/ (max(x)-min(x))
    
  } 
  return(a)
}
normalized_df[,1:14]<-lapply(normalized_df[,1:14],normalize)
head(normalized_df)


#---- VAL, TRAIN AND TEST DIFFERENCE OF "NORMALIZED" DATA.

install.packages("caret")
library(caret)

# Split the data into 70% test, 15% train and 15% validationset.seed(42)

index_train_val_normalize <- createDataPartition(normalized_df$target, p = 0.7, list = FALSE)
index_train_normalize <- index_train_val_normalize
index_validation_normalize <- createDataPartition(normalized_df$target[-index_train_val_normalize], p = 0.5, list = FALSE)

# Create test indexes
index_test_normalize <- setdiff(seq_len(nrow(normalized_df)), c(index_train_normalize, index_validation_normalize))

# Generate train, validation and test data
train_data_normalize<- normalized_df[index_train_normalize, ]
validation_data_normalize <- normalized_df[index_validation_normalize, ]
test_data_normalize <- normalized_df[index_test_normalize, ]


#results
print("Train Data:")
print(head(train_data_normalize))

print("Validation Data:")
print(head(validation_data_normalize))

print("Test Data:")
print(head(test_data_normalize))




# ---- DECISION TREE WITH NORMALIZED DATA
#NORMALIZED DATA 
train_data_normalize$target <- as.factor(train_data_normalize$target)
validation_data_normalize$target <- as.factor(validation_data_normalize$target)
test_data_normalize$target <- as.factor(test_data_normalize$target)

# Checking factor levels in the training dataset
levels(train_data_normalize$target)

#Equalizing factor levels across different data sets
validation_data_normalize$target <- factor(validation_data_normalize$target, levels = levels(train_data_normalize$target))
test_data_normalize$target <- factor(test_data_normalize$target, levels = levels(train_data_normalize$target))

library(rpart)

# Creating the decision tree model
decision_tree_model2 <- rpart(target ~ ., data = train_data_normalize, method = "class")

# Visualize the created model
plot(decision_tree_model2)
text(decision_tree_model2, pretty = 0)

# # Classification on Train data 
predictions_train_normalize <- predict(decision_tree_model2, newdata = train_data_normalize, type = "class")

# Classification on test data
predictions_test_normalize <- predict(decision_tree_model2, newdata = test_data_normalize, type = "class")

conf_matrix_train_normalize <- confusionMatrix(predictions_train_normalize, train_data_normalize$target)
conf_matrix_test_normalize <- confusionMatrix(predictions_test_normalize, test_data_normalize$target)

# Print Results
print("train Data Results:")
print(conf_matrix_train_normalize)

print("Test Data Results:")
print(conf_matrix_test_normalize)

library(caret)

# Sensitivity, Specificity, F1-score and Accuracy calculation function
calculate_metrics <- function(conf_matrix) {
  sensitivity <- conf_matrix[["byClass"]]["Sensitivity"]
  specificity <- conf_matrix[["byClass"]]["Specificity"]
  f1_score <- conf_matrix[["byClass"]]["F1"]
  accuracy <- conf_matrix[["overall"]]["Accuracy"]
  return(c(Sensitivity = sensitivity, Specificity = specificity, F1_Score = f1_score, Accuracy = accuracy))
}

# Calculate metrics
metrics_train_normalize <- calculate_metrics(conf_matrix_train_normalize)
metrics_test_normalize <- calculate_metrics(conf_matrix_test_normalize)

# print result 
cat("Train Data Results:\n")
print(conf_matrix_train_normalize)
print(metrics_train_normalize)

cat("\nTest Data Results:\n")
print(conf_matrix_test_normalize)
print(metrics_test_normalize)

#Visualize the classifier results.
install.packages("rpart.plot")
library(rpart.plot)

prp(decision_tree_model2, extra = 1, main = "Decision Tree Visualization")


#----  Desicion tree normalize  0.8773585 


# KNN -> Normalize
install.packages(c("class", "caret"))
library(class)
library(caret)

k_values <- seq(1,29,by=2)
accuracy_values<- numeric(length (k_values))

for (k in k_values){
  predictions <- knn(train=train_data_normalize[,1:14],
                     test=test_data_normalize[,1:14],
                     cl=train_data_normalize$target,
                     k=k)
  accuracy_values[which(k_values==k)] <-
    sum(predictions==test_data_normalize$target)/ length(test_data_normalize$target)
  
}

accuracy_df <- data.frame(k=k_values,
                          accuracy=accuracy_values)

print (accuracy_df)

ggplot(accuracy_df,aes(x=k,y=accuracy))+
  geom_line()+
  geom_point()+
  labs(title = "knn model accuracy",
       x="k values",
       y="accuracy")

best_k <- k_values[which.max(accuracy_values)]
print(best_k)

final_model <- knn(train=train_data_normalize[,1:14],
                   test=test_data_normalize[,1:14],
                   cl=train_data_normalize$target,
                   k=best_k)

#find the confusion matrix

confusion_matrix <- table (final_model, test_data_normalize$target)
print(confusion_matrix)



# Sensitivity, Specificity, F1-score and Accuracy calculation function
calculate_metrics <- function(predictions, true_labels) {
  conf_matrix <- confusionMatrix(predictions, true_labels)
  sensitivity <- conf_matrix[["byClass"]]["Sensitivity"]
  specificity <- conf_matrix[["byClass"]]["Specificity"]
  f1_score <- conf_matrix[["byClass"]]["F1"]
  accuracy <- conf_matrix[["overall"]]["Accuracy"]
  
  return(c(Sensitivity = sensitivity, Specificity = specificity, F1_Score = f1_score, Accuracy = accuracy))
}

# Find the best k value
best_k <- k_values[which.max(accuracy_values)]

# Classification on train data
predictions_train_normalize <- knn(train = train_data_normalize[, 1:14],
                                   test = train_data_normalize[, 1:14],
                                   cl = train_data_normalize$target,
                                   k = best_k)

# Classification on test data
predictions_test_normalize <- knn(train = train_data_normalize[, 1:14],
                                  test = test_data_normalize[, 1:14],
                                  cl = train_data_normalize$target,
                                  k = best_k)

# Calculate Sensitivity, Specificity, F1-score and Accuracy
metrics_train_normalize <- calculate_metrics(predictions_train_normalize, train_data_normalize$target)
metrics_test_normalize <- calculate_metrics(predictions_test_normalize, test_data_normalize$target)

# Print results
print("Train Data Results:")
print(metrics_train_normalize)

print("\nTest Data Results:")
print(metrics_test_normalize)

#Visualize the classifier results.
knn_plot <- ggplot(accuracy_df, aes(x = k, y = accuracy)) +
  geom_line(aes(color = "KNN"), linetype = "dashed") +
  geom_point(aes(color = "KNN")) +
  labs(title = "Model Comparison",
       x = "k Values",
       y = "Accuracy") +
  theme_minimal()

print(knn_plot)

#----  KNN normalize 

# ------ RANDOM FOREST
#-------NORMALIZE RANDOM FOREST
install.packages("randomForest")
library(randomForest)
library(caret)
train_data_normalize$target <- as.factor(train_data_normalize$target)
test_data_normalize$target <- as.factor(test_data_normalize$target)

# Set parameters
ntree_values <- c(50, 100, 150)
mtry_values <- c(3, 5, 7)

# Evaluate parameter combinations with grid search
results <- list()

for (ntree in ntree_values) {
  for (mtry in mtry_values) {
    model_rf <- randomForest(target ~ ., data = train_data_normalize, ntree = ntree, mtry = mtry)
    predictions <- predict(model_rf, newdata = validation_data_normalize)
    accuracy <- sum(predictions == validation_data_normalize$target) / length(validation_data_normalize$target)
    results[[paste("ntree", ntree, "mtry", mtry)]] <- accuracy
  }
}

# Show Result
print(results)

# Determine best parameter values
ntree_best <- 100
mtry_best <- 3

# Train the final model
final_model_rf <- randomForest(target ~ ., data = train_data_normalize, ntree = ntree_best, mtry = mtry_best)

# Evaluate the model
predictions <- predict(final_model_rf, newdata = test_data_normalize)

# Confusion matrix ve accuracy
confusion_matrix <- table(predictions, test_data_normalize$target)
print(confusion_matrix)

accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
print(paste("Accuracy: ", accuracy ))

# Sensitivity 
sensitivity <- confusion_matrix[2, 2] / sum(confusion_matrix[2, ])
print(paste("Sensitivity: ", sensitivity))

# Specificity 
specificity <- confusion_matrix[1, 1] / sum(confusion_matrix[1, ])
print(paste("Specificity: ", specificity))

# Precision 
precision <- confusion_matrix[2, 2] / sum(confusion_matrix[, 2])
print(paste("Precision: ", precision))

# Recall 
recall <- sensitivity
print(paste("Recall: ", recall))

# F1-Score
f1_score <- 2 * (precision * recall) / (precision + recall)
print(paste("F1-Score: ", f1_score))

#Visualize the classifier results.
accuracy_df <- data.frame(
  Model = "Random Forest",
  Accuracy = accuracy
)

# Creating a bar chart using ggplot2
ggplot(accuracy_df, aes(x = Model, y = Accuracy, fill = Model)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Model Accuracy",
       y = "Accuracy") +
  theme_minimal()
#----  Random Forest normalize 


#----- SVM -->> NORMALIZE 
install.packages("e1071")
library(e1071)
library(caret)

# Train the SVM model
svm_model <- svm(target ~ ., data = train_data_normalize, kernel = "linear")

# Classification on train data
predictions_train_normalize <- predict(svm_model, newdata = train_data_normalize)

# Classification on test data
predictions_test_normalize <- predict(svm_model, newdata = test_data_normalize)


confusion_matrix <- table(predictions, test_data_normalize$target)
print(confusion_matrix)

conf_matrix_train_normalize <- confusionMatrix(predictions_train_normalize, train_data_normalize$target)
conf_matrix_test_normalize <- confusionMatrix(predictions_test_normalize, test_data_normalize$target)

# Print results
print("Train  Data Results:")
print(conf_matrix_train_normalize)

print("Test Data Results:")
print(conf_matrix_test_normalize)

#  (Accuracy) 
accuracy_train <- conf_matrix_train_normalize$overall["Accuracy"]
print(paste("Train data Accuracy:", accuracy_train))

# Sensitivity
sensitivity <- confusion_matrix[2, 2] / sum(confusion_matrix[2, ])
print(paste("Sensitivity: ", sensitivity))

# Specificity 
specificity <- confusion_matrix[1, 1] / sum(confusion_matrix[1, ])
print(paste("Specificity: ", specificity))

# Precision 
precision <- confusion_matrix[2, 2] / sum(confusion_matrix[, 2])
print(paste("Precision: ", precision))

# Recall
recall <- sensitivity
print(paste("Recall: ", recall))

# F1-Score
f1_score <- 2 * (precision * recall) / (precision + recall)
print(paste("F1-Score: ", f1_score))

# Confusion matrix
conf_matrix_test_normalize <- confusionMatrix(predictions_test_normalize, test_data_normalize$target)$table

# Visualization
heatmap(conf_matrix_test_normalize, 
        col = c("white", "blue"), 
        main = "Confusion Matrix - Test Data")
#SVM -->> NORMALIZE 

# LOGISTIC REGRESSION->NORMALIZE 
library(caret)
# Train logistic regression model
logistic_model <- glm(target ~ ., data = train_data_normalize, family = binomial)
# View model summary
summary(logistic_model)
# Make predictions on validation set
predictions <- predict(logistic_model, newdata = train_data_normalize, type = "response")
predicted_classes <- ifelse(predictions > 0.5, 1, 0)
# Confusion matrix and accuracy
confusion_matrix <- table(predicted_classes, train_data_normalize$target)
print(confusion_matrix)

accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
print(paste("Accuracy: ", accuracy ))

# Sensitivity
sensitivity <- confusion_matrix[2, 2] / sum(confusion_matrix[2, ])
print(paste("Sensitivity: ", sensitivity))

# Specificity 
specificity <- confusion_matrix[1, 1] / sum(confusion_matrix[1, ])
print(paste("Specificity: ", specificity))

# Precision 
precision <- confusion_matrix[2, 2] / sum(confusion_matrix[, 2])
print(paste("Precision: ", precision))

# Recall 
recall <- sensitivity
print(paste("Recall: ", recall))

# F1-Score
f1_score <- 2 * (precision * recall) / (precision + recall)
print(paste("F1-Score: ", f1_score))

metrics_df <- data.frame(Metric = c("Accuracy", "Sensitivity", "Specificity", "Precision", "Recall", "F1-Score"),
                         Value = c(accuracy, sensitivity, specificity, precision, recall, f1_score))

# Creating a line chart using ggplot2
library(ggplot2)

ggplot(metrics_df, aes(x = Metric, y = Value)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Logistic Regression Model Metrics",
       y = "Value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#LOGISTIC REGRESSION->NORMALIZE
#------------------------------------------------------------------------------------------------------------

# prediction with the model that gives the best accuracy
# ---- DECISION TREE WITH NORMALIZED DATA

#NORMALIZED DATA 
train_data_normalize$target <- as.factor(train_data_normalize$target)
validation_data_normalize$target <- as.factor(validation_data_normalize$target)
test_data_normalize$target <- as.factor(test_data_normalize$target)


# Checking factor levels in the training dataset
levels(train_data_normalize$target)

#Equalizing factor levels across different data sets
validation_data_normalize$target <- factor(validation_data_normalize$target, levels = levels(train_data_normalize$target))
test_data_normalize$target <- factor(test_data_normalize$target, levels = levels(train_data_normalize$target))

library(rpart)

# Creating the decision tree model
decision_tree_model2 <- rpart(target ~ ., data = train_data_normalize, method = "class")

# Visualize the created model
plot(decision_tree_model2)
text(decision_tree_model2, pretty = 0)


# Classification on train data
predictions_train_normalize <- predict(decision_tree_model2, newdata = train_data_normalize, type = "class")

# Classification on test data
predictions_test_normalize <- predict(decision_tree_model2, newdata = test_data_normalize, type = "class")

conf_matrix_train_normalize <- confusionMatrix(predictions_train_normalize, train_data_normalize$target)
conf_matrix_test_normalize <- confusionMatrix(predictions_test_normalize, test_data_normalize$target)

# Print results
print("train Data Results:")
print(conf_matrix_train_normalize)

print("Test Data Results:")
print(conf_matrix_test_normalize)


library(caret)

# Sensitivity, Specificity, F1-score and Accuracy calculation function
calculate_metrics <- function(conf_matrix) {
  sensitivity <- conf_matrix[["byClass"]]["Sensitivity"]
  specificity <- conf_matrix[["byClass"]]["Specificity"]
  f1_score <- conf_matrix[["byClass"]]["F1"]
  accuracy <- conf_matrix[["overall"]]["Accuracy"]
  
  return(c(Sensitivity = sensitivity, Specificity = specificity, F1_Score = f1_score, Accuracy = accuracy))
}
# Calculate metrics
metrics_train_normalize <- calculate_metrics(conf_matrix_train_normalize)
metrics_test_normalize <- calculate_metrics(conf_matrix_test_normalize)

# Print results
cat("Train Data Results:\n")
print(conf_matrix_train_normalize)
print(metrics_train_normalize)

cat("\nTest Data Results:\n")
print(conf_matrix_test_normalize)
print(metrics_test_normalize)

#Visualize the classifier results.
install.packages("rpart.plot")
library(rpart.plot)

prp(decision_tree_model2, extra = 1, main = "Decision Tree Visualization")
# Create new data
new_data <- data.frame(
  age = c(0.4791667),  
  sex = c(1), 
  cp = c(1.0000000),   
  trestbps = c(0.3157895), 
  chol = c(0.2564103),  
  fbs = c(0),    
  restecg = c(0.0),  
  thalach = c(0.8867925), 
  exang = c(0), 
  oldpeak = c(0.000), 
  slope = c(0.5),  
  ca = c(0.0),    
  thal = c(0.0),  
  cardiovascular_riskscore=c(0.3025478)
)
# predict
prediction_new_data <- predict(decision_tree_model2, newdata = new_data, type = "class")
# Show prediction result
print(prediction_new_data)



