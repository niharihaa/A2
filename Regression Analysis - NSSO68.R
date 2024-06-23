# Load necessary libraries
library(dplyr)
library(car)
library(MASS)

# Load the dataset
data <- read.csv("C:/Users/nihar/OneDrive/Desktop/Bootcamp/SCMA 632/DataSet/NSSO68.csv")

# View the first few rows of the dataset
head(data)

# Check the structure and summary of the data
str(data)
summary(data)

# Function to get mode for categorical columns
get_mode <- function(x) {
  uniqx <- unique(x)
  uniqx[which.max(tabulate(match(x, uniqx)))]
}

# Replace missing values with median for numeric columns and mode for categorical columns
data <- data %>%
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), median(., na.rm = TRUE), .))) %>%
  mutate(across(where(is.character), ~ ifelse(is.na(.), get_mode(.), .)))

# Ensure categorical variables are treated as factors
data <- data %>%
  mutate(across(where(is.character), as.factor))

# Function to cap outliers using the IQR method
cap_outliers <- function(x) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower <- Q1 - 1.5 * IQR
  upper <- Q3 + 1.5 * IQR
  x[x < lower] <- lower
  x[x > upper] <- upper
  return(x)
}

# Apply the function to numeric columns
data <- data %>%
  mutate(across(where(is.numeric), cap_outliers))

# Define the sets of variables
dependent_var1 <- "MPCE_URP"
independent_vars1 <- c("Age", "Education", "hhdsz", "Social_Group", "Sex")

dependent_var2 <- "MPCE_MRP"
independent_vars2 <- c("HH_type", "Religion", "Whether_owns_any_land", "Regular_salary_earner", "Meals_At_Home")

dependent_var3 <- "MPCE_URP"
independent_vars3 <- c("Land_Total_possessed", "Land_Owned", "Cooking_code", "Lighting_code", "Dwelling_unit_code")

# Model 1

if (nrow(data[complete.cases(data[, c(dependent_var1, independent_vars1)]), ]) > 0) {
  model1 <- lm(as.formula(paste(dependent_var1, "~", paste(independent_vars1, collapse = "+"))), data = data)
  print(summary(model1))
} else {
  print("No non-NA cases for the first model")
}

# Diagnostic plots for model1
if (exists("model1")) {
  png("model1_diagnostics.png")
  par(mfrow = c(2, 2))
  plot(model1)
  dev.off()
}

browseURL("model1_diagnostics.png")

# Identify aliased coefficients in the model
aliased_coefs <- alias(model1)$Complete

# Remove aliased variables from the list of independent variables
independent_vars1 <- independent_vars1[!independent_vars1 %in% rownames(aliased_coefs)]

# Refit the model without the aliased variables
model1 <- lm(as.formula(paste(dependent_var1, "~", paste(independent_vars1, collapse = "+"))), data = data)

# Check the summary of the refitted model
summary(model1)

# Check for multicollinearity in the refitted model
vif_values <- vif(model1)
print(vif_values)

# Transform the dependent variable if necessary
data$log_MPCE_URP <- log(data$MPCE_URP)

# Fit robust regression model
model1_robust <- rlm(as.formula(paste("log(MPCE_URP) ~", paste(independent_vars1, collapse = "+"))), data = data)

# Summary of the robust regression model
summary(model1_robust)

# Diagnostic plots for the robust regression model
png("model1_robust_diagnostics.png")
par(mfrow = c(2, 2))
plot(model1_robust)
dev.off()

browseURL("model1_robust_diagnostics.png")

# Generate predictions using the robust regression model
predictions <- predict(model1_robust, newdata = data)

# Validate the model 
set.seed(123)
train_indices <- sample(seq_len(nrow(data)), size = 0.7 * nrow(data))
train_data <- data[train_indices, ]
test_data <- data[-train_indices, ]

# Fit the model on the training data
model1_robust_train <- rlm(as.formula(paste("log(MPCE_URP) ~", paste(independent_vars1, collapse = "+"))), data = train_data)

# Predict on the test data
predictions_test <- predict(model1_robust_train, newdata = test_data)

# Evaluate model performance on the test data
actuals_test <- log(test_data$MPCE_URP)
rmse <- sqrt(mean((predictions_test - actuals_test)^2))
print(paste("RMSE on test data:", rmse))

# Interpretation of the model coefficients
summary(model1_robust)

# Model 2
if (nrow(data[complete.cases(data[, c(dependent_var2, independent_vars2)]), ]) > 0) {
  model2 <- lm(as.formula(paste(dependent_var2, "~", paste(independent_vars2, collapse = "+"))), data = data)
  print(summary(model2))
} else {
  print("No non-NA cases for the second model")
}

# Diagnostic plots for model2
if (exists("model2")) {
  png("model2_diagnostics.png")
  par(mfrow = c(2, 2))
  plot(model2)
  dev.off()
}

browseURL("model2_diagnostics.png")

# Identify aliased coefficients in model2
aliased_coefs2 <- alias(model2)$Complete
print(aliased_coefs2)

# Remove aliased variables from the list of independent variables
independent_vars2 <- independent_vars2[!independent_vars2 %in% rownames(aliased_coefs2)]

# Refit the model without aliased variables
model2 <- lm(as.formula(paste(dependent_var2, "~", paste(independent_vars2, collapse = "+"))), data = data)

# Check the summary of the refitted model
summary(model2)

# Check for multicollinearity in the refitted model
vif_values2 <- vif(model2)
print(vif_values2)

# Transform the dependent variable if necessary
data$log_MPCE_MRP <- log(data$MPCE_MRP)

# Fit robust regression model
model2_robust <- rlm(as.formula(paste("log(MPCE_MRP) ~", paste(independent_vars2, collapse = "+"))), data = data)

# Summary of the robust regression model
summary(model2_robust)

# Diagnostic plots for the robust regression model
png("model2_robust_diagnostics.png")
par(mfrow = c(2, 2))
plot(model2_robust)
dev.off()

browseURL("model2_robust_diagnostics.png")

# Generate predictions using the robust regression model
predictions2 <- predict(model2_robust, newdata = data)

# Validate the model using a train-test split
set.seed(123)
train_indices2 <- sample(seq_len(nrow(data)), size = 0.7 * nrow(data))
train_data2 <- data[train_indices, ]
test_data2 <- data[-train_indices, ]

# Fit the model on the training data
model2_robust_train <- rlm(as.formula(paste("log(MPCE_MRP) ~", paste(independent_vars2, collapse = "+"))), data = train_data)

# Predict on the test data
predictions_test2 <- predict(model2_robust_train, newdata = test_data)

# Evaluate model performance on the test data
actuals_test <- log(test_data$MPCE_MRP)
rmse <- sqrt(mean((predictions_test - actuals_test)^2))
print(paste("RMSE on test data:", rmse))

# Interpretation of the model coefficients
summary(model2_robust)

# Model 3
if (nrow(data[complete.cases(data[, c(dependent_var3, independent_vars3)]), ]) > 0) {
  model3 <- lm(as.formula(paste(dependent_var3, "~", paste(independent_vars3, collapse = "+"))), data = data)
  print(summary(model3))
} else {
  print("No non-NA cases for the third model")
}

# Diagnostic plots for model3
if (exists("model3")) {
  png("model3_diagnostics.png")
  par(mfrow = c(2, 2))
  plot(model3)
  dev.off()
}

browseURL("model3_diagnostics.png")

# Identify aliased coefficients in model3
aliased_coefs3 <- alias(model3)$Complete
print(aliased_coefs3)

# Remove aliased variables from the list of independent variables
independent_vars3 <- independent_vars3[!independent_vars3 %in% rownames(aliased_coefs3)]

# Refit the model without aliased variables
model3 <- lm(as.formula(paste(dependent_var3, "~", paste(independent_vars3, collapse = "+"))), data = data)

# Check the summary of the refitted model
summary(model3)

# Check for multicollinearity in the refitted model
vif_values3 <- vif(model3)
print(vif_values3)

# Transform the dependent variable if necessary
data$log_MPCE_URP3 <- log(data$MPCE_URP)

# Fit robust regression model
model3_robust <- rlm(as.formula(paste("log(MPCE_URP) ~", paste(independent_vars3, collapse = "+"))), data = data)

# Summary of the robust regression model
summary(model3_robust)

# Diagnostic plots for the robust regression model
png("model3_robust_diagnostics.png")
par(mfrow = c(2, 2))
plot(model3_robust)
dev.off()

browseURL("model3_robust_diagnostics.png")

# Generate predictions using the robust regression model
predictions3 <- predict(model3_robust, newdata = data)

# Validate the model using a train-test split
set.seed(123)
train_indices3 <- sample(seq_len(nrow(data)), size = 0.7 * nrow(data))
train_data3 <- data[train_indices3, ]
test_data3 <- data[-train_indices3, ]

# Fit the model on the training data
model3_robust_train <- rlm(as.formula(paste("log(MPCE_URP) ~", paste(independent_vars3, collapse = "+"))), data = train_data3)

# Predict on the test data
predictions_test3 <- predict(model3_robust_train, newdata = test_data3)

# Evaluate model performance on the test data
actuals_test3 <- log(test_data3$MPCE_URP)
rmse3 <- sqrt(mean((predictions_test3 - actuals_test3)^2))
print(paste("RMSE on test data:", rmse3))

# Interpretation of the model coefficients
summary(model3_robust)
