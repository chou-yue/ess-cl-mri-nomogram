# ===================================================================
# Model Construction and Internal Validation
# ===================================================================
# This script performs:
# 1. Multivariate logistic regression model building
# 2. 10-fold cross-validation
# 3. ROC curve analysis
# 4. Nomogram creation
# 5. Calibration curve assessment

# ===================================================================
# Load Required Packages
# ===================================================================
library(readxl)
library(tidyverse)
library(gtsummary)
library(car)
library(pROC)
library(rms)
library(caret)
library(ggplot2)
library(flextable)
library(officer)

# ===================================================================
# USER CONFIGURATION - Please modify the path below
# ===================================================================
# Set this to the directory containing your data files
data_dir <- "/Users/apple/Desktop/间质肉瘤excel数据"  # CHANGE THIS to your data directory
setwd(data_dir)

# You can also set output directory (optional, defaults to current directory)
output_dir <- getwd()  # CHANGE THIS if you want outputs in a different location

# ===================================================================
# Load Data
# ===================================================================
my_data <- read_excel("final_data_1.5.xlsx")

# ===================================================================
# Data Preprocessing
# ===================================================================
final_data <- my_data %>%
  mutate(
    # Convert numeric variables
    age = as.numeric(age),
    TumorSize_cm = as.numeric(TumorSize_cm),
    CA125 = as.numeric(CA125),
    minADC = as.numeric(minADC),
    meanADC = as.numeric(meanADC),
    maxADC = as.numeric(maxADC),
    P10 = as.numeric(P10),
    P25 = as.numeric(P25),
    P50 = as.numeric(P50),
    P75 = as.numeric(P75),
    P90 = as.numeric(P90),
    skewness = as.numeric(skewness),
    kurtosis = as.numeric(kurtosis),
    
    # Convert outcome variable to binary factor
    Diagnosis_Binary = factor(Diagnosis_Binary, 
                              levels = c("0", "1"), 
                              labels = c("CL", "ESS")),
    
    # Convert binary variables to factors
    MRI_T2_HigherThanMyo = factor(MRI_T2_HigherThanMyo, levels = c("0", "1"), labels = c("No", "Yes")),
    DWI_HighSignal = factor(DWI_HighSignal, levels = c("0", "1"), labels = c("No", "Yes")),
    Hemorrhage = factor(Hemorrhage, levels = c("0", "1"), labels = c("No", "Yes")),
    CysticAreaT2 = factor(CysticAreaT2, levels = c("0", "1"), labels = c("No", "Yes")),
    NonEnhancingCenter = factor(NonEnhancingCenter, levels = c("0", "1"), labels = c("No", "Yes")),
    Margin = factor(Margin, levels = c("0", "1"), labels = c("Regular", "Irregular"))
  )

print("--- Data Structure ---")
str(final_data)

# ===================================================================
# Multivariate Regression Analysis
# ===================================================================
print("--- Multivariate Logistic Regression Analysis ---")

# Define candidate variables based on univariate analysis results
candidate_vars_for_vif <- c(
  "Margin",
  "Hemorrhage", 
  "CysticAreaT2",
  "NonEnhancingCenter",
  "meanADC",
  "maxADC",
  "P10",
  "P25",
  "P50", 
  "P75",
  "P90",
  "kurtosis"
)

# Multicollinearity check using VIF
formula_for_vif <- as.formula(paste("Diagnosis_Binary ~", paste(candidate_vars_for_vif, collapse = " + ")))
vif_model <- glm(formula_for_vif, data = final_data, family = binomial, na.action = na.omit)

if(length(coef(vif_model)) > 1) {
  vif_values <- vif(vif_model)
  print("--- VIF Values for Candidate Variables ---")
  print(vif_values)
  
  high_vif <- names(vif_values)[vif_values > 5]
  if(length(high_vif) > 0) {
    print(paste("High collinearity detected for:", paste(high_vif, collapse = ", ")))
  }
} else {
  print("VIF model fitting failed, please check variable settings")
}

# Simplified variable list (after removing high collinearity variables)
candidate_vars_simplified <- c(
  "Margin",
  "Hemorrhage",
  "CysticAreaT2",
  "NonEnhancingCenter",
  "meanADC",
  "kurtosis"
)

# Create clean dataset for modeling
print("--- Checking Variable Availability ---")
existing_vars <- candidate_vars_simplified[candidate_vars_simplified %in% names(final_data)]
missing_vars <- candidate_vars_simplified[!candidate_vars_simplified %in% names(final_data)]

if(length(missing_vars) > 0) {
  print(paste("Missing variables:", paste(missing_vars, collapse = ", ")))
}

print(paste("Available variables:", paste(existing_vars, collapse = ", ")))

data_for_model <- final_data %>%
  select(Diagnosis_Binary, all_of(existing_vars)) %>%
  na.omit() 

print(paste("Total complete cases for modeling:", nrow(data_for_model)))
print("--- Distribution by Group ---")
print(table(data_for_model$Diagnosis_Binary))

# Stepwise regression
if(nrow(data_for_model) > 0 && length(candidate_vars_simplified) > 0) {
  # Build full model
  full_model_for_step <- glm(Diagnosis_Binary ~ ., 
                             data = data_for_model, 
                             family = binomial)
  
  # Run stepwise regression
  stepwise_model <- step(full_model_for_step, direction = "both", trace = FALSE)
  
  print("--- Final Model from Stepwise Selection ---")
  summary(stepwise_model)
  
  # Format final model results
  final_model_table <- tbl_regression(
    stepwise_model, 
    exponentiate = TRUE
  ) %>%
    modify_header(
      label = "**Characteristic**",
      estimate = "**OR (95% CI)**",
      p.value = "**P**"
    ) %>%
    modify_column_merge(
      pattern = "{estimate} ({conf.low}, {conf.high})",
      rows = !is.na(estimate)
    ) %>%
    modify_column_hide(c(conf.low, conf.high)) %>%
    bold_p(t = 0.05)
  
  print("--- Final Multivariate Regression Model Results ---")
  print(final_model_table)
  
} else {
  print("Data preparation failed, please check variable settings and data integrity")
}

# ===================================================================
# Model Performance Evaluation
# ===================================================================

# Extract final predictors from the model
final_predictors <- all.vars(formula(stepwise_model))[-1]

print("--- Final Predictors Used in the Model ---")
print(final_predictors)

# Create clean dataset with final predictors
data_for_model <- final_data %>%
  select(Diagnosis_Binary, all_of(final_predictors)) %>%
  na.omit() 

print(paste("Final complete cases for model evaluation:", nrow(data_for_model)))

# Refit the final model
final_model_for_eval <- glm(Diagnosis_Binary ~ ., 
                            data = data_for_model, 
                            family = binomial)

# ===================================================================
# 10-Fold Cross-Validation
# ===================================================================
print("========== Starting 10-Fold Cross-Validation ==========")

set.seed(18)
k <- 10
n <- nrow(data_for_model)

# Create stratified folds
folds <- createFolds(data_for_model$Diagnosis_Binary, 
                     k = k, 
                     list = TRUE,
                     returnTrain = FALSE)

# Initialize result storage
cv_auc <- numeric(k)
cv_sensitivity <- numeric(k)
cv_specificity <- numeric(k)
all_predictions <- numeric(0)
all_actuals <- numeric(0)
roc_list <- list()

# Perform cross-validation
for(i in 1:k) {
  test_indices <- folds[[i]]
  train_data <- data_for_model[-test_indices, ]
  test_data <- data_for_model[test_indices, ]
  
  # Fit model on training set
  fold_model <- glm(Diagnosis_Binary ~ ., 
                    data = train_data, 
                    family = binomial)
  
  # Predict on test set
  pred_prob <- predict(fold_model, 
                       newdata = test_data, 
                       type = "response")
  
  # Store predictions and actuals
  all_predictions <- c(all_predictions, pred_prob)
  all_actuals <- c(all_actuals, test_data$Diagnosis_Binary)
  
  # Calculate ROC and AUC for this fold
  roc_obj <- roc(test_data$Diagnosis_Binary, pred_prob, quiet = TRUE)
  cv_auc[i] <- auc(roc_obj)
  
  # Save ROC object
  roc_list[[i]] <- roc_obj
  
  # Calculate sensitivity and specificity at best threshold
  best_coords <- coords(roc_obj, "best", 
                        ret = c("sensitivity", "specificity"))
  cv_sensitivity[i] <- best_coords$sensitivity
  cv_specificity[i] <- best_coords$specificity
  
  print(paste("Fold", i, ":",
              "AUC =", round(cv_auc[i], 3),
              "| Training set:", nrow(train_data), "cases",
              "| Test set:", nrow(test_data), "cases"))
}

# Print summary results
print("========== Cross-Validation Summary ==========")
print(paste("Mean AUC:", round(mean(cv_auc), 3), "±", round(sd(cv_auc), 3)))
print(paste("Mean Sensitivity:", round(mean(cv_sensitivity), 3), "±", round(sd(cv_sensitivity), 3)))
print(paste("Mean Specificity:", round(mean(cv_specificity), 3), "±", round(sd(cv_specificity), 3)))

# ===================================================================
# ROC Curve Visualization
# ===================================================================

# Create mean ROC curve data
spec_grid <- seq(1, 0, by = -0.01)
sens_matrix <- matrix(NA, nrow = length(spec_grid), ncol = k)

for(i in 1:k) {
  current_spec <- roc_list[[i]]$specificities
  current_sens <- roc_list[[i]]$sensitivities
  
  unique_indices <- !duplicated(current_spec)
  unique_spec <- current_spec[unique_indices]
  unique_sens <- current_sens[unique_indices]
  
  sens_matrix[, i] <- suppressWarnings(
    approx(x = unique_spec, 
           y = unique_sens, 
           xout = spec_grid, 
           method = "linear",
           rule = 2)$y
  )
}

mean_sens <- rowMeans(sens_matrix, na.rm = TRUE)
sd_sens <- apply(sens_matrix, 1, sd, na.rm = TRUE)

roc_df <- data.frame(
  specificity = spec_grid,
  sensitivity = mean_sens,
  sd = sd_sens,
  upper = pmin(1, mean_sens + sd_sens),
  lower = pmax(0, mean_sens - sd_sens)
)

# Plot mean ROC curve with error band
p_roc <- ggplot(roc_df, aes(x = 1 - specificity, y = sensitivity)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, fill = "blue") +
  geom_line(color = "blue", linewidth = 1.5) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray") +
  labs(subtitle = paste("Mean AUC =", round(mean(cv_auc), 3), "±", round(sd(cv_auc), 3)),
       x = "1 - Specificity (False Positive Rate)", 
       y = "Sensitivity (True Positive Rate)") +
  theme_minimal() +
  coord_equal()

print(p_roc)

pdf("ROC_curve_10fold_CV.pdf", width = 5, height = 5)
print(p_roc)
dev.off()

print("ROC curve saved as: ROC_curve_10fold_CV.pdf")

# ===================================================================
# Nomogram Construction
# ===================================================================
library(rms)

# Set datadist
ddist <- datadist(data_for_model)
options(datadist = 'ddist')

# Check factor variable levels
print("Margin variable information:")
print(class(data_for_model$Margin))
print(table(data_for_model$Margin))

print("CysticAreaT2 variable information:")
print(class(data_for_model$CysticAreaT2))
print(table(data_for_model$CysticAreaT2))

# Ensure variables are factors
if(!is.factor(data_for_model$Margin)) {
  data_for_model$Margin <- factor(data_for_model$Margin, 
                                  levels = c("Regular", "Irregular"),
                                  labels = c("Regular", "Irregular"))
}

if(!is.factor(data_for_model$CysticAreaT2)) {
  data_for_model$CysticAreaT2 <- factor(data_for_model$CysticAreaT2,
                                        levels = c("No", "Yes"),
                                        labels = c("No", "Yes"))
}

# Reset datadist
ddist <- datadist(data_for_model)
options(datadist = 'ddist')

# Refit model using lrm
lrm_final_model <- lrm(Diagnosis_Binary ~ Margin + CysticAreaT2 + meanADC,
                       data = data_for_model,
                       x = TRUE, 
                       y = TRUE)

print("LRM Model Summary:")
print(lrm_final_model)

# Create nomogram
nom <- nomogram(lrm_final_model,
                fun = function(x) 1/(1+exp(-x)),
                lp = TRUE,
                funlabel = "Probability of ESS",
                fun.at = c(0.001, 0.01, 0.05, 0.1, 0.2, 0.3, 0.4, 0.5,
                           0.6, 0.7, 0.8, 0.9, 0.95, 0.99, 0.999))

# Save nomogram as PDF
pdf("Nomogram_ESS_Prediction.pdf", width = 12, height = 8)
plot(nom,
     xfrac = 0.35,
     label.every = 1,
     col.grid = gray(c(0.85, 0.95)),
     lmgp = 0.2,
     points.label = "Points",
     total.points.label = "Total Points",
     total.sep.page = FALSE,
     cap.labels = FALSE,
     cex.axis = 0.85,
     cex.var = 1.0)
dev.off()

# Display nomogram in R
plot(nom,
     xfrac = 0.35,
     label.every = 1,
     col.grid = gray(c(0.85, 0.95)),
     points.label = "Points",
     total.points.label = "Total Points",
     cex.axis = 0.85,
     cex.var = 1.0)
title("Nomogram for ESS Prediction")

# ===================================================================
# Calculate Optimal Threshold
# ===================================================================
final_pred <- predict(final_model_for_eval, 
                      newdata = data_for_model, 
                      type = "response")
final_roc <- roc(data_for_model$Diagnosis_Binary, final_pred)

best_threshold_from_training <- coords(final_roc, 
                                       "best", 
                                       ret = "threshold", 
                                       best.method="youden")

print(paste("Optimal diagnostic threshold from training set:", 
            round(best_threshold_from_training$threshold, 3)))

# ===================================================================
# Internal Calibration Curve
# ===================================================================
library(CalibrationCurves)
library(ResourceSelection)

internal_pred_prob <- predict(final_model_for_eval, 
                              newdata = data_for_model, 
                              type = "response")

# Hosmer-Lemeshow test
actual_outcome_internal <- as.numeric(data_for_model$Diagnosis_Binary) - 1
hl_test_internal <- hoslem.test(actual_outcome_internal, internal_pred_prob, g = 10)

print("========== Internal Training Set Calibration Assessment ==========")
print(paste("Hosmer-Lemeshow test p-value:", round(hl_test_internal$p.value, 3)))

if(hl_test_internal$p.value > 0.05) {
  print("✓ Good calibration (p > 0.05)")
} else {
  print("⚠ Poor calibration (p < 0.05)")
}

# Plot calibration curve
pdf("Internal_Calibration_Plot.pdf", width = 5, height = 5)
val.prob(internal_pred_prob, actual_outcome_internal, 
         logistic.cal = TRUE, 
         xlab = "Predicted Probability", 
         ylab = "Observed Probability",
         statloc = FALSE)
title(main = "Internal Training Set Calibration Curve")
dev.off()

print("========== Model Construction and Internal Validation Complete ==========")
