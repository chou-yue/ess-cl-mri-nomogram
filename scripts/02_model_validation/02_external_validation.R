# ===================================================================
# External Validation Analysis
# ===================================================================
# This script performs external validation of the prediction model:
# 1. Load and preprocess external validation data
# 2. Apply the trained model for prediction
# 3. Calculate performance metrics (AUC, sensitivity, specificity, etc.)
# 4. Generate calibration curve
# 5. Compare internal vs external validation performance
# 6. DeLong test for AUC comparison

# ===================================================================
# Load Required Packages
# ===================================================================
library(readxl)
library(tidyverse)
library(pROC)
library(CalibrationCurves)
library(ResourceSelection)
library(ggplot2)
library(flextable)
library(officer)
library(dplyr)
library(tibble)

# ===================================================================
# Prerequisites
# ===================================================================
# Note: This script requires variables from the model construction script:
# - final_model_for_eval: The trained logistic regression model
# - data_for_model: Training dataset
# - all_actuals, all_predictions: Cross-validation results
# - cv_auc: Cross-validation AUC values
# - final_roc: ROC curve from training set

# Please run 01_model_construction_and_evaluation.R first!

# ===================================================================
# USER CONFIGURATION - Ensure this matches your data directory
# ===================================================================
# If not already set, define data_dir (should match previous script)
if (!exists("data_dir")) {
  data_dir <- "/Users/apple/Desktop/间质肉瘤excel数据"  # CHANGE THIS to your data directory
  setwd(data_dir)
}

if (!exists("output_dir")) {
  output_dir <- getwd()  # CHANGE THIS if you want outputs in a different location
}

# ===================================================================
# Load External Validation Data
# ===================================================================
print("========== Loading External Validation Data ==========")

external_data <- read_excel("external_validation_data.xlsx")

print("========== External Validation Data Overview ==========")
print(paste("Total external validation samples:", nrow(external_data)))
print(paste("Number of variables:", ncol(external_data)))
print("Variable list:")
print(names(external_data))

# Check for required variables
required_vars <- c("Diagnosis_Binary", "Margin", "CysticAreaT2", "meanADC")
if(!all(required_vars %in% names(external_data))) {
  missing <- setdiff(required_vars, names(external_data))
  stop(paste("External data missing variables:", paste(missing, collapse = ", ")))
} else {
  print("✓ All required variables are present")
}

# ===================================================================
# Check Outcome Variable Distribution
# ===================================================================
print("========== External Validation Set Disease Distribution ==========")
table(external_data$Diagnosis_Binary)
prop.table(table(external_data$Diagnosis_Binary))

# Check for missing values
print("========== Missing Value Check ==========")
sapply(external_data[, required_vars], function(x) sum(is.na(x)))

# ===================================================================
# Data Preprocessing
# ===================================================================
print("========== Data Preprocessing ==========")

external_data_clean <- external_data %>%
  mutate(
    # Convert ADC to numeric
    meanADC = as.numeric(meanADC),
    
    # Convert outcome variable to factor (must match training set)
    Diagnosis_Binary = factor(Diagnosis_Binary, 
                              levels = c("0", "1"), 
                              labels = c("CL", "ESS")),
    
    # Convert all binary variables to factors (must match training set)
    Margin = factor(Margin, levels = c("0", "1"), labels = c("Regular", "Irregular")),
    CysticAreaT2 = factor(CysticAreaT2, levels = c("0", "1"), labels = c("No", "Yes"))
  )

# Remove missing values
external_data_clean <- external_data_clean %>%
  select(all_of(required_vars)) %>%
  na.omit()

print(paste("Clean external validation samples:", nrow(external_data_clean)))

# Check data structure
str(external_data_clean)

# ===================================================================
# External Validation - Main Analysis
# ===================================================================
print("========== Starting External Validation ==========")

# Use the trained model for prediction
external_pred_prob <- predict(final_model_for_eval, 
                              newdata = external_data_clean, 
                              type = "response")

# Check prediction probability distribution
print("Prediction probability distribution:")
summary(external_pred_prob)

# ===================================================================
# Calculate ROC and AUC
# ===================================================================
external_roc <- roc(external_data_clean$Diagnosis_Binary, 
                    external_pred_prob,
                    levels = c("CL", "ESS"),
                    direction = "<")

external_auc <- auc(external_roc)
ci_auc <- ci.auc(external_roc, conf.level = 0.95)

print("========== External Validation AUC Results ==========")
print(paste("External Validation AUC:", round(external_auc, 3)))
print(paste("95% CI: [", round(ci_auc[1], 3), "-", round(ci_auc[3], 3), "]"))

# ===================================================================
# Calculate Best Threshold and Other Metrics
# ===================================================================
best_threshold <- coords(external_roc, "best", 
                         ret = c("threshold", "sensitivity", "specificity",
                                 "ppv", "npv", "accuracy"))

print("========== Performance Metrics at Best Threshold ==========")
print(paste("Best Threshold:", round(best_threshold$threshold, 3)))
print(paste("Sensitivity:", round(best_threshold$sensitivity, 3)))
print(paste("Specificity:", round(best_threshold$specificity, 3)))
print(paste("PPV:", round(best_threshold$ppv, 3)))
print(paste("NPV:", round(best_threshold$npv, 3)))
print(paste("Accuracy:", round(best_threshold$accuracy, 3)))

# ===================================================================
# External Validation Calibration Curve
# ===================================================================
print("========== Calibration Assessment ==========")

# Hosmer-Lemeshow test
actual_outcome <- as.numeric(external_data_clean$Diagnosis_Binary) - 1
hl_test <- hoslem.test(actual_outcome, external_pred_prob, g = 10)

print(paste("Hosmer-Lemeshow test p-value:", round(hl_test$p.value, 3)))
if(hl_test$p.value > 0.05) {
  print("✓ Good calibration (p > 0.05)")
} else {
  print("⚠ Poor calibration (p < 0.05)")
}

# Plot calibration curve
pdf("External_Calibration_Plot.pdf", width = 5, height = 5)
val.prob(external_pred_prob, 
         actual_outcome,
         logistic.cal = TRUE,
         xlab = "Predicted Probability",
         ylab = "Observed Probability",
         statloc = FALSE)
title(main = "External Validation Calibration Curve")
dev.off()

# ===================================================================
# Internal vs External Validation Comparison
# ===================================================================
print("========== Internal vs External Validation Comparison ==========")

# Create comparison table
validation_comparison <- data.frame(
  Metric = c("AUC", "Sensitivity", "Specificity", "Sample size"),
  Internal = c(round(mean(cv_auc), 3), 
               round(mean(cv_sensitivity), 3),
               round(mean(cv_specificity), 3),
               nrow(data_for_model)),
  External = c(round(external_auc, 3), 
               round(best_threshold$sensitivity, 3),
               round(best_threshold$specificity, 3),
               nrow(external_data_clean))
)

print(validation_comparison)

# Calculate AUC drop
auc_drop <- mean(cv_auc) - external_auc
print(paste("AUC drop:", round(auc_drop, 3)))
if(auc_drop < 0.05) {
  print("✓ Excellent: AUC drop < 0.05")
} else if(auc_drop < 0.10) {
  print("✓ Good: AUC drop 0.05-0.10")
} else {
  print("⚠ Attention needed: AUC drop > 0.10")
}

# ===================================================================
# Comparison ROC Curves
# ===================================================================
print("========== Generating Comparison ROC Curves ==========")

# Prepare internal validation data
overall_roc <- roc(all_actuals, all_predictions)
overall_auc <- auc(overall_roc)
internal_data <- data.frame(
  specificity = overall_roc$specificities,
  sensitivity = overall_roc$sensitivities,
  validation = "Internal (10-Fold CV)",
  auc = round(mean(cv_auc), 3)
)

# Prepare external validation data
external_data_plot <- data.frame(
  specificity = external_roc$specificities,
  sensitivity = external_roc$sensitivities,
  validation = "External",
  auc = round(external_auc, 3)
)

# Combine data
combined_roc_data <- rbind(internal_data, external_data_plot)

# Create comparison plot
p_roc_comparison <- ggplot(combined_roc_data, aes(x = 1 - specificity, y = sensitivity, color = validation)) +
  geom_line(size = 1.5) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray50") +
  scale_color_manual(values = c("Internal (10-Fold CV)" = "blue", "External" = "red")) +
  labs(
    x = "1 - Specificity (False Positive Rate)",
    y = "Sensitivity (True Positive Rate)",
    color = "Validation Type"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    legend.position = "bottom",
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  ) +
  coord_equal() +
  xlim(0, 1) +
  ylim(0, 1)

print(p_roc_comparison)
ggsave("ROC_Comparison.pdf", plot = p_roc_comparison, width = 8, height = 6)

# ===================================================================
# Generate Detailed Report
# ===================================================================
sink("External_Validation_Report.txt")

cat("======================================\n")
cat("External Validation Report\n")
cat("======================================\n\n")

cat("1. Data Overview\n")
cat(paste("  - External validation samples:", nrow(external_data_clean), "\n"))
cat(paste("  - ESS cases:", sum(external_data_clean$Diagnosis_Binary == "ESS"), "\n"))
cat(paste("  - CL cases:", sum(external_data_clean$Diagnosis_Binary == "CL"), "\n\n"))

cat("2. Model Performance\n")
cat(paste("  - AUC:", round(external_auc, 3), "\n"))
cat(paste("  - 95% CI:", round(ci_auc[1], 3), "-", round(ci_auc[3], 3), "\n"))
cat(paste("  - Sensitivity:", round(best_threshold$sensitivity, 3), "\n"))
cat(paste("  - Specificity:", round(best_threshold$specificity, 3), "\n"))
cat(paste("  - PPV:", round(best_threshold$ppv, 3), "\n"))
cat(paste("  - NPV:", round(best_threshold$npv, 3), "\n\n"))

cat("3. Calibration\n")
cat(paste("  - Hosmer-Lemeshow p-value:", round(hl_test$p.value, 3), "\n\n"))

cat("4. Comparison with Internal Validation\n")
cat(paste("  - Internal AUC:", round(mean(cv_auc), 3), "\n"))
cat(paste("  - External AUC:", round(external_auc, 3), "\n"))
cat(paste("  - AUC drop:", round(auc_drop, 3), "\n"))

sink()

print("✓ Report saved to External_Validation_Report.txt")

# ===================================================================
# Performance Table with 95% CI
# ===================================================================
print("========== Creating Performance Table ==========")

# Helper function for formatting CI
fmt_ci <- function(x_hat, lwr, upr, digits = 3) {
  paste0(round(x_hat, digits), " (", round(lwr, digits), " - ", round(upr, digits), ")")
}

# Helper function for binomial CI
binom_ci_fmt <- function(success, total, digits = 3) {
  bt <- binom.test(success, total, conf.level = 0.95)
  fmt_ci(unname(bt$estimate), bt$conf.int[1], bt$conf.int[2], digits)
}

# Get TP, TN, FP, FN for internal validation
internal_best <- coords(
  final_roc, "best",
  ret = c("threshold","sensitivity","specificity","ppv","npv","tp","tn","fp","fn"),
  best.method = "youden",
  best.policy = "closest.topleft",
  transpose = FALSE
)

# Get TP, TN, FP, FN for external validation
external_best <- coords(
  external_roc, "best",
  ret = c("threshold","sensitivity","specificity","ppv","npv","tp","tn","fp","fn"),
  best.method = "youden",
  best.policy = "closest.topleft",
  transpose = FALSE
)

# Calculate metrics for internal set
tp_i <- as.numeric(internal_best["tp"]); tn_i <- as.numeric(internal_best["tn"])
fp_i <- as.numeric(internal_best["fp"]); fn_i <- as.numeric(internal_best["fn"])

sens_i <- binom_ci_fmt(tp_i, tp_i + fn_i)
spec_i <- binom_ci_fmt(tn_i, tn_i + fp_i)
ppv_i  <- binom_ci_fmt(tp_i, tp_i + fp_i)
npv_i  <- binom_ci_fmt(tn_i, tn_i + fn_i)

# Calculate metrics for external set
tp_e <- as.numeric(external_best["tp"]); tn_e <- as.numeric(external_best["tn"])
fp_e <- as.numeric(external_best["fp"]); fn_e <- as.numeric(external_best["fn"])

sens_e <- binom_ci_fmt(tp_e, tp_e + fn_e)
spec_e <- binom_ci_fmt(tn_e, tn_e + fp_e)
ppv_e  <- binom_ci_fmt(tp_e, tp_e + fp_e)
npv_e  <- binom_ci_fmt(tn_e, tn_e + fn_e)

# AUC with 95% CI
internal_ci <- ci.auc(final_roc)
external_ci <- ci.auc(external_roc)

# Create performance table
performance_table_data <- tibble(
  `Characteristic` = c("Training Set", "External Validation Set"),
  
  `AUC (95% CI)` = c(
    paste0(round(auc(final_roc), 3), " (", round(internal_ci[1], 3), " - ", round(internal_ci[3], 3), ")"),
    paste0(round(auc(external_roc), 3), " (", round(external_ci[1], 3), " - ", round(external_ci[3], 3), ")")
  ),
  
  `Threshold` = c(
    as.character(round(as.numeric(internal_best["threshold"]), 3)),
    as.character(round(as.numeric(external_best["threshold"]), 3))
  ),
  
  `Sensitivity (95% CI)` = c(sens_i, sens_e),
  `Specificity (95% CI)` = c(spec_i, spec_e),
  `PPV (95% CI)`         = c(ppv_i,  ppv_e),
  `NPV (95% CI)`         = c(npv_i,  npv_e)
)

# Format and save table
formatted_table <- flextable(performance_table_data) %>%
  set_caption("Table. Performance of the Prediction Model in the Development and External Validation Cohorts") %>%
  align(j = 1, align = "left", part = "all") %>%
  align(j = 2:ncol(performance_table_data), align = "center", part = "all") %>%
  bold(part = "header") %>%
  border_remove() %>%
  hline_top(part = "header", border = fp_border(color = "black", width = 1.5)) %>%
  hline_bottom(part = "header", border = fp_border(color = "black", width = 1)) %>%
  hline_bottom(part = "body", border = fp_border(color = "black", width = 1.5)) %>%
  autofit()

print(formatted_table)

save_as_docx(formatted_table, path = file.path(output_dir, "Final_Performance_Table.docx"))
cat("✓ Performance table formatted and saved successfully.\n")

# ===================================================================
# DeLong Test (Internal vs External AUC Comparison)
# ===================================================================
print("========== DeLong Test for AUC Comparison ==========")

print(paste0("Internal AUC: ", round(auc(final_roc), 3),
             " (95% CI ", round(internal_ci[1], 3), " - ", round(internal_ci[3], 3), ")"))

print(paste0("External AUC: ", round(auc(external_roc), 3),
             " (95% CI ", round(external_ci[1], 3), " - ", round(external_ci[3], 3), ")"))

delong_test <- roc.test(final_roc, external_roc, method = "delong")
print(delong_test)

print("========== External Validation Complete ==========")
