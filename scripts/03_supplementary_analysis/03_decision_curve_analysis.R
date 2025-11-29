# ===================================================================
# Decision Curve Analysis (DCA)
# ===================================================================
# This script performs decision curve analysis to evaluate the clinical
# utility of the prediction model in both internal and external validation sets

# ===================================================================
# Load Required Packages
# ===================================================================
library(rmda)
library(ggplot2)

# ===================================================================
# Prerequisites
# ===================================================================
# Note: This script requires variables from previous analyses:
# - all_actuals, all_predictions: Cross-validation results from internal validation
# - external_data_clean: External validation dataset
# - external_pred_prob: External validation predictions
#
# Please run the model construction and external validation scripts first!

# ===================================================================
# USER CONFIGURATION - Ensure this matches your data directory
# ===================================================================
if (!exists("data_dir")) {
  data_dir <- "/Users/apple/Desktop/间质肉瘤excel数据"  # CHANGE THIS to your data directory
  setwd(data_dir)
}

if (!exists("output_dir")) {
  output_dir <- getwd()  # CHANGE THIS if you want outputs in a different location
}

# ===================================================================
# Prepare Data for DCA
# ===================================================================
print("========== Preparing Data for DCA ==========")

# Prepare internal validation data
internal_dca_data <- data.frame(
  outcome_raw = all_actuals,
  model_prob = all_predictions
)

# Prepare external validation data
external_dca_data <- data.frame(
  outcome_raw = external_data_clean$Diagnosis_Binary,
  model_prob = external_pred_prob
)

# Convert outcome to numeric (0 and 1)
# For internal data
internal_dca_data$outcome <- as.numeric(all_actuals) - 1

# For external data
external_dca_data$outcome <- as.numeric(external_data_clean$Diagnosis_Binary) - 1

# Verify conversion
print("--- Internal data distribution after conversion (should be 0 and 1) ---")
print(table(internal_dca_data$outcome))

print("--- External data distribution after conversion (should be 0 and 1) ---")
print(table(external_dca_data$outcome))

# ===================================================================
# Internal Validation DCA
# ===================================================================
print("========== Performing Internal Validation DCA Analysis ==========")

dca_internal <- decision_curve(
  outcome ~ model_prob,
  data = internal_dca_data,
  family = binomial(link = "logit"),
  thresholds = seq(0, 1, by = 0.01),
  confidence.intervals = 0.95,
  study.design = "cohort"
)

# Save internal DCA plot
pdf("DCA_Internal_Validation.pdf", width = 8, height = 6)

plot_decision_curve(
  dca_internal, 
  curve.names = "My Prediction Model",
  col = "red",
  confidence.intervals = FALSE,
  cost.benefit.axis = FALSE,
  xlab = "Threshold Probability",
  ylab = "Net Benefit",
  main = "Decision Curve Analysis - Internal Validation",
  ylim = c(-0.2, 1)
)

dev.off()

print("✓ Internal DCA plot saved as: DCA_Internal_Validation.pdf")

# ===================================================================
# External Validation DCA
# ===================================================================
print("========== Performing External Validation DCA Analysis ==========")

dca_external <- decision_curve(
  outcome ~ model_prob,
  data = external_dca_data,
  family = binomial(link = "logit"),
  thresholds = seq(0, 1, by = 0.01),
  confidence.intervals = 0.95,
  study.design = "cohort"
)

# ===================================================================
# Combined DCA Comparison Plot
# ===================================================================
print("========== Creating Combined DCA Comparison Plot ==========")

pdf("DCA_Combined_Comparison.pdf", width = 8, height = 7)

# Use corrected parameters to plot comparison
dca_list <- list(dca_internal, dca_external)
plot_decision_curve(
  dca_list,
  
  curve.names = c("Internal Validation Model", "External Validation Model"),
  
  # Specify colors for both model curves
  col = c("red", "darkred"), 
  
  # Key correction: change linetype to lty, use numeric codes
  lty = c(1, 2), # Internal validation use solid line(1), external validation use dashed line(2)
  
  # Other beautification parameters
  confidence.intervals = FALSE,
  cost.benefit.axis = FALSE,
  legend.position = "topright",
  
  xlab = "Threshold Probability",
  ylab = "Net Benefit",
  ylim = c(-0.2, 1)
)

dev.off()

print("✓ Combined comparison DCA plot successfully saved to: DCA_Combined_Comparison.pdf")

# ===================================================================
# Summary Statistics
# ===================================================================
print("========== DCA Summary ==========")

cat("\n")
cat("Decision Curve Analysis evaluates the clinical utility of the model\n")
cat("by calculating net benefit across different threshold probabilities.\n\n")

cat("Internal Validation:\n")
cat("  - Model: Prediction model based on training data\n")
cat("  - Sample size:", nrow(internal_dca_data), "\n\n")

cat("External Validation:\n")
cat("  - Model: Same prediction model applied to external cohort\n")
cat("  - Sample size:", nrow(external_dca_data), "\n\n")

cat("Interpretation:\n")
cat("  - The decision curve shows net benefit at different threshold probabilities\n")
cat("  - Higher net benefit indicates better clinical utility\n")
cat("  - The model is compared against 'treat all' and 'treat none' strategies\n")
cat("  - Useful range: where the model curve is above both reference lines\n\n")

print("========== Decision Curve Analysis Complete ==========")
