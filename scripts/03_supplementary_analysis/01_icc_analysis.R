# ===================================================================
# Inter-reader Agreement Analysis (ICC) - Training Set
# ===================================================================
# This script analyzes inter-reader reliability for ADC histogram metrics:
# minADC, meanADC, maxADC, P10, P25, P50, P75, P90, skewness, kurtosis
# 
# Includes:
# 1. Descriptive statistics for both readers
# 2. Intraclass Correlation Coefficient (ICC) analysis
# 3. Bland-Altman plots for agreement visualization

# ===================================================================
# Load Required Packages
# ===================================================================
library(readxl)
library(psych)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(corrplot)
library(tidyr)
library(cowplot)
library(flextable)
library(irr)

# ===================================================================
# Step 1: Read Excel Files
# ===================================================================
print("=== Reading Excel Files ===")

# ===================================================================
# USER CONFIGURATION - Please modify the path below
# ===================================================================
if (!exists("data_dir")) {
  data_dir <- "/Users/apple/Desktop/间质肉瘤excel数据"  # CHANGE THIS to your data directory
  setwd(data_dir)
}

if (!exists("output_dir")) {
  output_dir <- getwd()  # CHANGE THIS if you want outputs in a different location
}

# Read first Excel file (Reader 1)
data1 <- read_excel("Training Set Consistency Analysis 1.xlsx")
print("File 1 structure:")
print(colnames(data1))
print(paste("File 1 rows:", nrow(data1)))
print("File 1 first few rows:")
print(head(data1))

# Read second Excel file (Reader 2)
data2 <- read_excel("Training Set Consistency Analysis 2.xlsx")
print("File 2 structure:")
print(colnames(data2))
print(paste("File 2 rows:", nrow(data2)))

# ===================================================================
# Step 2: Define Target Metrics
# ===================================================================
target_metrics <- c("minADC", "meanADC", "maxADC", "P10", "P25", "P50", "P75", "P90", "skewness", "kurtosis")

print("\n=== Target Analysis Metrics ===")
print(target_metrics)

# Check which metrics are available in the data
available_metrics1 <- intersect(target_metrics, colnames(data1))
available_metrics2 <- intersect(target_metrics, colnames(data2))
common_metrics <- intersect(available_metrics1, available_metrics2)

print(paste("Available metrics in file 1:", paste(available_metrics1, collapse = ", ")))
print(paste("Available metrics in file 2:", paste(available_metrics2, collapse = ", ")))
print(paste("Common metrics in both files:", paste(common_metrics, collapse = ", ")))

if (length(common_metrics) == 0) {
  stop("Error: No common target metrics found in both files! Please check column names.")
}

# ===================================================================
# Step 3: Data Preprocessing and Matching
# ===================================================================
print("\n=== Data Preprocessing ===")

# Get ID column name (should be the image ID)
id_col <- colnames(data1)[1]
print(paste("Identified ID column name:", id_col))

# Select required columns: ID column and all available target metrics
data1_selected <- data1 %>%
  select(all_of(c(id_col, common_metrics))) %>%
  rename(ID = !!id_col)

data2_selected <- data2 %>%
  select(all_of(c(id_col, common_metrics))) %>%
  rename(ID = !!id_col)

# Add suffixes to distinguish two measurements
colnames(data1_selected)[2:ncol(data1_selected)] <- paste0(colnames(data1_selected)[2:ncol(data1_selected)], "_1")
colnames(data2_selected)[2:ncol(data2_selected)] <- paste0(colnames(data2_selected)[2:ncol(data2_selected)], "_2")

print("Processed data1 column names:")
print(colnames(data1_selected))
print("Processed data2 column names:")
print(colnames(data2_selected))

# Merge two datasets by ID
merged_data <- merge(data1_selected, data2_selected, by = "ID", all = FALSE)
print(paste("Merged data rows:", nrow(merged_data)))

# ===================================================================
# Step 4: Data Quality Check
# ===================================================================
print("\n=== Data Quality Check ===")

# Check for missing values
missing_summary <- merged_data %>%
  summarise_all(~sum(is.na(.))) %>%
  gather(key = "variable", value = "missing_count")

print("Missing value statistics for each variable:")
print(missing_summary)

# Convert all numeric columns to numeric type
numeric_cols <- colnames(merged_data)[2:ncol(merged_data)]
for (col in numeric_cols) {
  merged_data[[col]] <- as.numeric(merged_data[[col]])
}

# Remove rows with any missing values
initial_rows <- nrow(merged_data)
merged_data <- merged_data %>%
  filter(complete.cases(.))
final_rows <- nrow(merged_data)

print(paste("Rows before removing missing values:", initial_rows))
print(paste("Rows after removing missing values:", final_rows))
print(paste("Rows removed:", initial_rows - final_rows))

if (final_rows == 0) {
  stop("Error: No complete data rows after cleaning!")
}

# ===================================================================
# Step 5: Descriptive Statistics
# ===================================================================
print("\n=== Descriptive Statistics ===")

desc_stats <- data.frame()

for (metric in common_metrics) {
  col1 <- paste0(metric, "_1")
  col2 <- paste0(metric, "_2")
  
  if (col1 %in% colnames(merged_data) && col2 %in% colnames(merged_data)) {
    stats <- data.frame(
      Metric = metric,
      Mean_1 = mean(merged_data[[col1]], na.rm = TRUE),
      SD_1 = sd(merged_data[[col1]], na.rm = TRUE),
      Mean_2 = mean(merged_data[[col2]], na.rm = TRUE),
      SD_2 = sd(merged_data[[col2]], na.rm = TRUE),
      Difference_Mean = mean(merged_data[[col1]] - merged_data[[col2]], na.rm = TRUE),
      Difference_SD = sd(merged_data[[col1]] - merged_data[[col2]], na.rm = TRUE),
      Correlation = cor(merged_data[[col1]], merged_data[[col2]], use = "complete.obs")
    )
    desc_stats <- rbind(desc_stats, stats)
  }
}

print("Descriptive statistics for each metric:")
numeric_cols_desc <- sapply(desc_stats, is.numeric)
desc_stats_display <- desc_stats
desc_stats_display[numeric_cols_desc] <- round(desc_stats_display[numeric_cols_desc], 4)
print(desc_stats_display)

# ===================================================================
# Step 6: ICC Analysis
# ===================================================================
print("\n=== ICC Analysis ===")

icc_results <- data.frame()

for (metric in common_metrics) {
  col1 <- paste0(metric, "_1")
  col2 <- paste0(metric, "_2")
  
  if (col1 %in% colnames(merged_data) && col2 %in% colnames(merged_data)) {
    print(paste("\n--- Analyzing", metric, "---"))
    
    # Prepare data
    icc_data <- merged_data %>%
      select(all_of(c(col1, col2))) %>%
      filter(complete.cases(.))
    
    # Rename columns for ICC analysis
    colnames(icc_data) <- c("Measurement1", "Measurement2")
    
    if (nrow(icc_data) >= 3) {  # ICC requires at least 3 observations
      tryCatch({
        # Calculate ICC
        icc_result <- ICC(icc_data)
        
        # Extract ICC(2,1) and ICC(3,1) results
        icc_2_1 <- icc_result$results[2, ]  # ICC(2,1)
        icc_3_1 <- icc_result$results[6, ]  # ICC(3,1)
        
        print(paste("ICC(2,1):", round(icc_2_1$ICC, 4)))
        print(paste("95% CI:", round(icc_2_1$`lower bound`, 4), "-", round(icc_2_1$`upper bound`, 4)))
        print(paste("P-value:", format(icc_2_1$p, scientific = TRUE)))
        
        # Consistency interpretation
        icc_value <- icc_2_1$ICC
        if (icc_value >= 0.90) {
          interpretation <- "Excellent"
        } else if (icc_value >= 0.75) {
          interpretation <- "Good"
        } else if (icc_value >= 0.50) {
          interpretation <- "Moderate"
        } else {
          interpretation <- "Poor"
        }
        
        # Save results
        result_row <- data.frame(
          Metric = metric,
          ICC_2_1 = icc_2_1$ICC,
          ICC_2_1_LowerCI = icc_2_1$`lower bound`,
          ICC_2_1_UpperCI = icc_2_1$`upper bound`,
          ICC_2_1_F = icc_2_1$F,
          ICC_2_1_p = icc_2_1$p,
          ICC_3_1 = icc_3_1$ICC,
          ICC_3_1_LowerCI = icc_3_1$`lower bound`,
          ICC_3_1_UpperCI = icc_3_1$`upper bound`,
          Interpretation = interpretation,
          SampleSize = nrow(icc_data)
        )
        
        icc_results <- rbind(icc_results, result_row)
        
      }, error = function(e) {
        print(paste("Error calculating ICC for", metric, ":", e$message))
        
        # Record basic information even on error
        result_row <- data.frame(
          Metric = metric,
          ICC_2_1 = NA,
          ICC_2_1_LowerCI = NA,
          ICC_2_1_UpperCI = NA,
          ICC_2_1_F = NA,
          ICC_2_1_p = NA,
          ICC_3_1 = NA,
          ICC_3_1_LowerCI = NA,
          ICC_3_1_UpperCI = NA,
          Interpretation = "Calculation Failed",
          SampleSize = nrow(icc_data)
        )
        
        icc_results <- rbind(icc_results, result_row)
      })
    } else {
      print(paste(metric, "has insufficient valid data points, cannot calculate ICC"))
    }
  }
}

# ===================================================================
# Step 7: Results Summary
# ===================================================================
print("\n=== ICC Analysis Results Summary ===")
if (nrow(icc_results) > 0) {
  numeric_cols_icc <- sapply(icc_results, is.numeric)
  icc_display <- icc_results
  icc_display[numeric_cols_icc] <- round(icc_display[numeric_cols_icc], 4)
  print(icc_display[, 1:7])  # Display main results
}

# ===================================================================
# Step 8: Bland-Altman Plots (10 metrics)
# ===================================================================
print("\n=== Generating Bland-Altman Plots ===")

all_metrics <- c("minADC", "meanADC", "maxADC", "P10", "P25", "P50", "P75", "P90", "skewness", "kurtosis")

# Calculate appropriate y-axis range for each metric
plot_data_list <- list()

for (i in seq_along(all_metrics)) {
  metric <- all_metrics[i]
  col1 <- paste0(metric, "_1")
  col2 <- paste0(metric, "_2")
  
  if (col1 %in% colnames(merged_data) && col2 %in% colnames(merged_data)) {
    mean_val <- (merged_data[[col1]] + merged_data[[col2]]) / 2
    diff_val <- merged_data[[col1]] - merged_data[[col2]]
    
    mean_diff <- mean(diff_val, na.rm = TRUE)
    sd_diff <- sd(diff_val, na.rm = TRUE)
    upper_loa <- mean_diff + 1.96 * sd_diff
    lower_loa <- mean_diff - 1.96 * sd_diff
    
    # Calculate y-axis range (centered on zero line)
    y_min_data <- min(diff_val, na.rm = TRUE)
    y_max_data <- max(diff_val, na.rm = TRUE)
    
    all_y_values <- c(y_min_data, y_max_data, mean_diff, upper_loa, lower_loa, 0)
    y_min_needed <- min(all_y_values)
    y_max_needed <- max(all_y_values)
    
    # Center zero line: find maximum distance from zero, then create symmetric range
    max_distance_from_zero <- max(abs(y_min_needed), abs(y_max_needed))
    y_limits <- c(-max_distance_from_zero * 1.1, max_distance_from_zero * 1.1)
    
    plot_data_list[[i]] <- list(
      data = data.frame(mean_val = mean_val, diff_val = diff_val),
      mean_diff = mean_diff,
      sd_diff = sd_diff,
      upper_loa = upper_loa,
      lower_loa = lower_loa,
      metric = metric,
      y_limits = y_limits
    )
  }
}

# Create all subplots
plot_list <- list()

for (i in seq_along(plot_data_list)) {
  if (!is.null(plot_data_list[[i]])) {
    plot_info <- plot_data_list[[i]]
    plot_data <- plot_info$data
    mean_diff <- plot_info$mean_diff
    upper_loa <- plot_info$upper_loa
    lower_loa <- plot_info$lower_loa
    metric <- plot_info$metric
    y_limits <- plot_info$y_limits
    
    p <- ggplot(plot_data, aes(x = mean_val, y = diff_val)) +
      geom_point(alpha = 0.6, size = 1.5) +
      geom_hline(yintercept = 0, color = "gray", linetype = "dotted", alpha = 0.7, size = 1.2) +
      geom_hline(yintercept = mean_diff, color = "blue", linetype = "solid", size = 1) +
      geom_hline(yintercept = upper_loa, color = "red", linetype = "dashed", size = 1) +
      geom_hline(yintercept = lower_loa, color = "red", linetype = "dashed", size = 1) +
      geom_smooth(method = "lm", se = TRUE, color = "green", alpha = 0.3) +
      coord_cartesian(ylim = y_limits) +
      scale_x_continuous(expand = expansion(mult = c(0.05, 0.05))) +
      scale_y_continuous(expand = expansion(mult = c(0.02, 0.02))) +
      annotate("text", x = Inf, y = mean_diff, 
               label = paste("Mean:", round(mean_diff, 3)), 
               hjust = 1.1, vjust = -0.5, color = "blue", size = 3) +
      annotate("text", x = Inf, y = upper_loa, 
               label = paste("+1.96 SD:", round(upper_loa, 3)), 
               hjust = 1.1, vjust = -0.5, color = "red", size = 3) +
      annotate("text", x = Inf, y = lower_loa, 
               label = paste("-1.96 SD:", round(lower_loa, 3)), 
               hjust = 1.1, vjust = 1.5, color = "red", size = 3) +
      labs(
        title = paste("Bland-Altman:", metric),
        x = paste("Mean of", metric),
        y = paste("Diff (", metric, "_1 - ", metric, "_2)", sep = "")
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 10, face = "bold"),
        axis.title = element_text(size = 9),
        axis.text = element_text(size = 8),
        panel.grid.minor = element_blank(),
        plot.margin = margin(5.5, 5.5, 5.5, 5.5, "pt")
      )
    
    plot_list[[i]] <- p
  }
}

# Fill empty positions
num_plots <- length(plot_list)
if (num_plots < 12) {
  for (i in (num_plots + 1):12) {
    plot_list[[i]] <- NULL
  }
}

# Combine plots
final_plot <- plot_grid(
  plotlist = plot_list,
  ncol = 4,
  align = 'vh'
)

# Save
save_plot(
  "Bland_Altman_Plots.pdf",
  plot = final_plot,
  base_width = 16,
  base_height = 12,
  units = "in"
)

cat("Bland-Altman plots saved as: Bland_Altman_Plots.pdf\n")

# ===================================================================
# Step 9: Generate Summary Table
# ===================================================================
print("\n=== Generating Summary Table ===")

# Ensure all ADC columns are numeric
merged_data <- merged_data %>%
  mutate(across(-ID, as.numeric))

metrics_to_analyze <- c(
  "minADC", "meanADC", "maxADC", 
  "P10", "P25", "P50", "P75", "P90", 
  "skewness", "kurtosis"
)

results_list <- list()

for (metric in metrics_to_analyze) {
  col1_name <- paste0(metric, "_1")
  col2_name <- paste0(metric, "_2")
  
  if (col1_name %in% names(merged_data) && col2_name %in% names(merged_data)) {
    
    data_subset <- merged_data %>%
      select(all_of(c(col1_name, col2_name))) %>%
      na.omit()
    
    differences <- data_subset[[col1_name]] - data_subset[[col2_name]]
    
    bias <- mean(differences)
    sd_diff <- sd(differences)
    lower_loa <- bias - 1.96 * sd_diff
    upper_loa <- bias + 1.96 * sd_diff
    loa_width <- upper_loa - lower_loa
    p_value_bias <- t.test(differences, mu = 0)$p.value
    
    icc_result <- tryCatch({
      icc_val <- irr::icc(data_subset, model = "twoway", type = "agreement", unit = "single")$value
      round(icc_val, 2)
    }, error = function(e) {
      NA 
    })
    
    results_list[[metric]] <- data.frame(
      Metric = metric,
      Bias = bias,
      SD_of_Difference = sd_diff,
      Lower_LoA = lower_loa,
      Upper_LoA = upper_loa,
      LoA_Width = loa_width,
      ICC = icc_result,
      P_value_Bias = p_value_bias
    )
  } else {
    warning(paste("Data missing", col1_name, "or", col2_name, ", skipping this metric."))
  }
}

summary_table <- bind_rows(results_list)

# Create and format table
final_table <- flextable(summary_table)
final_table <- set_header_labels(final_table,
                                 Metric = "Metric",
                                 Bias = "Mean\nDifference (Bias)",
                                 SD_of_Difference = "SD of\nDifference",
                                 Lower_LoA = "Lower LoA",
                                 Upper_LoA = "Upper LoA",
                                 LoA_Width = "LoA Width",
                                 ICC = "ICC\n(Agreement)",
                                 P_value_Bias = "P-value\n(Bias)"
)
final_table <- colformat_double(final_table, j = 2:8, digits = 3)
final_table <- colformat_double(final_table, j = "ICC", digits = 2)
final_table <- theme_booktabs(final_table)
final_table <- width(final_table, width = 1)
final_table <- fontsize(final_table, size = 8, part = "all")
final_table <- align(final_table, align = "center", part = "all")
final_table <- align(final_table, j = 1, align = "left", part = "all")

# Save as Word file
output_filename <- "Agreement_Statistics_Table.docx"
save_as_docx(final_table, path = file.path(output_dir, output_filename))
print(final_table)
message(paste("Table with manually specified metrics successfully saved as:", output_filename))

# ===================================================================
# Step 10: Save Results
# ===================================================================
print("\n=== Saving Results ===")

# Save merged raw data
write.csv(merged_data, "merged_all_metrics_data.csv", row.names = FALSE)
cat("Merged raw data saved as 'merged_all_metrics_data.csv'\n")

# Save descriptive statistics
write.csv(desc_stats, "descriptive_statistics.csv", row.names = FALSE)
cat("Descriptive statistics saved as 'descriptive_statistics.csv'\n")

# Save ICC results
write.csv(icc_results, "icc_analysis_results.csv", row.names = FALSE)
cat("ICC analysis results saved as 'icc_analysis_results.csv'\n")

# Create simplified results table for reporting
summary_table_report <- icc_results %>%
  select(Metric, ICC_2_1, ICC_2_1_LowerCI, ICC_2_1_UpperCI, Interpretation) %>%
  rename(
    "Metric" = Metric,
    "ICC Value" = ICC_2_1,
    "95% CI Lower" = ICC_2_1_LowerCI,
    "95% CI Upper" = ICC_2_1_UpperCI,
    "Consistency Evaluation" = Interpretation
  )

write.csv(summary_table_report, "icc_summary_table.csv", row.names = FALSE)
cat("ICC results summary table saved as 'icc_summary_table.csv'\n")

print("\n=== Analysis Complete ===")
print("All result files saved to current working directory.")
