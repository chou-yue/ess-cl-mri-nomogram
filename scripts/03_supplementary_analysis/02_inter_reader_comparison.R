# ===================================================================
# Inter-reader Comparison of ADC Histogram Metrics
# ===================================================================
# This script compares ADC histogram metrics between two readers,
# stratified by diagnosis groups (CL vs ESS)

# ===================================================================
# Load Required Packages
# ===================================================================
library(readxl)
library(dplyr)
library(tidyr)
library(kableExtra)
library(flextable)
library(officer)

# ===================================================================
# Read Excel Files
# ===================================================================
print("=== Reading Excel Files ===")

# Note: Ensure Excel files are in the same directory as the R script, 
# otherwise provide full path

# Read actual Excel files
data1 <- read_excel("Training Set Consistency Analysis 1.xlsx")
data2 <- read_excel("Training Set Consistency Analysis 2.xlsx")

print(paste("Reader 1 data rows:", nrow(data1)))
print(paste("Reader 2 data rows:", nrow(data2)))

# Define ADC metrics to analyze
target_metrics <- c("minADC", "meanADC", "maxADC", "P10", "P25",
                    "P50", "P75", "P90", "skewness", "kurtosis")

# ===================================================================
# Step 2: Data Merging and Grouping
# ===================================================================
# Match by row number (assuming rows in both files correspond to same patients)
min_rows <- min(nrow(data1), nrow(data2))

# Merge data
merged_data <- data.frame(
  # Diagnosis information
  Diagnosis_Binary = data1$Diagnosis_Binary[1:min_rows],
  # Reader 1 data
  data1[1:min_rows, target_metrics],
  # Reader 2 data
  data2[1:min_rows, target_metrics]
)

# Rename columns
colnames(merged_data) <- c("Diagnosis_Binary",
                           paste0(target_metrics, "_Reader1"),
                           paste0(target_metrics, "_Reader2"))

# ===================================================================
# Step 3: Fix Data Types
# ===================================================================
print("\n=== Force Converting Metric Columns to Numeric ===")
# This is a key step to resolve errors
# Loop through all metric columns, force convert data type from "character" to "numeric"
# suppressWarnings suppresses warnings from text that cannot be converted to numbers, 
# these texts will automatically become NA
metric_cols_to_convert <- c(paste0(target_metrics, "_Reader1"), paste0(target_metrics, "_Reader2"))
for (col in metric_cols_to_convert) {
  merged_data[[col]] <- suppressWarnings(as.numeric(as.character(merged_data[[col]])))
}
print("Data type conversion complete.")

# Create diagnosis labels
merged_data$Diagnosis_Label <- ifelse(merged_data$Diagnosis_Binary == 0, "Leiomyoma", "Sarcoma")

print(paste("Merged data rows:", nrow(merged_data)))
# Check diagnosis grouping
diagnosis_counts <- table(merged_data$Diagnosis_Label)
print("Diagnosis grouping statistics:")
print(diagnosis_counts)

# ===================================================================
# Step 4: Calculate Statistics and P-values
# ===================================================================
print("\n=== Calculating Statistics ===")
# Create three-line table dataframe
table_data <- data.frame(
  MRI_features = character(),
  ALM_Reader1 = character(),
  US_Reader1 = character(),
  P_Reader1 = character(),
  ALM_Reader2 = character(),
  US_Reader2 = character(),
  P_Reader2 = character(),
  stringsAsFactors = FALSE
)

# Get sample sizes
n_leiomyoma <- sum(merged_data$Diagnosis_Binary == 0, na.rm = TRUE)
n_sarcoma <- sum(merged_data$Diagnosis_Binary == 1, na.rm = TRUE)

for(metric in target_metrics) {
  reader1_col <- paste0(metric, "_Reader1")
  reader2_col <- paste0(metric, "_Reader2")
  
  # Group data
  leiomyoma_data <- merged_data[merged_data$Diagnosis_Binary == 0, ]
  sarcoma_data <- merged_data[merged_data$Diagnosis_Binary == 1, ]
  
  # Reader 1 statistics
  # Leiomyoma group
  r1_leio <- leiomyoma_data[[reader1_col]][!is.na(leiomyoma_data[[reader1_col]])]
  r1_leio_stat <- ifelse(length(r1_leio) > 0,
                         paste0(round(mean(r1_leio), 2), " ± ", round(sd(r1_leio), 2)),
                         "NA")
  
  # Sarcoma group
  r1_sarc <- sarcoma_data[[reader1_col]][!is.na(sarcoma_data[[reader1_col]])]
  r1_sarc_stat <- ifelse(length(r1_sarc) > 0,
                         paste0(round(mean(r1_sarc), 2), " ± ", round(sd(r1_sarc), 2)),
                         "NA")
  
  # Reader 1 P-value (Leiomyoma vs Sarcoma)
  p_value_r1 <- "NA"
  if(length(r1_leio) > 2 && length(r1_sarc) > 2) {
    t_test_r1 <- t.test(r1_leio, r1_sarc)
    p_value_r1 <- ifelse(t_test_r1$p.value < 0.001, "< 0.001",
                         sprintf("%.3f", t_test_r1$p.value))
  }
  
  # Reader 2 statistics
  # Leiomyoma group
  r2_leio <- leiomyoma_data[[reader2_col]][!is.na(leiomyoma_data[[reader2_col]])]
  r2_leio_stat <- ifelse(length(r2_leio) > 0,
                         paste0(round(mean(r2_leio), 2), " ± ", round(sd(r2_leio), 2)),
                         "NA")
  
  # Sarcoma group
  r2_sarc <- sarcoma_data[[reader2_col]][!is.na(sarcoma_data[[reader2_col]])]
  r2_sarc_stat <- ifelse(length(r2_sarc) > 0,
                         paste0(round(mean(r2_sarc), 2), " ± ", round(sd(r2_sarc), 2)),
                         "NA")
  
  # Reader 2 P-value (Leiomyoma vs Sarcoma)
  p_value_r2 <- "NA"
  if(length(r2_leio) > 2 && length(r2_sarc) > 2) {
    t_test_r2 <- t.test(r2_leio, r2_sarc)
    p_value_r2 <- ifelse(t_test_r2$p.value < 0.001, "< 0.001",
                         sprintf("%.3f", t_test_r2$p.value))
  }
  
  # Add to table
  table_data <- rbind(table_data, data.frame(
    MRI_features = metric,
    ALM_Reader1 = r1_leio_stat,
    US_Reader1 = r1_sarc_stat,
    P_Reader1 = p_value_r1,
    ALM_Reader2 = r2_leio_stat,
    US_Reader2 = r2_sarc_stat,
    P_Reader2 = p_value_r2,
    stringsAsFactors = FALSE
  ))
}

# ===================================================================
# Step 5: Generate Three-line Table
# ===================================================================
print("\n=== Generating Three-line Table Format ===")

# Set column names
colnames(table_data) <- c("ADC Metrics",
                          paste0("CL (n = ", n_leiomyoma, ")"),
                          paste0("US (n = ", n_sarcoma, ")"),
                          "P",
                          paste0("CL (n = ", n_leiomyoma, ")"),
                          paste0("US (n = ", n_sarcoma, ")"),
                          "P")

# Generate three-line table using kableExtra (if available)
if(require(kableExtra, quietly = TRUE)) {
  three_line_table <- table_data %>%
    kable("html",
          caption = "Table. Analysis Results of Ten ADC Histogram Metrics by Two Readers",
          escape = FALSE,
          align = c("l", rep("c", 6))) %>%
    kable_styling(bootstrap_options = "basic",
                  full_width = FALSE,
                  position = "center") %>%
    # Add header grouping
    add_header_above(c(" " = 1, "Reader 1" = 3, "Reader 2" = 3)) %>%
    # Create three-line table style manually
    # First line: top thick line
    # Second line: thin line below header
    # Third line: bottom thick line
    row_spec(0, bold = TRUE, extra_css = "border-top: 2px solid black; border-bottom: 1px solid black;") %>%
    row_spec(nrow(table_data), extra_css = "border-bottom: 2px solid black;") %>%
    # Remove all internal lines between cells
    column_spec(1:ncol(table_data), border_left = FALSE, border_right = FALSE)
  
  print(three_line_table)
} else {
  # If kableExtra is not available, use basic output
  print("=== ADC Metric Analysis Results (Two Readers) ===")
  print(table_data)
}

# ===================================================================
# Step 6: Create Flextable and Save as Word
# ===================================================================
ft <- flextable(table_data)

# Run separately
ft <- set_header_labels(ft,
                        MRI_features = "ADC Metrics",
                        ALM_Reader1 = paste0("CL (n = ", n_leiomyoma, ")"),
                        US_Reader1 = paste0("ESS (n = ", n_sarcoma, ")"),
                        P_Reader1 = "P",
                        ALM_Reader2 = paste0("CL (n = ", n_leiomyoma, ")"),
                        US_Reader2 = paste0("ESS (n = ", n_sarcoma, ")"),
                        P_Reader2 = "P"
)

# Add header row with correct parameter name
ft <- add_header_row(ft, values = c("", "Reader 1", "Reader 2"), colwidths = c(1, 3, 3))
ft <- theme_booktabs(ft)
ft <- set_caption(ft, "Table. Analysis Results of Ten ADC Histogram Metrics by Two Readers")
ft <- autofit(ft)

# Save as Word document
doc <- read_docx()
doc <- body_add_flextable(doc, value = ft)
output_filename <- "ADC_Analysis_Table.docx"
print(doc, target = file.path(output_dir, output_filename))
print(paste0("Table successfully saved as '", output_filename, "'"))

print("========== Inter-reader Comparison Analysis Complete ==========")
