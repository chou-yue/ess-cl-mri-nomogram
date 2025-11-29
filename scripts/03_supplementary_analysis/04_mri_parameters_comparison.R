# ===================================================================
# MRI Parameters Comparison Between Training and Validation Sets
# ===================================================================
# This script compares MRI imaging parameters between training and 
# external validation cohorts

# ===================================================================
# Load Required Packages
# ===================================================================
library(readxl)
library(dplyr)
library(tidyr)
library(flextable)
library(officer)

# ===================================================================
# Read MRI Parameters Data
# ===================================================================
print("========== Reading MRI Parameters Data ==========")

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

all_data_wide <- read_excel("MR参数.xlsx", sheet = "Sheet1")

# Convert all data to character type for consistency
all_data_wide <- all_data_wide %>%
  mutate(across(.fns = as.character))

# ===================================================================
# Data Transformation
# ===================================================================
print("========== Transforming Data ==========")

# Transform from wide to long format
all_data_long <- all_data_wide %>%
  pivot_longer(
    cols = -c(Cohort, Series),
    names_to = "Parameter",
    values_to = "Value"
  )

# Create dual-row structure
table_data <- all_data_long %>%
  pivot_wider(
    id_cols = c(Series, Parameter),
    names_from = Cohort,
    values_from = Value
  ) %>%
  # Reorganize data, create two rows for each parameter
  pivot_longer(
    cols = c(`Training Set`, `Validation Set`),
    names_to = "Dataset",
    values_to = "Value"
  ) %>%
  pivot_wider(
    id_cols = c(Parameter, Dataset),
    names_from = Series,
    values_from = Value
  ) %>%
  arrange(Parameter, Dataset) %>%
  arrange(desc(Parameter), Dataset) %>%
  # Remove Dataset column
  select(-Dataset)

# ===================================================================
# Create and Format Table
# ===================================================================
print("========== Creating Formatted Table ==========")

final_table <- flextable(table_data) %>%
  add_header_lines("Table. Comparison of MRI Imaging Parameters Between Training and External Validation Sets") %>%
  bold(i = 1, part = "header") %>%
  align(i = 1, align = "left", part = "header") %>%
  set_header_labels(Parameter = "Parameter") %>%
  # Remove default borders and add custom borders
  border_remove() %>%
  # Add top line at parameter row (1.5pt)
  border(i = 2, border.top = fp_border(color = "black", width = 1.5), part = "header") %>%
  # Add middle line below parameter row (0.5pt)
  border(i = 2, border.bottom = fp_border(color = "black", width = 0.5), part = "header") %>%
  # Add bottom line at table bottom (0.5pt)
  border(i = nrow(table_data), border.bottom = fp_border(color = "black", width = 0.5), part = "body") %>%
  autofit() %>%
  align(align = "center", part = "all") %>%
  align(j = 1, align = "left", part = "all") %>%
  bold(part = "header") %>%
  font(fontname = "Times New Roman", part = "all") %>%
  fontsize(size = 10, part = "all") %>%
  line_spacing(space = 1, part = "all") %>%
  # Merge cells with same parameter in Parameter column
  merge_v(j = "Parameter") %>%
  add_footer_lines(c(
    "Training set data (first row) and validation set data (second row, gray background) are shown. Imaging protocols: FS-T2WI and DWI (b=1000 s/mm²) for training set; Dixon-T2WI and DWI (b=800 s/mm²) for validation set."
  )) %>%
  fontsize(size = 8, part = "footer") %>%
  # Left align footer notes
  align(align = "left", part = "footer")

print(final_table)

# ===================================================================
# Save as Word Document
# ===================================================================
save_as_docx(final_table, path = file.path(output_dir, "MRI_Parameters_Comparison.docx"))

print("✓ Table successfully saved as: MRI_Parameters_Comparison.docx")
print("========== MRI Parameters Comparison Complete ==========")
