# ===================================================================
# Training vs Validation Set Feature Comparison
# ===================================================================
# This script compares the significant features between training and 
# external validation cohorts

# ===================================================================
# Load Required Packages
# ===================================================================
library(readxl)
library(dplyr)
library(flextable)
library(officer)
library(here)
library(tidyr)

# ===================================================================
# Data Preprocessing Function
# ===================================================================
preprocess_data <- function(data) {
  data %>%
    mutate(
      across(
        c(any_of("meanADC"), 
          any_of("Diagnosis_Binary"), 
          any_of("Margin"), 
          any_of("CysticAreaT2")),
        as.numeric
      )
    )
}

# ===================================================================
# Core Analysis Function
# ===================================================================
analyze_dataset <- function(data) {
  
  # Helper function to format p-values
  format_p_value <- function(p) {
    if (is.na(p)) return("N/A")
    if (p < 0.001) return("<0.001")
    if (p < 0.01) return(sprintf("%.3f", p))
    return(sprintf("%.2f", p))
  }
  
  # Calculate statistics for categorical variables
  calc_categorical <- function(df, variable) {
    summary_data <- df %>%
      group_by(Diagnosis_Binary) %>%
      summarise(
        count = sum({{ variable }} == 1, na.rm = TRUE),
        total = n(),
        .groups = 'drop'
      ) %>%
      mutate(label = paste0(count, " (", round(count / total * 100, 1), "%)"))
    
    cl_label <- summary_data$label[summary_data$Diagnosis_Binary == 0]
    ess_label <- summary_data$label[summary_data$Diagnosis_Binary == 1]
    
    contingency_table <- table(df$Diagnosis_Binary, df[[as.character(ensym(variable))]])
    p_value <- fisher.test(contingency_table)$p.value
    
    tibble(
      CL = cl_label,
      ESS = ess_label,
      P_value = format_p_value(p_value)
    )
  }
  
  # Calculate statistics for continuous variables
  calc_continuous <- function(df, variable) {
    cl_group <- df %>% filter(Diagnosis_Binary == 0) %>% pull({{ variable }}) %>% na.omit()
    ess_group <- df %>% filter(Diagnosis_Binary == 1) %>% pull({{ variable }}) %>% na.omit()
    
    if (length(cl_group) < 2 || length(ess_group) < 2) {
      p_value <- NA
    } else {
      p_value <- tryCatch(
        t.test(cl_group, ess_group)$p.value,
        error = function(e) wilcox.test(cl_group, ess_group)$p.value
      )
    }
    
    # Format as mean ± SD
    cl_label <- paste0(sprintf("%.3f", mean(cl_group)), "±", sprintf("%.3f", sd(cl_group)))
    ess_label <- paste0(sprintf("%.3f", mean(ess_group)), "±", sprintf("%.3f", sd(ess_group)))
    
    tibble(
      CL = cl_label,
      ESS = ess_label,
      P_value = format_p_value(p_value)
    )
  }
  
  # Calculate statistics for each feature
  margin_stats <- calc_categorical(data, Margin)
  cystic_stats <- calc_categorical(data, CysticAreaT2)
  adc_stats <- calc_continuous(data, meanADC)
  
  # Combine results
  results <- bind_rows(cystic_stats, margin_stats, adc_stats) %>%
    mutate(
      Characteristic = c("Cystic change", "Margin", "meanADC"),
      .before = 1
    )
  
  # Get sample sizes
  sample_sizes <- data %>%
    count(Diagnosis_Binary) %>%
    pivot_wider(names_from = Diagnosis_Binary, values_from = n, names_prefix = "n_")
  
  list(
    result = results,
    cl_n = sample_sizes$n_0,
    ess_n = sample_sizes$n_1
  )
}

# ===================================================================
# Main Analysis
# ===================================================================
print("========== Loading Data ==========")

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

# Load training and external validation data
training_data <- read_excel("final_data_1.5.xlsx") %>% preprocess_data()
external_data <- read_excel("external_validation_data.xlsx") %>% preprocess_data()

print("========== Analyzing Training Set ==========")
training_analysis <- analyze_dataset(training_data)

print("========== Analyzing External Validation Set ==========")
external_analysis <- analyze_dataset(external_data)

# Combine results
final_table_data <- training_analysis$result %>%
  rename(Training_CL = CL, Training_ESS = ESS, Training_P = P_value) %>%
  left_join(
    external_analysis$result %>%
      rename(External_CL = CL, External_ESS = ESS, External_P = P_value),
    by = "Characteristic"
  )

# ===================================================================
# Create and Format Table
# ===================================================================
print("========== Creating Formatted Table ==========")

border_bold <- fp_border(color = "black", width = 1.5)

ft <- flextable(final_table_data) %>%
  
  # Set English headers
  set_header_labels(
    Characteristic = "Characteristic",
    Training_CL  = "CL", Training_ESS = "ESS", Training_P  = "P-value",
    External_CL  = "CL", External_ESS = "ESS", External_P = "P-value"
  ) %>%
  add_header_row(
    values = c("", "Training Set", "External Validation Set"),
    colwidths = c(1, 3, 3)
  ) %>%
  
  # Set caption
  set_caption(
    caption = "Table. Comparison of Significant Features Between CL and ESS in the Training and Validation Cohorts"
  ) %>%
  
  # Add footer notes
  add_footer_lines(
    values = c(
      "Data are presented as n (%) or mean ± standard deviation (SD). P-values were calculated using the Chi-square test (or Fisher's exact test) for categorical variables and the Mann-Whitney U test for continuous variables. Abbreviations: ADC, apparent diffusion coefficient; CL, cellular leiomyoma; ESS, endometrial stromal sarcoma; SD, standard deviation."
    )
  ) %>%
  
  # Layout and alignment
  width(j = 1, width = 1.5) %>%
  width(j = 2:7, width = 1.0) %>%
  align(align = "center", part = "all") %>%
  align(j = 1, align = "left", part = "body") %>%
  
  # Borders (three-line table style)
  border_remove() %>%
  hline_top(border = border_bold, part = "header") %>%
  hline_bottom(border = border_bold, part = "header") %>%
  hline_bottom(border = border_bold, part = "body") %>%
  
  # Font and formatting
  bold(part = "header") %>%
  compose(i = 2, j = 2, part = "header", 
          value = as_paragraph("CL (n=", training_analysis$cl_n, ")")) %>%
  compose(i = 2, j = 3, part = "header", 
          value = as_paragraph("ESS (n=", training_analysis$ess_n, ")")) %>%
  compose(i = 2, j = 5, part = "header", 
          value = as_paragraph("CL (n=", external_analysis$cl_n, ")")) %>%
  compose(i = 2, j = 6, part = "header", 
          value = as_paragraph("ESS (n=", external_analysis$ess_n, ")"))

# Display table
print(ft)

# Save as Word document
save_as_docx(ft, path = file.path(output_dir, "Independent_Predictors_Table.docx"))

print("✓ Table saved successfully as: Independent_Predictors_Table.docx")
print("========== Feature Comparison Analysis Complete ==========")
