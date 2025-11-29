# ===================================================================
# Part 1: Preparation (Load Packages, Set Paths, Read Data)
# ===================================================================

# 1. Load all required R packages
library(readxl)
library(tidyverse)
library(gtsummary)
library(car)
library(pROC)
library(rms)
library(gt)
library(officer)

# ===================================================================
# USER CONFIGURATION - Please modify the path below
# ===================================================================
# Set this to the directory containing your data files
data_dir <- "/Users/apple/Desktop/间质肉瘤excel数据"  # CHANGE THIS to your data directory
setwd(data_dir)

# You can also set output directory (optional, defaults to current directory)
output_dir <- getwd()  # CHANGE THIS if you want outputs in a different location

# 3. Read your Excel file
my_data <- read_excel("final_data_1.5.xlsx")


# ===================================================================
# Part 2: Data Cleaning and Transformation (Key Steps)
# ===================================================================

# Use mutate() to complete all variable type conversions at once
final_data <- my_data %>%
  mutate(
    # --- Convert numeric variables from text to numeric ---
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
    
    # --- Use existing THREE-LEVEL diagnosis variable from Excel ---
    # Diagnosis_3_Groups: 1 = CL, 2 = ESS_Low, 3 = ESS_High
    Diagnosis_3Level = factor(Diagnosis_3_Groups, 
                              levels = c("1", "2", "3"), 
                              labels = c("CL", "ESS_Low", "ESS_High")),
    
    # --- Convert the original outcome variable to binary factor ---
    Diagnosis_Binary = factor(Diagnosis_Binary, 
                              levels = c("0", "1"), 
                              labels = c("CL", "ESS")),
    
    # --- Convert all 0/1 binary variables to factors ---
    MRI_T2_HigherThanMyo = factor(MRI_T2_HigherThanMyo, levels = c("0", "1"), labels = c("No", "Yes")),
    DWI_HighSignal = factor(DWI_HighSignal, levels = c("0", "1"), labels = c("No", "Yes")),
    Hemorrhage = factor(Hemorrhage, levels = c("0", "1"), labels = c("No", "Yes")),
    CysticAreaT2 = factor(CysticAreaT2, levels = c("0", "1"), labels = c("No", "Yes")),
    NonEnhancingCenter = factor(NonEnhancingCenter, levels = c("0", "1"), labels = c("No", "Yes")),
    Margin = factor(Margin, levels = c("0", "1"), labels = c("Regular", "Irregular")),
    
    # --- Convert multi-level categorical variables to factors ---
    `Menopausal Status` = factor(`Menopausal Status`,
                                 levels = c("1", "2", "3"),
                                 labels = c("Premenopausal", "Perimenopausal", "Postmenopausal"))
  )

# Check the structure of the transformed data
print("--- Structure of Cleaned and Transformed Data ---")
str(final_data)


# ===================================================================
# Part 3: Descriptive Statistics (Generate "Table 1")
# ===================================================================
library(flextable)

# 1) Table 1: Ten ADC metrics
library(dplyr)
library(purrr)
library(gtsummary)

# -------------------------
# Configuration
# -------------------------
adc_vars <- c("minADC", "meanADC", "maxADC",
              "P10", "P25", "P50", "P75", "P90",
              "skewness", "kurtosis")

grp_var <- "Diagnosis_Binary"
grp_cl  <- "CL"
grp_ess <- "ESS"

# -------------------------
# 1) Perform Normality Test by Group (Shapiro–Wilk)
#    If both groups have p >= 0.05 => considered "normal"
# -------------------------
is_normal_in_both <- function(var, data, gvar, g1, g2) {
  x1 <- na.omit(data[data[[gvar]] == g1, var, drop = TRUE])
  x2 <- na.omit(data[data[[gvar]] == g2, var, drop = TRUE])
  # Shapiro–Wilk requires at least 3 distinct values to be meaningful
  ok1 <- length(unique(x1)) >= 3
  ok2 <- length(unique(x2)) >= 3
  p1 <- if (ok1) shapiro.test(x1)$p.value else NA_real_
  p2 <- if (ok2) shapiro.test(x2)$p.value else NA_real_
  isTRUE(p1 >= 0.05 & p2 >= 0.05)
}

normal_vars   <- keep(adc_vars, ~ is_normal_in_both(.x, final_data, grp_var, grp_cl, grp_ess))
nonnormal_vars <- setdiff(adc_vars, normal_vars)

# -------------------------
# 2) Assemble test list for add_p
#    Normal -> t.test (Welch)
#    Non-normal -> wilcox.test
# -------------------------
test_list <- list()
if (length(normal_vars))   test_list <- c(test_list, setNames(as.list(rep("t.test",     length(normal_vars))),   normal_vars))
if (length(nonnormal_vars)) test_list <- c(test_list, setNames(as.list(rep("wilcox.test", length(nonnormal_vars))), nonnormal_vars))

# -------------------------
# 3) Statistics Display:
#    Default: median (IQR)
#    Override for normal variables: mean ± SD
# -------------------------
stat_list <- list(
  all_continuous() ~ "{median} ({p25}, {p75})"
)
if (length(normal_vars)) {
  stat_list <- c(stat_list, list(all_of(normal_vars) ~ "{mean} ± {sd}"))
}

# -------------------------
# 4) Generate Table
# -------------------------
table1_adc <- final_data %>%
  select(all_of(c(grp_var, adc_vars))) %>%
  tbl_summary(
    by = !!grp_var,
    type = all_continuous() ~ "continuous",
    statistic = stat_list,
    label = list(
      minADC   ~ "Minimum ADC (×10⁻³ mm²/s)",
      meanADC  ~ "Mean ADC (×10⁻³ mm²/s)",
      maxADC   ~ "Maximum ADC (×10⁻³ mm²/s)",
      P10      ~ "ADC 10th Percentile (×10⁻³ mm²/s)",
      P25      ~ "ADC 25th Percentile (×10⁻³ mm²/s)",
      P50      ~ "ADC 50th Percentile (Median) (×10⁻³ mm²/s)",
      P75      ~ "ADC 75th Percentile (×10⁻³ mm²/s)",
      P90      ~ "ADC 90th Percentile (×10⁻³ mm²/s)",
      skewness ~ "ADC Skewness",
      kurtosis ~ "ADC Kurtosis"
    ),
    missing = "no"
  ) %>%
  add_p(test = test_list) %>%             # 按变量使用 t 或 Wilcoxon
  add_overall() %>%
  bold_p(t = 0.05) %>%
  bold_labels() %>%
  modify_header(
    label ~ "**Characteristic**",
    p.value ~ "**P value**"
  ) %>%
  modify_footnote(
    all_stat_cols() ~ "a ADC metrics are summarized as mean ± SD when both groups are normally distributed (Shapiro–Wilk p ≥ 0.05), otherwise as median (IQR).

b P values: Welch’s t-test for variables normal in both groups; Wilcoxon rank-sum test otherwise. Normality assessed by Shapiro–Wilk within groups (CL n = 70; ESS n = 41).

Abbreviations: ADC, apparent diffusion coefficient; CL, cellular leiomyoma; ESS, endometrial stromal sarcoma; IQR, interquartile range.

Bold values indicate statistical significance (p < 0.05)."
  ) %>%
  modify_caption("**Table 1. ADC Histogram Metrics for Differentiating Cellular Leiomyoma and Endometrial Stromal Sarcoma **")


# 2) Table 2: Clinical + Qualitative MRI Features (Three groups: CL, ESS_Low, ESS_High)
# Note: Variable names with spaces or special characters require backticks; ensure these column names match the data
clinical_mri_vars <- c("age", "Menopausal Status", "TumorSize_cm", "CA125",
                       "Margin", "MRI_T2_HigherThanMyo", "DWI_HighSignal",
                       "Hemorrhage", "CysticAreaT2", "NonEnhancingCenter")

table2_clin_mri <- final_data %>%
  select(all_of(c("Diagnosis_3Level", clinical_mri_vars))) %>%
  tbl_summary(
    by = Diagnosis_3Level,
    type = list(
      age ~ "continuous",
      TumorSize_cm ~ "continuous",
      CA125 ~ "continuous"
    ),
    statistic = list(
      all_continuous() ~ "{mean} ± {sd}",
      all_categorical() ~ "{n} ({p}%)"
    ),
    label = list(
      age ~ "Age (years)",
      `Menopausal Status` ~ "Menopausal Status",
      TumorSize_cm ~ "Tumor Size (cm)",
      CA125 ~ "CA125 (U/mL)",
      Margin ~ "Margin",
      MRI_T2_HigherThanMyo ~ "T2 High Signal",
      DWI_HighSignal ~ "DWI High Signal",
      Hemorrhage ~ "Hemorrhage",
      CysticAreaT2 ~ "Cystic Area on T2WI",
      NonEnhancingCenter ~ "Central Non-enhancing Area"
    ),
    missing = "no"
  ) %>%
  add_p(test = list(
    # Continuous variables - use ANOVA or Kruskal-Wallis for three-group comparison
    age ~ "aov",
    TumorSize_cm ~ "kruskal.test",
    CA125 ~ "kruskal.test",
    # Categorical variables - use Fisher's exact test or chi-square test
    `Menopausal Status` ~ "fisher.test",
    Hemorrhage ~ "fisher.test",
    # Other categorical variables - use chi-square test
    Margin ~ "chisq.test",
    MRI_T2_HigherThanMyo ~ "chisq.test",
    DWI_HighSignal ~ "chisq.test",
    CysticAreaT2 ~ "chisq.test",
    NonEnhancingCenter ~ "chisq.test"
  )) %>%
  add_overall() %>%
  bold_p(t = 0.05) %>%
  bold_labels() %>%
  modify_header(
    label ~ "**Characteristic**",
    p.value ~ "**P value**"
  ) %>%
  modify_footnote(
    all_stat_cols() ~ "<sup>a</sup> Continuous variables are presented as mean ± SD; categorical variables are n (%).
<sup>b</sup> P values: ANOVA for age; Kruskal-Wallis test for Tumor Size and CA125; Fisher's exact test or chi-square test for categorical variables.
Abbreviations: CL, cellular leiomyoma; ESS_Low, low-grade endometrial stromal sarcoma; ESS_High, high-grade endometrial stromal sarcoma; SD, standard deviation.
Bold values indicate statistical significance (p < 0.05)."
  )%>%
  modify_caption("**Table 2. Baseline Clinical Characteristics and Qualitative MRI Features by Diagnosis Group**")

# =============== Convert to flextable and set line spacing & padding =================
ft_adc <- table1_adc %>%
  as_flex_table() %>%
  fontsize(size = 10, part = "all") %>%        # Slightly reduce font size
  autofit() %>%             # Auto-fit column widths
  width(j = 1, width = 3) %>%   # For example, first column width 3 cm, can be adjusted
  line_spacing(space = 1, part = "all") %>%  # Tighter line spacing
  padding(padding.top = 1, padding.bottom = 1,
          padding.left = 1, padding.right = 1,
          part = "all")

ft_clin_mri <- table2_clin_mri %>%
  as_flex_table() %>%
  line_spacing(space = 1, part = "all") %>%
  padding(i = NULL, j = NULL,
          padding.top = 0, padding.bottom = 0,
          padding.left = 2, padding.right = 2,
          part = "all")

# =============== Export to Word =================
# 1) Export two files separately
save_as_docx(ft_adc,      path = file.path(output_dir, "Table1_ADC_Metrics.docx"))
save_as_docx(ft_clin_mri, path = file.path(output_dir, "Table2_Clinical_MRI.docx"))

# ===================================================================
# Part 4: Univariate Regression Analysis (Screen Candidate Predictors) - Using Binary Classification
# ===================================================================
print("--- Performing univariate regression analysis ---")
univariate_table <- final_data %>%
  # Exclude three-level classification variables, retain only variables for analysis
  select(-Diagnosis_3_Groups, -Diagnosis_3Level) %>%
  tbl_uvregression(
    method = glm,
    y = Diagnosis_Binary,
    method.args = list(family = binomial),
    exponentiate = TRUE,
    pvalue_fun = ~ style_pvalue(.x, digits = 3)
  ) %>%
  # Customize column headers
  modify_header(
    label = "**Characteristic**",
    estimate = "**OR (95% CI)**",
    p.value = "**P**"
  ) %>%
  # Merge OR values and confidence intervals
  modify_column_merge(
    pattern = "{estimate} ({conf.low}, {conf.high})",
    rows = !is.na(estimate)
  ) %>%
  # Hide original confidence interval columns
  modify_column_hide(c(conf.low, conf.high)) %>%
  # Add sample size
  add_n() %>%
  # Highlight significant results
  bold_p(t = 0.05) %>%
  # Bold variable labels
  bold_labels() %>%
  # Add table caption
  modify_caption("**Univariate Logistic Regression Analysis Results**") %>%
  # Add footnote
  modify_footnote(
    estimate ~ "OR: Odds Ratio; CI: Confidence Interval",
    p.value ~ "Wald test was used"
  )

# Display results
print(univariate_table)


# ===================================================================
# Part 5: Multivariate Regression Analysis (Screen Final Independent Predictors)
# ===================================================================
print("--- Performing multivariate regression analysis ---")

# --- 5.1: Define Candidate Variables ---
# !! KEY STEP !!
# Based on Part 4 univariate analysis results, fill in all variable names with P < 0.1 (or your criterion) in the list below
# Note: Use variable names without backticks for VIF testing
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

# --- 5.2: Multicollinearity Test ---
formula_for_vif <- as.formula(paste("Diagnosis_Binary ~", paste(candidate_vars_for_vif, collapse = " + ")))
vif_model <- glm(formula_for_vif, data = final_data, family = binomial, na.action = na.omit)

# Check VIF values
if(length(coef(vif_model)) > 1) {  # Ensure model has coefficients
  vif_values <- vif(vif_model)
  print("--- VIF Values for Candidate Variables ---")
  print(vif_values)
  
  # Identify high collinearity variables (VIF > 5)
  high_vif <- names(vif_values)[vif_values > 5]
  if(length(high_vif) > 0) {
    print(paste("High collinearity variables detected:", paste(high_vif, collapse = ", ")))
  }
} else {
  print("VIF model fitting failed, please check variable settings")
}

# --- 5.3: Simplified Variable List (Remove High Collinearity Variables) ---
# Based on VIF results, retain representative variables, using original variable names (without backticks)
candidate_vars_simplified <- c(
  "Margin",
  "Hemorrhage",
  "CysticAreaT2",
  "NonEnhancingCenter",
  "meanADC" ,
  "kurtosis"  # Only retain meanADC as representative of ADC histogram
)

# --- 5.4: Create Clean Dataset for Modeling ---
# Check if these variables exist in the data
print("--- Checking if candidate variables exist in the data ---")
existing_vars <- candidate_vars_simplified[candidate_vars_simplified %in% names(final_data)]
missing_vars <- candidate_vars_simplified[!candidate_vars_simplified %in% names(final_data)]

if(length(missing_vars) > 0) {
  print(paste("Missing variables:", paste(missing_vars, collapse = ", ")))
  print("Actual column names in the data:")
  print(names(final_data))
}

print(paste("Existing variables:", paste(existing_vars, collapse = ", ")))

# Select only existing variables
data_for_model <- final_data %>%
  select(Diagnosis_Binary, all_of(existing_vars)) %>%
  na.omit() 

# Check complete case count
print(paste("Complete cases for modeling:", nrow(data_for_model)))

# Check case count by group
print("--- Case Distribution by Group ---")
print(table(data_for_model$Diagnosis_Binary))

# --- 5.5: Stepwise Regression Selection ---
if(nrow(data_for_model) > 0 && length(candidate_vars_simplified) > 0) {
  # Build full model
  full_model_for_step <- glm(Diagnosis_Binary ~ ., 
                             data = data_for_model, 
                             family = binomial)
  
  # Run stepwise regression
  stepwise_model <- step(full_model_for_step, direction = "both", trace = FALSE)
  
  print("--- Final Model Selected by Stepwise Regression ---")
  summary(stepwise_model)
  
  # --- 5.6: Format Final Model Results Table ---
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
  print("Data preparation failed, please check variable settings and data completeness")
}


tbl <- tbl_merge(
  tbls = list(
    univariate_table %>% 
      modify_table_body(~.x %>% select(-stat_n)),  # 移除N列
    final_model_table
  ),
  tab_spanner = c("**Univariate Analyses**", "**Multivariate Analyses**")
) %>%
  modify_caption("**Univariate and Multivariate Logistic Regression Analyses**")

# Display and format (three-line table style)
flex_tbl <- tbl %>%
  as_flex_table() %>%
  border_remove() %>%  
  font(fontname = "Times New Roman", part = "all") %>%
  fontsize(size = 11, part = "all") %>%
  fontsize(size = 12, part = "header") %>%
  bold(part = "header") %>%
  align(align = "center", part = "header") %>%
  align(align = "left", j = 1, part = "body") %>%
  align(align = "center", j = -1, part = "body") %>%
  # Adjust line spacing
  line_spacing(space = 1, part = "all") %>%   # Smaller is tighter, 1 = single spacing
  # Adjust cell padding
  padding(i = NULL, j = NULL, padding.top = 0, padding.bottom = 0,
          padding.left = 2, padding.right = 2, part = "all") %>%
  # Add three-line table borders
  hline_top(part = "header", border = fp_border(color = "black", width = 2)) %>%
  hline_bottom(part = "header", border = fp_border(color = "black", width = 1)) %>%
  hline_bottom(part = "body", border = fp_border(color = "black", width = 2))

save_as_docx(flex_tbl, path = file.path(output_dir, "table.docx"))