# Differentiating Endometrial Stromal Sarcoma from Cellular Leiomyoma Based on a Nomogram Integrating Multimodal MRI and Clinical Data

This repository contains the R code for developing and validating a prediction model to differentiate endometrial stromal sarcoma (ESS) from cellular leiomyoma (CL) using MRI-based features.

## Project Overview

This study developed a logistic regression prediction model combining qualitative MRI features and ADC histogram metrics to distinguish ESS from CL. The model was validated using both internal 10-fold cross-validation and external validation from an independent institution.

## Repository Structure

```
ess-cl-mri-nomogram-github/
│
├── scripts/
│   ├── 01_data_analysis/
│   │   ├── 01_descriptive_statistics_and_regression.R  # Descriptive statistics and regression analysis
│   │   └── 02_flowchart.R                               # Study flowchart generation
│   │
│   ├── 02_model_validation/
│   │   ├── 01_model_construction_and_evaluation.R       # Model building and internal validation
│   │   ├── 02_external_validation.R                     # External validation analysis
│   │   └── 03_feature_comparison.R                      # Training vs validation feature comparison
│   │
│   └── 03_supplementary_analysis/
│       ├── 01_icc_analysis.R                            # Inter-reader agreement (ICC) analysis
│       ├── 02_inter_reader_comparison.R                 # ADC metrics comparison between readers
│       ├── 03_decision_curve_analysis.R                 # Decision curve analysis (DCA)
│       └── 04_mri_parameters_comparison.R               # MRI parameters comparison
│
├── web_tool/
│   ├── diagnosis_tool.html                              # Interactive web-based prediction tool
│   └── README.md                                        # Web tool documentation
│
├── docs/                                                # Additional documentation
├── LICENSE                                              # License information
└── README.md                                            # This file
```

## Analysis Workflow

### 1. Data Analysis (`01_data_analysis/`)

#### Descriptive Statistics and Regression (`01_descriptive_statistics_and_regression.R`)
- Data preprocessing and variable type conversion
- Descriptive statistics for ADC histogram metrics
- Baseline clinical characteristics and qualitative MRI features
- Univariate logistic regression analysis
- Multivariate logistic regression with stepwise selection
- Variance Inflation Factor (VIF) for multicollinearity check

**Output:**
- `Table1_ADC_Metrics.docx`: ADC histogram metrics summary
- `Table2_Clinical_MRI.docx`: Clinical and MRI characteristics
- `table.docx`: Combined univariate and multivariate regression results

#### Study Flowchart (`02_flowchart.R`)
- Generates study flowchart showing patient selection process
- Illustrates inclusion/exclusion criteria for both cohorts

**Output:**
- `flowchart.svg`: Vector format flowchart
- `flowchart.pdf`: PDF format flowchart

### 2. Model Validation (`02_model_validation/`)

#### Model Construction and Internal Validation (`01_model_construction_and_evaluation.R`)
- Multicollinearity assessment using VIF
- Stepwise logistic regression for variable selection
- 10-fold cross-validation
- ROC curve analysis with mean curve and confidence intervals
- Nomogram construction for clinical use
- Calibration curve assessment (Hosmer-Lemeshow test)
- Optimal diagnostic threshold determination

**Output:**
- `ROC_curve_10fold_CV.pdf`: ROC curve with 10-fold CV results
- `Nomogram_ESS_Prediction.pdf`: Clinical nomogram
- `Internal_Calibration_Plot.pdf`: Calibration curve

#### External Validation (`02_external_validation.R`)
- External dataset preprocessing
- Model performance on external cohort
- AUC calculation with 95% CI
- Sensitivity, specificity, PPV, NPV calculation
- External calibration assessment
- Internal vs external performance comparison
- ROC curve comparison plot
- DeLong test for AUC comparison

**Output:**
- `External_Calibration_Plot.pdf`: External validation calibration curve
- `ROC_Comparison.pdf`: Internal vs external ROC comparison
- `Final_Performance_Table.docx`: Performance metrics table
- `External_Validation_Report.txt`: Detailed validation report

#### Feature Comparison (`03_feature_comparison.R`)
- Compares significant features between training and validation sets
- Statistical testing for between-group differences

**Output:**
- `Independent_Predictors_Table.docx`: Feature comparison table

### 3. Supplementary Analysis (`03_supplementary_analysis/`)

#### ICC Analysis (`01_icc_analysis.R`)
- Inter-reader reliability assessment for ADC metrics
- Intraclass Correlation Coefficient (ICC) calculation
- Bland-Altman plots for all 10 ADC metrics
- Agreement statistics (bias, limits of agreement)

**Output:**
- `Bland_Altman_Plots.pdf`: Comprehensive Bland-Altman plots
- `Agreement_Statistics_Table.docx`: ICC and agreement statistics
- `icc_analysis_results.csv`: Detailed ICC results
- `merged_all_metrics_data.csv`: Combined reader measurements

#### Inter-reader Comparison (`02_inter_reader_comparison.R`)
- Compares ADC metrics between two readers stratified by diagnosis
- Statistical comparison (CL vs ESS) for each reader

**Output:**
- `ADC_Analysis_Table.docx`: Reader comparison table

#### Decision Curve Analysis (`03_decision_curve_analysis.R`)
- Clinical utility assessment of the prediction model
- Net benefit calculation across threshold probabilities
- Comparison of internal and external validation

**Output:**
- `DCA_Internal_Validation.pdf`: Internal validation DCA
- `DCA_Combined_Comparison.pdf`: Combined DCA comparison

#### MRI Parameters Comparison (`04_mri_parameters_comparison.R`)
- Compares MRI imaging parameters between cohorts
- Documents technical differences between training and validation sets

**Output:**
- `MRI_Parameters_Comparison.docx`: MRI parameters table

## Requirements

### R Version
- R >= 4.0.0

### Required R Packages

```r
# Data manipulation
install.packages(c("readxl", "tidyverse", "dplyr", "tidyr"))

# Statistical analysis
install.packages(c("psych", "car", "caret", "irr"))

# ROC and calibration
install.packages(c("pROC", "ResourceSelection", "CalibrationCurves"))

# Modeling
install.packages(c("rms", "gtsummary"))

# Visualization
install.packages(c("ggplot2", "gridExtra", "corrplot", "cowplot", "DiagrammeR", "DiagrammeRsvg", "rsvg"))

# Tables and output
install.packages(c("flextable", "officer", "kableExtra", "gt"))

# Decision curve analysis
install.packages("rmda")
```

## Getting Started

### ⚠️ Important: Data Configuration

**Before running any scripts**, you must configure your data directory path. See [DATA_SETUP.md](DATA_SETUP.md) for detailed instructions.

### Data Preparation

1. Prepare your training data in Excel format (`final_data_1.5.xlsx`)
2. Prepare external validation data (`external_validation_data.xlsx`)
3. For ICC analysis, prepare two reader datasets:
   - `Training Set Consistency Analysis 1.xlsx`
   - `Training Set Consistency Analysis 2.xlsx`
4. For MRI parameters comparison: `MR参数.xlsx`

**Note:** All data files should be placed in your configured `data_dir`. The scripts will read from and write outputs to this directory.

### Running the Analysis

The scripts should be run in the following order:

1. **Data Analysis**
   ```r
   source("scripts/01_data_analysis/01_descriptive_statistics_and_regression.R")
   source("scripts/01_data_analysis/02_flowchart.R")
   ```

2. **Model Construction and Validation**
   ```r
   source("scripts/02_model_validation/01_model_construction_and_evaluation.R")
   source("scripts/02_model_validation/02_external_validation.R")
   source("scripts/02_model_validation/03_feature_comparison.R")
   ```

3. **Supplementary Analysis** (can be run independently)
   ```r
   source("scripts/03_supplementary_analysis/01_icc_analysis.R")
   source("scripts/03_supplementary_analysis/02_inter_reader_comparison.R")
   source("scripts/03_supplementary_analysis/03_decision_curve_analysis.R")
   source("scripts/03_supplementary_analysis/04_mri_parameters_comparison.R")
   ```

### Important Notes

- Set your working directory appropriately before running scripts
- External validation scripts depend on objects created by model construction script
- DCA scripts require results from both internal and external validation
- Ensure all required data files are in the working directory

## Web-based Prediction Tool

An interactive web-based tool is provided in the `web_tool/` directory for clinical use. See `web_tool/README.md` for details.

## Key Features

### Final Prediction Model

The final model includes three independent predictors:
- **Margin** (Regular vs Irregular)
- **Cystic change on T2WI** (No vs Yes)
- **Mean ADC** (continuous variable)

### Model Performance

**Internal Validation (10-fold CV):**
- AUC: 0.819 (95% CI: 0.806-0.832)
- Sensitivity: 0.800
- Specificity: 0.886

**External Validation:**
- Results vary based on your dataset
- Calibration and discrimination metrics provided
- ROC comparison with internal validation

## Output Files

All analysis scripts generate multiple output files including:
- **PDF files**: Plots and figures (ROC curves, calibration curves, Bland-Altman plots)
- **Word documents (.docx)**: Formatted tables ready for publication
- **CSV files**: Raw data and detailed results
- **Text files**: Summary reports

## Citation

If you use this code for your research, please cite our paper:

To be updated after journal acceptance

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## Contact

For questions or issues, please open an issue on GitHub or contact:

Yue Zhou  
Email: 641613803@qq.com

## Acknowledgments

- Study conducted at Fudan University, Obstetrics & Gynecology Hospital
- IRB approval obtained from both institutions

## Version History

- **v1.0.0** (2025): Initial release with complete analysis pipeline

---

**Note:** This repository contains only the analysis code. Patient data is not included due to privacy considerations. The code is provided to ensure reproducibility and transparency of the research methods.
