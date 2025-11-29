# Data Configuration Guide

## Before Running the Scripts

**IMPORTANT**: Before running any analysis scripts, you must configure the data directory path.

### Quick Setup

1. Open any R script you want to run
2. Find the **USER CONFIGURATION** section at the top
3. Change the `data_dir` path to your actual data location

### Example Configuration

```r
# ===================================================================
# USER CONFIGURATION - Please modify the path below
# ===================================================================
# Set this to the directory containing your data files
data_dir <- "/path/to/your/data"  # CHANGE THIS to your data directory
setwd(data_dir)

# You can also set output directory (optional, defaults to current directory)
output_dir <- getwd()  # CHANGE THIS if you want outputs in a different location
```

### Required Data Files

Place the following Excel files in your `data_dir`:

#### For Main Analysis:
- `final_data_1.5.xlsx` - Training dataset
- `external_validation_data.xlsx` - External validation dataset

#### For Supplementary Analysis:
- `Training Set Consistency Analysis 1.xlsx` - Reader 1 measurements
- `Training Set Consistency Analysis 2.xlsx` - Reader 2 measurements
- `MR参数.xlsx` - MRI parameters data

### File Path Examples

**Windows:**
```r
data_dir <- "C:/Users/YourName/Documents/ESS_CL_Data"
```

**macOS/Linux:**
```r
data_dir <- "/Users/YourName/Documents/ESS_CL_Data"
```

**Network Drive:**
```r
data_dir <- "//server/shared/ESS_CL_Data"
```

### Running Scripts in Order

1. **Data Analysis** (scripts/01_data_analysis/)
2. **Model Validation** (scripts/02_model_validation/)
3. **Supplementary Analysis** (scripts/03_supplementary_analysis/)

See main README.md for detailed workflow.

---

**Note:** The default path in scripts (`/Users/apple/Desktop/间质肉瘤excel数据`) is the author's local path and **must be changed** to your own data directory.
