# Interactive Diagnosis Tool

## Overview
This folder contains an interactive web-based diagnostic tool for differentiating Endometrial Stromal Sarcoma (ESS) from Cellular Leiomyoma (CL).

## Features
- **Real-time calculation**: Instant probability calculation based on MRI features
- **Interactive interface**: User-friendly design with visual feedback
- **Adjustable threshold**: Customizable diagnostic threshold (default: 0.477)
- **Offline capable**: Can be used without internet connection
- **Mobile responsive**: Works on desktop, tablet, and mobile devices

## How to Use

### Option 1: Direct Browser Access
1. Open `diagnosis_tool.html` in any modern web browser
2. Input the following parameters:
   - Lesion margin (Regular/Irregular)
   - Cystic area on T2WI (Yes/No)
   - Mean ADC value (×10⁻³ mm²/s)
3. Click "Calculate Diagnostic Probability"
4. View the diagnostic results and probability

### Option 2: Deploy on GitHub Pages
1. Push this repository to GitHub
2. Go to repository Settings → Pages
3. Select branch and folder to deploy
4. Your tool will be available at: `https://yourusername.github.io/repository-name/web_tool/diagnosis_tool.html`

### Option 3: Hospital Intranet Deployment
1. Copy `diagnosis_tool.html` to your hospital web server
2. Configure access permissions as needed
3. Can be integrated into PACS or EMR systems via iframe

## Model Information
- **Algorithm**: Logistic Regression
- **AUC**: 0.828
- **Optimal Threshold**: 0.477
- **Training Sample**: 111 cases
- **Model Equation**: 
  ```
  Logit(P) = 3.4640 + 1.7344×IrregularMargin + 1.6732×CysticAreaT2 - 4.7710×meanADC
  P(ESS) = e^Logit / (1 + e^Logit)
  ```

## Input Parameters
| Parameter | Type | Range | Description |
|-----------|------|-------|-------------|
| Margin | Categorical | Regular/Irregular | Lesion margin characteristics on MRI |
| Cystic Area | Categorical | Yes/No | Presence of cystic component on T2WI |
| Mean ADC | Continuous | 0.5-2.5 | Mean Apparent Diffusion Coefficient (×10⁻³ mm²/s) |

## Technical Details
- **Technology**: Pure HTML/CSS/JavaScript (no dependencies)
- **Browser Support**: Chrome, Firefox, Safari, Edge (latest versions)
- **File Size**: ~15 KB
- **Load Time**: Instant

## Important Notice
⚠️ This tool is for **clinical reference only** and cannot replace professional medical judgment. Final diagnosis requires pathological examination confirmation.

## Version
- Version: 1.0
- Last Updated: 2025-11-29
- Based on: Research study "MRI-based prediction model for ESS vs CL differentiation"
