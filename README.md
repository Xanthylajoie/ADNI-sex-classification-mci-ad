# Sex-specific cognitive predictors of conversion from MCI to Alzheimer’s disease

This repository contains the code used for the analyses presented in the manuscript:

**"Sex-specific cognitive predictors of conversion from mild cognitive impairment to Alzheimer’s disease"**

---

## 📌 Overview

Mild cognitive impairment (MCI) is a prodromal stage of Alzheimer’s disease (AD), but not all individuals with MCI progress to dementia. This project aims to:

- Identify baseline neuropsychological and sociodemographic predictors of conversion from MCI to AD
- Evaluate the predictive performance of machine learning models using cognitive and demographic data
- Examine **sex-specific differences** in predictive features and model performance

Using data from the Alzheimer's Disease Neuroimaging Initiative (ADNI), we compare MCI converters and non-converters and implement classification models to predict disease progression.

---

## 📊 Dataset

Data were obtained from the **Alzheimer’s Disease Neuroimaging Initiative (ADNI)** database.

- Website: https://adni.loni.usc.edu
- Access requires registration and approval

⚠️ **Note:** Due to data usage agreements, ADNI data are not included in this repository.

---

## 🧠 Methods Summary

### Statistical analyses
- ANCOVAs controlling for:
  - Age
  - Education
  - Race
- Comparison of baseline cognitive performance between:
  - MCI converters
  - MCI non-converters
- Analysis of **sex × conversion interactions**

### Machine Learning
- Model: Linear Support Vector Classifier (LSVC)
- Features:
  - Neuropsychological test scores (22 variables)
  - Composite cognitive scores (memory, executive, language, visuospatial)
  - Demographic variables (age, sex, education, race)

### Model setup
- Train/test split: 80% / 20% (stratified)
- Cross-validation: 10-fold (training set)
- Preprocessing: z-score normalization

### Evaluation metrics
- Accuracy
- Sensitivity
- Specificity
- AUC (ROC)
- Confusion matrices

### Additional analyses
- Feature importance (model coefficients)
- Sex-specific models (men vs women)

---


