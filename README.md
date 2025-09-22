# Health Data Science Repository

This repository contains a collection of R scripts and analyses for various health data science projects, focusing on genomics, statistical hypothesis testing, predictive modeling, and population health behavior analysis.

## 📁 Project Structure

### 1. Genomics Processing
Located in `genomics processing/`

Scripts for genomic data analysis and gene classification:
- **`labelgenes.R`** - Labels genes with essentiality classes (lethal vs viable)
- **`getgenes.R`** - Gene data extraction and processing
- **`create_train_test_sets.R`** - Dataset splitting for machine learning
- **`check_duplicate.R`** - Data quality control for duplicate detection
- **`man_whitney_feature_rank.R`** - Mann-Whitney U test for feature ranking
- **`merge_files.R`** - Data consolidation utilities
- **`merge_known_unknown_genes.R`** - Combines known and unknown gene datasets
- **`plot_genes_on_chromosomes.R`** - Genomic visualization tools

### 2. Hypothesis Testing
Located in `hypothesis testing/`

Statistical analysis tools for biomedical research:
- **`fisher_test_chisquared_test_cramer_V.R`** - Statistical tests for categorical data analysis
- **Research Documentation**: "Testing Association of Electrical Impedance Spectroscopy Features and Metabolite Features in Preterm Birth Prediction" (PDF/DOCX)

### 3. Preterm Birth Prediction Models
Located in `logistic_regression_random_forest_with_confidence_intervals_ROC_AUC for Preterm Birth Prediction/`

Machine learning models for predicting preterm birth using Electrical Impedance Spectroscopy (EIS):
- **`logreg.R`** - Logistic regression with p-value analysis and AUC evaluation
- **`logreg2.R`** - Advanced logistic regression implementation
- **`rf.R`** - Random Forest classification models
- **`ci.R`** - Confidence interval calculations for model performance metrics

### 4. Smoking Behavior Analysis
Located in `smoking behaviour analysis of England population using datasets Health Survey for England (HSE) and Smoking Toolkit Studies (STS)/`

Comprehensive analysis of smoking patterns in England using national survey data.

#### Data Exploration and Transformation
- **`dataexplore.R`** - Initial data exploration and summary statistics
- **`hse_explore.R`** - Health Survey for England data analysis
- **`hseclean.R`** - HSE data cleaning and preprocessing
- **`STS explore.R`** - Smoking Toolkit Studies data exploration
- **`COMB_variables_of_HSE.R`** - HSE variable combination and feature engineering
- **`sts_comb_variables.R`** - STS variable combination
- **`match_sts_hse.R`** - Data matching between HSE and STS datasets
- **`e-cig.R`** - E-cigarette usage analysis
- **`missing_data.R`** - Missing data analysis and handling
- **`utilities.R`** - Helper functions and utilities
- **`fs_model.R`** - Feature selection modeling
- **`mifr.R`** - Multiple imputation and analysis
- **`nb.R`** - Naive Bayes classification

#### E-cigarette Diffusion Analysis
- **`0_bass_functions.R`** - Bass diffusion model functions
- **`1_ecigarettes_descriptives.R`** - Descriptive analysis of e-cigarette adoption
- **`2_ecigarettes_fitbass.R`** - Bass model fitting for e-cigarette diffusion
- **`3_ecigarettes_spline.R`** - Spline modeling for trend analysis
- **`3_example_predict_bass.R`** - Bass model prediction examples

#### Synthetic Population Generation
- **`create_synthetic_population_functions.R`** - Functions for synthetic data generation
- **`create_synthetic_population.R`** - Main synthetic population creation script
- **`create_synthetic_population2.R`** - Alternative synthetic population implementation

## 🔬 Research Areas

### Genomics and Bioinformatics
- Essential gene prediction and classification
- Genomic feature analysis and ranking
- Chromosome-level gene visualization

### Predictive Healthcare Analytics
- Preterm birth prediction using biomarker data
- Electrical Impedance Spectroscopy (EIS) feature analysis
- Metabolite feature association studies

### Population Health and Epidemiology
- National smoking behavior trends analysis
- E-cigarette adoption and diffusion modeling
- Health survey data integration and analysis

### Statistical Methods and Machine Learning
- Logistic regression with confidence intervals
- Random Forest classification
- Hypothesis testing (Fisher's exact, Chi-squared, Mann-Whitney U)
- ROC curve analysis and AUC evaluation
- Bass diffusion modeling
- Synthetic population generation

## 🛠️ Technologies and Libraries

- **R Programming Language**
- **Statistical Packages**: `aod`, `pROC`, `caret`
- **Data Manipulation**: `tidyverse`, `haven`, `lubridate`
- **Visualization**: `scales`, `ggplot2`
- **Network Analysis**: `netdiffuseR`
- **Machine Learning**: Random Forest, Logistic Regression, Naive Bayes

## 📊 Key Features

- **Reproducible Research**: Well-documented R scripts with clear function definitions
- **Statistical Rigor**: Implementation of proper statistical tests and confidence intervals
- **Data Quality**: Comprehensive data cleaning and validation procedures
- **Visualization**: Genomic and statistical plotting capabilities
- **Modeling**: Multiple machine learning approaches for different prediction tasks

## 🚀 Getting Started

1. **Prerequisites**: Ensure R is installed with required packages
2. **Data Setup**: Configure data paths in scripts according to your local setup
3. **Execution**: Run scripts in appropriate order (preprocessing → analysis → modeling)
4. **Results**: Output files and models will be generated in specified directories

## 📈 Research Impact

This repository supports research in:
- Precision medicine and personalized healthcare
- Public health policy and smoking cessation strategies
- Genomic medicine and essential gene discovery
- Predictive modeling in obstetrics and maternal health

## 📄 License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## 🤝 Contributing

This repository represents ongoing health data science research. For collaboration or questions, please refer to the documentation within individual project folders.

---

*Repository maintained for health data science research and analysis*