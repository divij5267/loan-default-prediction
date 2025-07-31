# Credit Risk Modeling: Loan Default Prediction

This project implements a credit risk modeling pipeline to predict loan default using a public LendingClub dataset from Kaggle. The workflow includes data cleaning, feature selection, and model building using Random Forest, Lasso Logistic Regression, and CART, with cross-validation and profit analysis.

## Table of Contents
- [Project Overview](#project-overview)
- [Dataset](#dataset)
- [Project Structure](#project-structure)
- [Requirements](#requirements)
- [Usage](#usage)
- [Results](#results)
- [License](#license)

## Project Overview
The goal is to predict whether a loan will default based on borrower and loan characteristics. The project covers:
- Data preprocessing and cleaning
- Feature selection (correlation, VIF, Lasso)
- Model training (Random Forest, Lasso Logistic Regression, CART)
- Model evaluation (cross-validation, ROC, confusion matrix)
- Profit analysis for lending decisions

## Dataset
- **Source:** [LendingClub Loan Data (Kaggle)](https://www.kaggle.com/datasets/wordsforthewise/lending-club)
- **File:** `accepted_2007_to_2018Q4.csv`
- **Note:** Due to size, download the dataset manually from Kaggle and place it in the `data/` folder.

## Project Structure
```
loan-default-prediction/
├── data/
│   └── accepted_2007_to_2018Q4.csv   # Download from Kaggle
├── src/
│   └── loan_default_model.R          # Main R script
├── results/
│   └── final_profits.csv             # Output (optional)
├── .gitignore
├── LICENSE
└── README.md
```

## Requirements
- R (>= 4.0.0)
- R packages:
  - car
  - corrplot
  - ggplot2
  - lattice
  - caret
  - randomForest
  - pROC
  - glmnet
  - rpart
  - rpart.plot

Install all required packages in R:
```r
install.packages(c("car", "corrplot", "ggplot2", "lattice", "caret", "randomForest", "pROC", "glmnet", "rpart", "rpart.plot"))
```

## Usage
1. **Download the dataset** from Kaggle and place it in the `data/` directory.
2. **Run the main script:**
   ```r
   source("src/loan_default_model.R")
   ```
   This will preprocess the data, train models, evaluate performance, and output results.

## Results
- Model performance metrics (accuracy, ROC, AUC, confusion matrix) are printed in the R console.
- Feature importance and profit analysis are visualized and exported to `results/final_profits.csv`.

## License
This project is licensed under the MIT License. See [LICENSE](LICENSE) for details.