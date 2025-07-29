# Predicting Medical Expenses with Regression Techniques

## Objective
To predict individual healthcare costs based on demographic and lifestyle data such as age, BMI, number of children, and smoking status.

## Key Questions
- What factors most influence medical expenses?
- Can we predict who is likely to be a smoker?
- How can we improve predictions by addressing multicollinearity?

## Methods Used
- Multiple Linear Regression
- Ridge & Lasso Regression
- Best Subset Selection
- Principal Components Regression (PCR)
- Partial Least Squares Regression (PLSR)
- Regression Trees, Bagging, Boosting, and BART
- Logistic Regression, LDA, QDA, and Naive Bayes


## Key Findings
- Smoking was the strongest predictor of high medical expenses.
- Ridge regression and random forests outperformed standard linear models in predictive accuracy.
- Classification models such as LDA and QDA predicted smoker status with ~80% accuracy.


## Tools & Libraries

This project was built entirely in R using the following libraries:

- `glmnet` – for Ridge and Lasso regression  
- `leaps` – for best subset and stepwise selection  
- `tree` – for regression tree modeling and pruning  
- `randomForest` – for bagging and assessing variable importance  
- `gbm` – for gradient boosting models  
- `BART` – for Bayesian Additive Regression Trees  
- `pls` – for Principal Component Regression (PCR) and Partial Least Squares (PLSR)  
- `MASS` – for Linear Discriminant Analysis (LDA) and Quadratic Discriminant Analysis (QDA)  
- `e1071` – for Naive Bayes classification  
- `base R` – for data manipulation, regression modeling, plotting, and evaluation  
- `RMarkdown` – for generating the final report and visual outputs

## References
- ISLR by James, Witten, Hastie & Tibshirani
- Dataset: [Kaggle – Medical Cost Personal Dataset](https://www.kaggle.com/datasets/mirichoi0218/insurance)
