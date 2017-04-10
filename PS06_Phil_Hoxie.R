# Load Packages ----------------------------------------------------------------
library(tidyverse)
library(broom)
library(plotly)
library(glmnet)
# Package for string manipulation:
library(stringr)





# Setup ------------------------------------------------------------------------
# Load training data
train <- read_csv("https://raw.githubusercontent.com/rudeboybert/MATH218/gh-pages/assets/PS/PS06/train.csv") %>% 
  # Variable names in R can't start with numbers, so rename:
  dplyr::rename(
    FirstFlrSF = `1stFlrSF`,
    SecondFlrSF = `2ndFlrSF`,
    ThreeSsnPorch = `3SsnPorch`
  ) %>% 
  # To keep things simple for now, select only variables that 1) are numerical 
  # (i.e. no categorical) 2) don't have any missing values in both the training
  # and test sets
  select(
    Id, MSSubClass, LotArea, OverallQual, OverallCond, YearBuilt, YearRemodAdd, 
    FirstFlrSF, SecondFlrSF, LowQualFinSF,
    FullBath, HalfBath, BedroomAbvGr, KitchenAbvGr, 
    TotRmsAbvGrd, Fireplaces, WoodDeckSF, OpenPorchSF, 
    EnclosedPorch, ThreeSsnPorch, ScreenPorch, PoolArea, MiscVal, MoSold, 
    YrSold, SalePrice
  )

# Create the model formula for lm(), ridge regression, and LASSO:
model_formula <- train %>% 
  # Take all predictor variable names and separate them with + signs:
  names() %>% 
  setdiff(c("Id", "fold", "SalePrice")) %>% 
  stringr::str_c(collapse=" + ") %>% 
  # Add outcome variable and ~ sign and convert to formula
  stringr::str_c("SalePrice ~ ", .)
model_formula
model_formula <- as.formula(model_formula)

# Used in glmnet() function for ridge regression and LASSO
X <- model.matrix(model_formula, data = train)[, -1]
y <- train$SalePrice

# Number of folds to use for all CV
n_folds <- 10





# 1. Obtain optimal lambda for ridge regression --------------------------------

# Your code should
# 1. Save the optimal lambda (tuning parameter) in lambda_star_ridge (as a
# scalar and not a data frame). You will be using this optimal lambda later
# 2. Compute the data frame coefficients that stores the values of the beta-
# coefficients for each value of lambda as seen in shrinkage.R from Lec16
# 3. And thus save a ggplot to PDF that shows the shrinking of each beta as 
# lambda varies AND has the optimal lambda marked with a vertical line

# Find the optimal lambda from these values. Feel free to toy with the from, to,
# and length values.
lambda_values <- 10^seq(from = 2, to = 4, length = 2500)

# FILL IN BELOW:

model_ridge <- glmnet(X, y, alpha = 0, lambda = lambda_values)
cvfit <- cv.glmnet(X, y, alpha = 0, lambda=lambda_values, nfolds = n_folds)

plot(cvfit)

cvfit %>%
  glance()
lambda_star_ridge <- cvfit %>%
  glance() %>%
  .[["lambda.min"]]
lambda_star_ridge

cv_results <- cvfit %>%
  tidy() %>%
  tbl_df() %>%
  mutate(log_lambda=log(lambda))
cv_results

## Coeffs

coefficients_ridge <- model_ridge %>%
  tidy() %>%
  tbl_df() %>%
  mutate(log_lambda = log(lambda)) %>% 
  select(-c(step, dev.ratio)) %>% 
  filter(term != "(Intercept)")


model_ridge %>%
    tidy() %>%
    tbl_df() %>%
    # lambda's on x-axis are better viewed on a log-scale:
    mutate(log_lambda = log(lambda)) %>%
    # We're not interested in the intercept estimate beta_0_hat
    filter(term != "(Intercept)") %>%
    ggplot(aes(x=log_lambda, y=estimate, col=term)) +
    geom_line()
ggplotly()


# Given code:
# Plot coefficients for each value of lambda, with optimal lambda marked
ridge_coefficients_plot <- ggplot(coefficients_ridge, aes(x=log_lambda, y=estimate, col=term)) +
  geom_line() +
  geom_vline(xintercept=log(lambda_star_ridge)) +
  labs(x="log(lambda)", y="Coefficient Estimate", title="Ridge Regression Coefficients")
ridge_coefficients_plot

# Save to PDF
ggsave(filename="PS06_Phil_Hoxie_ridge_coefficients.pdf", 
       ridge_coefficients_plot, width=11, height=8.5)


# 2. Obtain optimal lambda for LASSO -------------------------------------------

# Your code should
# 1. Save the optimal lambda (tuning parameter) in lambda_star_LASSO (as a
# scalar and not a data frame). You will be using this optimal lambda later
# 2. Compute the data frame coefficients that stores the values of the beta-
# coefficients for each value of lambda as seen in shrinkage.R from Lec16
# 3. And thus save a ggplot to PDF that shows the shrinking of each beta as 
# lambda varies AND has the optimal lambda marked with a vertical line

# Find the optimal lambda from these values. Feel free to toy with the from, to,
# and length values.
lambda_values <- 10^seq(from = 1, to = 3.5, length = 2500)

# FILL IN BELOW:

model_LASSO <- glmnet(X, y, alpha = 1, lambda = lambda_values)

coefficients_LASSO <- model_LASSO %>%
  tidy() %>%
  tbl_df() %>%
  mutate(log_lambda = log(lambda)) %>% 
  select(-c(step, dev.ratio)) %>% 
  filter(term != "(Intercept)")

cvfit_2 <- cv.glmnet(X, y, alpha = 1, lambda=lambda_values, nfolds = n_folds)

plot(cvfit_2)

cvfit_2 %>%
  glance()
lambda_star_LASSO <- cvfit_2 %>%
  glance() %>%
  .[["lambda.min"]]
lambda_star_LASSO

# Plot coefficients for each value of lambda, with optimal lambda marked
LASSO_coefficients_plot <- ggplot(coefficients_LASSO, aes(x=log_lambda, y=estimate, col=term)) +
  geom_line() +
  geom_vline(xintercept=log(lambda_star_LASSO)) +
  labs(x="log(lambda)", y="Coefficient Estimate", title="LASSO Coefficients")
LASSO_coefficients_plot
# Save to PDF
ggsave(filename="PS06_Phil_Hoxie_lasso_coefficients.pdf", 
       LASSO_coefficients_plot, width=11, height=8.5)

# 3. Cross-validation comparison of lm(), ridge, and LASSO ---------------------
# Create folds
train <- train %>%
  sample_frac(1) %>%
  mutate(fold = rep(1:n_folds, length=n())) %>%
  arrange(fold)

# For all n_folds folds, save 3 MSE's from the 3 methods here. For each fold,
# we will append/bind the results for each fold.
results_all_folds <- NULL

for(i in 1:n_folds){
  # FILL IN BELOW:
  
  pseudo_train <- train %>%
    filter(fold != i)
  pseudo_test <- train %>%
    filter(fold == i)
  
  X_train <- model.matrix(model_formula, data = pseudo_train)[, -1]
  y_train <- pseudo_train$SalePrice
  X_test <- model.matrix(model_formula, data = pseudo_test)[, -1]
  
  model_lm <- lm(model_formula, data = pseudo_train)
  model_LASSO <- glmnet(X_train, y_train, alpha = 1, lambda = lambda_star_LASSO)
  model_ridge <- glmnet(X_train, y_train, alpha = 0, lambda = lambda_star_ridge)
  
  # Get predictions for all three methods.
  LM_Predictions <- model_lm %>% 
    predict(newdata=pseudo_test) %>% 
    as.vector()
  
  RIDGE_Predictions <- model_ridge %>% 
    predict(newx=X_test, s=lambda_star_ridge) %>% 
    as.vector()
  
  LASSO_Predictions <- model_LASSO %>% 
    predict(newx=X_test, s=lambda_star_LASSO) %>% 
    as.vector()
  
  length(LASSO_Predictions)
  length(RIDGE_Predictions)
  length(LM_Predictions)
  
  # Compute MSE for each method and save them (by appending/binding to
  # results_all_folds)
  
#Taxation is theft
  
  results_all_folds <- 
    # Create data frame of y=SalePrice and predictions from all three methods
    data_frame(
      SalePrice = pseudo_test$SalePrice,# FILL IN HERE
      lm = as.vector(LM_Predictions),
      ridge = as.vector(RIDGE_Predictions),
      LASSO = as.vector(LASSO_Predictions)
    ) %>% 
    # Switch to tidy format so we can use group_by() to compute MSE's for all
    # three methods easily
    tidyr::gather(method, yhat, -SalePrice) %>% 
    group_by(method) %>% 
    summarise(MSE = mean((SalePrice-yhat)^2)) %>% 
    # Include fold i variable and append/bind to results_all_folds
    mutate(fold=i) %>% 
    bind_rows(results_all_folds)
}

# Show results
results_all_folds %>% 
  group_by(method) %>% 
  summarise(MSE_CV = mean(MSE)) %>% 
  arrange(desc(MSE_CV))

# Run the CV comparision a few times manually (in order to account for sampling
# variability) and get a rough ranking of which method does best. You will use
# this method below


## Let's use LASSO!


# 4. Submit Predictions to Kaggle ----------------------------------------------
sample_submission <- read_csv("https://raw.githubusercontent.com/rudeboybert/MATH218/gh-pages/assets/PS/PS06/sample_submission.csv")
test <- read_csv("https://raw.githubusercontent.com/rudeboybert/MATH218/gh-pages/assets/PS/PS06/test.csv") %>%
  # Variable names in R can't start with numbers, so rename:
  dplyr::rename(
    FirstFlrSF = `1stFlrSF`,
    SecondFlrSF = `2ndFlrSF`,
    ThreeSsnPorch = `3SsnPorch`
  ) %>% 
  # Add a "dummy" SalePrice outcome so that the model.matrix() commmand below
  # will still work
  mutate(SalePrice=1)

# In case your optimal method was ridge regression or LASSO, you'll need this 
# for glmnet()
test_X <- model.matrix(model_formula, data = test)[, -1]

# Get predictions using what you think was the best method:
X_Final <- model.matrix(model_formula, data = train)[, -1]
y_Final <- train$SalePrice
model_LASSO_Final <- glmnet(X_Final, y_Final, alpha = 1, lambda = lambda_star_LASSO)

predictions <- model_LASSO_Final %>% 
  predict(newx=test_X, s=lambda_star_LASSO) %>% 
  as.vector()



# Write submissions to CSV
sample_submission %>% 
  mutate(SalePrice = predictions) %>% 
  write_csv("PS06_Phil_Hoxie_submission.csv")