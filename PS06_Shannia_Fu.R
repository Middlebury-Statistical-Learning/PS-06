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

# Create the model formula for lm():
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

model_lm <- lm(model_formula, data=train)
lm_estimates <- model_lm %>%
  tidy()
lm_estimates


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
# find optimal lambda based on Ridge Regression CV
model_ridge_cv <- cv.glmnet(X, y, alpha = 0, lambda=lambda_values, nfolds = n_folds)
model_ridge_cv %>%
  glance()

# optimal lambda
lambda_star_ridge <- model_ridge_cv %>%
  glance() %>%
  .[["lambda.min"]]
lambda_star_ridge

# fit model again for graph
model_ridge <- glmnet(X, y, alpha = 0, lambda = lambda_values)

# find coefficients of model
coefficients <- model_ridge %>%
  tidy() %>%
  tbl_df() %>% 
  mutate(log_lambda = log(lambda)) %>% 
  filter(term != "(Intercept)")

coefficients
# Plot coefficients for each value of lambda, with optimal lambda marked
ridge_coefficients <- ggplot(coefficients, aes(x=log_lambda, y=estimate, col=term)) +
  geom_line() +
  geom_vline(xintercept=log(lambda_star_ridge)) +
  labs(x="log(lambda)", y="Coefficient Estimate", title="Ridge Regression Coefficients")
ridge_coefficients
ggplotly()
# Save to PDF
ggsave(filename="PS06_Shannia_Fu_ridge_coefficients.pdf", 
       ridge_coefficients, width=11, height=8.5)

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
# find optimal lambda based on LASSO CV
model_LASSO_cv <- cv.glmnet(X, y, alpha = 1, lambda=lambda_values, nfolds = n_folds)
model_LASSO_cv %>%
  glance()

# optimal lambda
lambda_star_LASSO <- model_LASSO_cv %>%
  glance() %>%
  .[["lambda.min"]]
lambda_star_LASSO

# fit model again for graph
model_LASSO <- glmnet(X, y, alpha = 1, lambda = lambda_values)

# find coefficients of model
coefficients <- model_LASSO %>%
  tidy() %>%
  tbl_df() %>% 
  mutate(log_lambda = log(lambda)) %>% 
  filter(term != "(Intercept)")
coefficients
# Plot coefficients for each value of lambda, with optimal lambda marked
LASSO_coefficients <- ggplot(coefficients, aes(x=log_lambda, y=estimate, col=term)) +
  geom_line() +
  geom_vline(xintercept=log(lambda_star_LASSO)) +
  labs(x="log(lambda)", y="Coefficient Estimate", title="LASSO Coefficients")
LASSO_coefficients

ggplotly()
# Save to PDF
ggsave(filename="PS06_Shannia_Fu_lasso_coefficients.pdf", 
       LASSO_coefficients, width=11, height=8.5)

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
  
  # create pseudo test and train
  pseudo_test <- train %>% 
    filter(fold == i)
  pseudo_train <- anti_join(train, pseudo_test)
  
  # for fitting
  X <- model.matrix(model_formula, data = pseudo_train)[, -1]
  y <- pseudo_train$SalePrice
  
  # the three models
  model_lm <- lm(model_formula, data=pseudo_train)
  model_ridge <- glmnet(X, y, alpha = 0, lambda = lambda_star_ridge)
  model_LASSO <- glmnet(X, y, alpha = 1, lambda = lambda_star_LASSO)
  
  # for the ridge and lasso predictions
  X_test <- model.matrix(model_formula, data = pseudo_test)[, -1]  
  
  
  # Get predictions for all three methods.
  lm_predictions <- model_lm %>% 
    predict(newdata=pseudo_test)
  ridge_predictions <- predict(model_ridge, s=lambda_star_ridge, newx = X_test, type="response")
  LASSO_predictions <- predict(model_LASSO, s=lambda_star_LASSO, newx = X_test, type="response")
  
  # Compute MSE for each method and save them (by appending/binding to
  # results_all_folds)
  results_all_folds <- 
    # Create data frame of y=SalePrice and predictions from all three methods
    data_frame(
      # the original sale price to figure out MSE
      SalePrice = as.vector(pseudo_test$SalePrice),# FILL IN HERE
      lm = as.vector(lm_predictions),
      ridge = as.vector(ridge_predictions),
      LASSO = as.vector(LASSO_predictions)
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

# SHANNIA'S COMMENTS
# I ran the for loop around 10 times, and each time, LASSO had an MSE that was
# less than or equal to the other two MSEs. So I'm using LASSO for the Kaggle
# predictions.


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

# to retrain because I fit model_LASSO again in the for loop, and the X and y
# vals are from pseudo train still
X <- model.matrix(model_formula, data = train)[, -1]
y <- train$SalePrice
model_LASSO <- glmnet(X, y, alpha = 1, lambda = lambda_star_LASSO)
# Get predictions using what you think was the best method:
predictions <- # FILL THIS IN
  predict(model_LASSO, s=lambda_star_LASSO, newx = test_X, type="response")

# need to replace the negative/zero values with something else; use average to
# replace
mean_val <- mean(as.vector(predictions))
# Write submissions to CSV
sample_submission %>% 
  mutate(SalePrice = as.vector(predictions)) %>% 
  mutate(SalePrice = ifelse(SalePrice > 0, SalePrice, mean_val)) %>% 
  write_csv("PS06_Shannia_Fu_submission.csv")
