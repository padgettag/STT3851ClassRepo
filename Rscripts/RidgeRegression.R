# Ridge Regression
library(ISLR)
Hitters <- na.omit(Hitters)  # Remove missing values
X <- model.matrix(Salary ~ ., data = Hitters)[, -1]
y <- Hitters$Salary
library(glmnet)
grid <- 10^seq(10, -2, length = 100)
#
ridge.mod <- glmnet(X, y, alpha = 0, lambda = grid)
dim(coef(ridge.mod))
