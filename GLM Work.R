library(ISLR)
library(glmnet)

##  GLM Regression ----
hist(Credit$Cards, breaks = 11, freq = F)
par(new = T)
lines(0:12, dpois(seq(0,12),mean(Credit$Cards)), col = "red")

table(as.factor(Credit$Cards))

mGLM = glm(Cards ~ Rating, data = Credit, family = poisson(link = "log"))
summary(mGLM)


## Regularization: Ridge & Lasso ----

# Need a stinking model matrix :-(
data("Hitters")
x = model.matrix(Salary ~ ., Hitters)[,-1]  # Don't need the intercept
y = na.omit(Hitters$Salary)

# Ridge
grid = 10^seq(10, -2, length=100)
range(grid)
mRidge = glmnet(x, y, alpha = 0, lambda = grid)

# cv.ridge, to select lambda
set.seed(1)
train = sample(1:nrow(x), nrow(x) * .6)
test = -train
cvRidge = cv.glmnet(x[train, ], y[train], alpha = 0, lambda = grid)
cvRidge$lambda.min

bestRidge = glmnet(x[train, ], y[train], lambda = cvRidge$lambda.min, alpha = 0)

coef(glmnet(x, y, alpha  = 0, lambda = cvRidge$lambda.min))

out = glmnet(x,y, alpha = 0)
predict(out, type = "coefficients", s = cvRidge$lambda.min)
