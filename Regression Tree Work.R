library(MASS)
library(tree)
library(randomForest)
library(gbm)

# Regresion Trees ----
# Predict medv: median value of homes in Boston
set.seed(1)
train = sample(1:nrow(Boston), nrow(Boston) / 2)
tree_boston = tree(medv ~ ., data = Boston, subset = train) 
summary(tree_boston) 

pred_tree = predict(tree_boston, Boston[-train, ])
print(glue("Tree Test MSE: {mean((pred_tree - Boston$medv[-train])^2)}"))

plot(pred_tree, Boston$medv[-train], xlab = "Predicted", ylab = "Actual", 
     main = "Tree Based Predictions")

# Random Forest: ----
set.seed(1)
rf_boston = randomForest(medv ~ ., data = Boston, subset = train,
                         mtry = 6, importance = T)
pred_rf = predict(rf_boston, Boston[-train,])
print(glue("Random Forest Test MSE: {mean((pred_rf - Boston$medv[-train])^2)}"))
plot(rf_boston)

# Gradient Boosing Machines: ----
set.seed(1)
gbm_boost = gbm(medv ~ ., data = Boston[train, ], 
               distribution = "gaussian",
               n.trees = 5000, shrinkage = .001, 
               interaction.depth = 4)
summary(gbm_boost)
# Partial Dependence Plots
par(mfrow = c(1,2))
plot(gbm_boost, i = "rm")
plot(gbm_boost, i = "lstat")

pred_gbm = predict(gbm_boost, Boston[-train, ], n.trees = 5000)
print(glue("GBM Trees Test MSE: {mean((pred_gbm - Boston$medv[-train])^2)}"))
