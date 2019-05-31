library(randomForest)
library(tree)
library(ISLR)
library(MASS) #For Boston Dataset
data(Boston)
data(Carseats)

# Classification Trees ----
# Example, predicting sales of carseats: High or Low
# High will be the target
Carseats$High = as.factor(ifelse(Carseats$Sales <= 8, "No", "Yes"))

tree_Carseats = tree(High ~. -Sales, Carseats)
summary(tree_Carseats)

plot(tree_Carseats)
text(tree_Carseats, pretty = 0)

predict(tree_Carseats, Carseats[1:10,])

set.seed(2)
train = sample(1:nrow(Carseats), 200)
Carseats_test = Carseats[-train, ]
High_test = Carseats$High[-train]

tree_carseats = tree(High~. -Sales, Carseats, subset = train)
tree_pred = predict(tree_carseats, Carseats_test, type = "class")

(confMat = table(tree_pred, High_test))
print(glue("Tree Accuracy: {sum(diag(confMat)) / sum(confMat)}"))
# Random Forest  ----

rf_carseats = randomForest(High ~ . -Sales, Carseats, subset = train)
rfPred = predict(rf_carseats, Carseats_test)
(rfConfMat = table(rfPred, High_test))
print(glue("rf accuracy: {sum(diag(rfConfMat)) / sum(rfConfMat)}"))

#Variable Importance
importance(rf_carseats)
varImpPlot(rf_carseats, pch = 20)
