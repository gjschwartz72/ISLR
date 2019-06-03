library(ISLR)
library(ROCR)
View(Smarket)

# Model ----
# Train on 2001 - 2004, predict on 2005
train = Smarket$Year <= 2004
test = !train

glm.fit = glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume,
              data = Smarket[train,], family = binomial)

summary(glm.fit)

# Predict ----


glm_fit = predict(glm.fit, Smarket[test,], type = "response")

glm.pred = rep("Down", nrow(Smarket[test,]))
glm.pred[glm_fit > .5] = "Up"

confMat = table(glm.pred, Smarket$Direction[test])
print(confMat)

#accuracy
(77 + 44) / (77 + 44 + 34 + 97)

#Not Very Good! 
#accuracy = .48, error rate = .52
#If model is on lag1 and lag2 only then accuracy = .582.

summary(glm(Direction ~ Lag1 + Lag2, data = Smarket, 
            family = binomial("logit"), subset = train))


# Logit & sigmoid functions:
par(mfcol = c(2,1))
p = seq(0,1,length.out = 100)
plot(p, log(p/(1-p)), type = "l", main = "Logit Function logit(p)")

eta = seq(-5, 5, length.out = 1000)
plot(eta, 1 / (1 + exp(-1 * eta)), type = "l", main = "sigmoid function (sig(eta))")     
