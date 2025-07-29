---
title: "Healthcare Cost Prediction"
author: "Miranda Gemme"
date: "`r Sys.Date()`"
output: pdf_document
---

# Multiple linear regression.  

```{r}
setwd("~/Desktop")
insurance <- read.csv("insurance.csv", header = TRUE)
names(insurance)
head(insurance, 5)
any(is.na(insurance))

##### dataset has 7 variables and 1338 cases.
```

```{r}
summary(insurance)
mean(insurance$expenses)
median(insurance$expenses)
```

```{r}
col.class <- sapply(insurance, class)
print(col.class)

## Categorical Data : Sex, Smoker, Region 
## Numerical Data: Age, bmi, Children, and expenses.
```

```{r}
### making data set with numeric data only
num_data <- unlist(lapply(insurance, is.numeric))
numb_data <- subset(insurance, select=c(age, bmi,children,expenses))
### cannot plot numb_data because it is a list technically thats why there is 2 data sets
```

```{r}
plot(insurance[,num_data])
```

```{r}
### creating scatterplots to compare data w regression line
par(mfrow=c(2,2))

plot(expenses ~ age, data = numb_data, cex = 0.4, main = "Expenses by age")
lm.age.exp = lm(expenses ~ age, data = numb_data)
abline(lm.age.exp, lwd, col = "red")

plot(expenses ~ bmi, data = numb_data, cex = 0.4, main = "Expenses by BMI")
lm.bmi.exp = lm(expenses ~ bmi, data = numb_data)
abline(lm.bmi.exp, lwd, col = "purple")

plot(expenses ~ children, data = numb_data, cex = 0.4, main = "Expenses by Number of Children")
lm.child.exp = lm(expenses ~ children, data = numb_data)
abline(lm.child.exp, lwd, col = "blue")


```

```{r}
# histogram to compare numeric data and expenses
par(mfrow=c(2,2))
hist(numb_data$age)
hist(numb_data$bmi)
hist(numb_data$children)
hist(numb_data$expenses)
```

```{r}
### Plotting and comparing categorical data + number of children
par(mfrow=c(2,2))
boxplot(expenses ~ sex, data = insurance, main = "Expenses by Sex")
boxplot(expenses ~ smoker, data = insurance, main = "Expenses for Smokers")
boxplot(expenses ~ children, data = insurance, main = "Expenses by number of children")
boxplot(expenses ~ region, data = insurance, main = "Expenses by Region")
```

```{r}
round(cor(insurance[,num_data]),3)
```

```{r}
ins.lm <- lm(expenses ~ ., data = insurance)
summary(ins.lm)
```

```{r}
ins.lm.sig <- lm(expenses ~ age + bmi + region + smoker, data = insurance)
summary(ins.lm.sig)

```

```{r}
par(mfrow = c(2,2))
plot(ins.lm.sig)
```

```{r}
ins.lm.sig2 <- lm(expenses ~ age + bmi + smoker, data = insurance)
summary(ins.lm.sig2)
```

```{r}
par(mfrow = c(2,2))
plot(ins.lm.sig2)

```

```{r}
cor(insurance$expenses, insurance$age)

cor(insurance$expenses, insurance$bmi)

cor(insurance$expenses, insurance$children)
```


```{r}
rm(list = setdiff(ls(), "insurance"))
```

```{r}
insurance$sex <- as.factor(insurance$sex)
insurance$smoker <- as.factor(insurance$smoker)
insurance$region <- as.factor(insurance$region)
```

# Best Subset Selection.   

```{r}
dim(insurance)
sum(is.na(insurance))

```

```{r}
library(leaps)
reg.fit.ins <- regsubsets(expenses ~ ., insurance)
summary(reg.fit.ins)
```

```{r}
reg.fit.ins <- regsubsets(expenses ~ ., data = insurance, nvmax = 8)

reg.sum <- summary(reg.fit.ins)

names(reg.sum)

reg.sum$rsq
```

```{r}
which.max(reg.sum$adjr2)
### max used for plot below


summary(reg.fit.ins)
```



```{r}
### the red dot shows the model with the largest adjusted r^2 stat
par(mfrow = c(2, 2))
plot(reg.sum$rss, xlab = "# of variables", 
     ylab = "RSS", type = "l")

plot(reg.sum$adjr2, xlab = "# of variables", 
     ylab = "Adjusted RSq", type = "l")

points(6, reg.sum$adjr2[6], col = "red", cex = 2, 
       pch = 20)
```

```{r}
#reg.sum min used for plot below
which.min(reg.sum$cp)

which.min(reg.sum$bic)
```

```{r}
par(mfrow = c(2, 2))
plot(reg.sum$cp, xlab = "# of Variables",
     ylab = "Cp", type = "l")

points(6, reg.sum$cp[6], col = "red", cex = 2,
       pch = 20)

plot(reg.sum$bic, xlab = "# of Variables",
     ylab = "BIC", type = "l")

points(4, reg.sum$bic[4], col = "red", cex = 2,
       pch = 20)
```

```{r}
plot(reg.fit.ins, scale = "r2")
plot(reg.fit.ins, scale = "adjr2")
plot(reg.fit.ins, scale = "Cp")
plot(reg.fit.ins, scale = "bic")
```

```{r}
# coef regression fit for ins
coef(reg.fit.ins, 7)
```




# Stepwise Selection - FORWARD BACKWARD.  

```{r}
step.fwd <- regsubsets(expenses ~., data = insurance, nvmax = 9, method = "forward")

fwd.sum <- summary(step.fwd)

names(fwd.sum)

fwd.sum$rsq
```

```{r}
# backward stepwise
step.bwd <- regsubsets(expenses ~., data = insurance, nvmax = 9, method = "backward")
bwd.sum <- summary(step.bwd)

names(bwd.sum)

bwd.sum$rsq
```

```{r}
#full coef for reg fit
coef(reg.fit.ins, 8)
```

```{r}
#for fwd fit
coef(step.fwd, 8)
```

```{r}
coef(step.bwd, 8)
```


```{r}
str(step.fwd)
```

```{r}
str(step.bwd)
```


```{r}
rm(list = setdiff(ls(), "insurance"))
```



# RIDGE REGRESSION.  

```{r}
library(glmnet)
insurance <- read.csv("insurance.csv", header = TRUE)
set.seed(1)

sum(is.na(insurance))

train.eq <- sample(1:nrow(insurance), nrow(insurance) / 2)
train.ds <- insurance[train.eq, ]
test <- insurance[-train.eq, ]

x.tr <- model.matrix(expenses ~ ., data = train.ds)[, -1]
y.tr <- train.ds$expenses
x.tst <- model.matrix(expenses ~ ., data = test)[, -1]

grid <- 10 ^ seq(4, -2, length = 100)

ridge.mod <- glmnet(x.tr, y.tr, alpha = 0, lambda = grid, thresh = 1e-6)

cv.mod <- cv.glmnet(x.tr, y.tr, alpha = 0)
best.lam <- cv.mod$lambda.min
plot(cv.mod)
print("best.lam:")
print(best.lam)

ridge.pred <- predict(ridge.mod, newx = x.tst, s = best.lam)

mean.ridge <- mean((ridge.pred - test$expenses)^2)
print("mean.ridge:")
print(mean.ridge)
dim(coef(ridge.mod))

```

```{r}
library(glmnet)
insurance <- read.csv("insurance.csv", header = TRUE)
insurance$expenses <- log(insurance$expenses)

set.seed(1)

sum(is.na(insurance))

train.eq <- sample(1:nrow(insurance), nrow(insurance) / 2)
train.ds <- insurance[train.eq, ]
test <- insurance[-train.eq, ]

x.tr <- model.matrix(expenses ~ ., data = train.ds)[, -1]
y.tr <- train.ds$expenses
x.tst <- model.matrix(expenses ~ ., data = test)[, -1]

grid <- 10 ^ seq(4, -2, length = 100)

ridge.mod <- glmnet(x.tr, y.tr, alpha = 0, lambda = grid, thresh = 1e-6)

cv.mod <- cv.glmnet(x.tr, y.tr, alpha = 0)
best.lam <- cv.mod$lambda.min
plot(cv.mod)
print("best.lam:")
print(best.lam)

ridge.pred <- predict(ridge.mod, newx = x.tst, s = best.lam)

mean.ridge <- mean((ridge.pred - test$expenses)^2)
print("mean.ridge:")
print(mean.ridge)
dim(coef(ridge.mod))

```


```{r}
plot(test$expenses, ridge.pred, main = "True v. Predicted Values", xlab = "True", ylab = "Predicted", col = "black")
abline(a = 0, b = 1, col = "red")

```



```{r}
rm(list = setdiff(ls(), "insurance"))
```



# LASSO.  


```{r}
train.eq <- sample(1:nrow(insurance), nrow(insurance) / 2)
train.ds <- insurance[train.eq, ]
test <- insurance[-train.eq, ]


x <- model.matrix(expenses ~ ., data =insurance)[, -1]
y <- insurance$expenses


grid <- 10 ^ seq(4, -2, length = 100)

las.out <- cv.glmnet(x[train.eq, ], y[train.eq], alpha = 1)
best.lambda.las <- las.out$lambda.min

lasso.mod <- glmnet(x[train.eq, ], y[train.eq], alpha = 1, lambda = best.lambda.las, thresh = 1e-10)

lasso.pred <- predict(lasso.mod, newx = model.matrix(expenses ~ ., data = test)[, -1], s = best.lambda.las)

mean.las <- mean((lasso.pred - test$expenses)^2)

lasso.coef <- predict(lasso.mod, s = best.lambda.las, type = "coefficients")

print("best.lambda.las:")
print(best.lambda.las)

print("mean.las:")
print(mean.las)

print("lasso.coef:")
print(lasso.coef)

```
```{r}
plot(las.out)
```




```{r}
rm(list = setdiff(ls(), "insurance"))
```



# PCR.  

```{r}
library(pls)
set.seed(1)
#PCR with Cross-Validation
pcrfit <- pcr(expenses ~ ., data = insurance, scale = TRUE,
validation = "CV")
summary(pcrfit)
validationplot(pcrfit, val.type = "MSEP")

#Data Splitting and PCR on Training Set:
train_eq <- sample(1:nrow(insurance), nrow(insurance) / 2)
train_ds <- insurance[train_eq,]
test <- insurance[-train_eq,]
pcrfit = pcr(expenses ~ ., data = insurance, subset = train_eq, scale = TRUE, validation = "CV")
summary(pcrfit)
validationplot(pcrfit, val.type = "MSEP")

```


```{r}
x <- model.matrix(expenses ~ ., insurance)[, -1]
y <- insurance$expenses
pcr.pred <- predict(pcrfit, x[-train_eq,], ncomp = 5)
mean((pcr.pred - test$expenses)^2)
```

```{r}
# Example of PCR
pcr_model <- pcr(expenses ~ age + bmi + children + smoker + sex + region, data = insurance, scale = TRUE, validation = "CV")
summary(pcr_model)

```



# PLSR.  

```{r}
set.seed(1)
plsr.fit <- plsr(expenses ~ ., data = insurance, subset = train_eq, scale = TRUE, validation = "CV")
summary(plsr.fit)
validationplot(plsr.fit, val.type = "MSEP")


plsr.pred <- predict(plsr.fit, newdata = x[-train_eq, ], ncomp = 2)
plsr.mean <- mean((plsr.pred - test$expenses)^2)
print(plsr.mean)
```



```{r}
rm(list = setdiff(ls(), "insurance"))
```



```{r}
library(tree)
insurance <- read.csv("insurance.csv", header = TRUE)
names(insurance)
head(insurance, 5)
any(is.na(insurance))
# re-downloading insurance data set, so not using log anymore
```

# Regression Trees.  

```{r}
set.seed(10)
# splits 50:50 
train.eq <- sample(1:nrow(insurance), nrow(insurance) / 2)
train.ds <- insurance[train.eq, ]
test <- insurance[-train.eq, ]
tree.mod.ins <- tree(expenses ~., data = train.ds)
summary(tree.mod.ins)
```
```{r}
plot(tree.mod.ins)
text(tree.mod.ins, pretty = 0, cex = 0.6)

ins.pred <- predict(tree.mod.ins, newdata = test)
ins.mse.test <- mean((ins.pred - insurance$expenses)^2)
print(ins.mse.test)
```

```{r}
#Pruning the tree 
set.seed(10)
cv.ins <- cv.tree(tree.mod.ins)
plot(cv.ins, type = "b")
```


```{r}
ins.prune <- prune.tree(tree.mod.ins, best = 9)
plot(ins.prune)
text(ins.prune, pretty = 0, cex = 0.6)
```

```{r}
prune.pred <- predict(ins.prune, newdata = test)
mse.test.ins <- mean((ins.pred - test$expenses)^2)
mse.test.ins

```


# Bagging.  

```{r Bagging approach}
library(randomForest)
bag.model.ins <- randomForest(expenses ~., data = train.ds, importance = TRUE, ntree = 500, mtry = 3)
ins.pred2 <- predict(bag.model.ins, newdata = test)
bag.mse.ins <- mean((ins.pred2 - test$expenses)^2)
print(bag.mse.ins)
```



```{r}
# Before and after pruning, after bagging
B4.pruning <-  ins.mse.test
after.pruning <- mse.test.ins
after.bagging <- bag.mse.ins
table.ins.pru <- data.frame(B4.pruning, after.pruning, after.bagging)
colnames(table.ins.pru) <- c("MSE Before Pruning", "MSE After Pruning", "MSE After Bagging")
head(table.ins.pru)
```

```{r}
#MSE before pruning: 186,832,377

#MSE after pruning: 161,920,636

#MSE after bagging: 23,764,342

```


```{r}
bag.model.ins$importance
importance(bag.model.ins)
```



# BART!  
```{r}
library(BART)
```

```{r} 
ins <- insurance
ins$sex <- as.factor(ins$sex)
ins$smoker <- as.factor(ins$smoker)
ins$region <- as.factor(ins$region)



x.ins <- ins[,1:7]
y.ins <- ins[, "expenses"] 
xtrain <- x.ins[train.eq, ]
ytrain <- y.ins[train.eq]
xtest <- x.ins[-train.eq, ]
ytest <- y.ins[-train.eq]
```


```{r}
set.seed(10)
bart.fit <- gbart(xtrain, ytrain, x.test = xtest)
```

```{r}
yhat.bart <- bart.fit$yhat.test.mean
mse.bag.ins <- mean((ytest - yhat.bart)^2)
mse.bag.ins
```



```{r}
rm(list = setdiff(ls(), "insurance"))
```


```{r}
set.seed(10)
library("gbm")
```




# Boosting.  

```{r}
# log form
ins <- na.omit(insurance)
insurance$expenses <- log(insurance$expenses)
ins$sex <- as.factor(ins$sex)
ins$smoker <- as.factor(ins$smoker)
ins$region <- as.factor(ins$region)
```


 

```{r}
train <- 1:200
ins.train <- ins[train, ]
ins.test <- ins[-train, ]
```



```{r}
p <- seq(-4, 0, by = 0.1)
lamb <- 10^p
train <- rep(NA, length(lamb))
for (i in 1:length(lamb)) {
    boost.ins <- gbm(expenses ~ ., data = ins.train, distribution = "gaussian", n.trees = 1000, shrinkage = lamb[i])
    pred.train <- predict(boost.ins, ins.train, n.trees = 1000)
    train[i] <- mean((pred.train - ins.train$expenses)^2)
}
plot(x=lamb, y=train, type = "b", cex = 0.6, xlab = "Shrink", ylab = "Training MSE")
```



```{r}
### A plot with diferent shrinkage values on the x-axis and the corresponding test set MSE on the y-axis. 
set.seed(10)
test.err = rep(NA, length(lamb))
for (i in 1:length(lamb)) {
  mod = gbm(expenses ~ ., data = ins.train, distribution = "gaussian", n.trees = 1000, shrinkage = lamb[i])
  mod.pred = predict(mod, ins.test, n.trees = 1000)
  test.err[i] = mean((mod.pred - ins.test$expenses)^2)
  }
plot(lamb, test.err, type = "b", cex = 0.5, xlab = "Shrinkage", ylab = "Test MSE")

```

```{r}
boost.test = min(test.err)
boost.test
```




```{r}
# Linear
lm.mod <- lm(expenses ~ ., data = ins.train)
lm.pred <- predict(lm.mod, ins.test)
mean((lm.pred - ins.test$expenses)^2)
```

```{r}
library("glmnet")
```

```{r}
# Ridge Regression !!!
x <- model.matrix(expenses ~ ., data = ins.train)
x.tst <- model.matrix(expenses ~ ., data = ins.test)
y <- ins.train$expenses
lm.fit <- glmnet(x, y, alpha = 0)
lm.pred <- predict(lm.fit, s = 0.01, newx = x.tst)
mean((lm.pred - ins.test$expenses)^2)
```


```{r}
# Smoker and Age!
boost.mod = gbm(expenses~., data = ins.train, distribution = "gaussian", n.trees = 1000, shrinkage = lamb[which.min(test.err)])

summary(boost.mod)
```


```{r}
rm(list = setdiff(ls(), "insurance"))
```


# Logistic Regression:  

```{r}
set.seed(1)
ins <- insurance
ins$smoker01 <- as.numeric(ins$smoker == 'yes')
ins$sex01 <- as.numeric(ins$sex == 'female')
as.numeric(levels(ins$sex01))[levels(ins$sex01)]
as.numeric(levels(ins$smoker01))[levels(ins$smoker01)]
dim(insurance)
```

```{r}
glm.log.fit <- glm(smoker01 ~ region + sex01 + children + bmi, data = ins, family = binomial)
summary(glm.log.fit)

coef(glm.log.fit)
summary(glm.log.fit)$coef

glm.probs <- predict(glm.log.fit, type = "response")
glm.probs[1:10]
```


```{r}
# The logistic regression model is built to predict the probability of being a smoker (smoker01) based on the specified predictor variables. The coefficients and predicted probabilities provide insights into the relationships between the predictors and the likelihood of being a smoker.
```


```{r}
lm.ins.log <- lm(smoker01 ~ region + sex01 + children, data = ins)

# predict on model
predictions <- predict(lm.ins.log, data = ins)
ins.pred <- predict(lm.ins.log, data = ins, type = "response")

# Create vector for predictions
ins.prediction <- ifelse(ins.pred < 1, "smoker", "non smoker")

table(ins.prediction, ins$smoker)
accuracy <- mean(ins.prediction == ins$smoker)
print(accuracy)
```


```{r}
rm(list = setdiff(ls(), "insurance"))
```



# LDA.  
```{r}
library(MASS)
```

```{r}
insurance <- read.csv("insurance.csv", header = TRUE)

set.seed(1)
ins <- insurance

as.numeric(levels(ins$sex))[levels(ins$sex)]
as.numeric(levels(ins$smoker))[levels(ins$smoker)]


ins.idx = sample(1:nrow(ins), nrow(ins) / 2)
ins.trn = ins[ins.idx, ]
ins.tst = ins[-ins.idx, ]

# training data
trn.ds = ins.trn$smoker

# testing data
tst.ds = ins.tst$smoker
```

```{r}

ins.lda <- lda(smoker ~ region + expenses + bmi, data = ins, subset = ins.idx)
ins.lda

pred.lda <- predict(ins.lda, ins.tst)

# This says the prediction is 79.5% accurate.
accuracy.lda <- mean(pred.lda$class == tst.ds)
accuracy.lda
```

```{r}
table(pred.lda$class, tst.ds)
```

```{r}
summary(ins.lda)
```



# QDA.  

```{r}
set.seed(1)
ins.qda <- qda(smoker ~ region + expenses + bmi + children, data = ins, subset = ins.idx)
ins.qda


pred.qda <- predict(ins.qda, ins.tst)

table(pred.qda$class, tst.ds)

```

```{r}
# This says the prediction is 79.5% accurate.
accuracy.qda <- mean(pred.qda$class == tst.ds)
print(accuracy.qda)
summary(ins.qda)
```

# Naive Bayes.  
```{r}
library(e1071)
```

```{r}
nb.fit <- naiveBayes(smoker ~ region + sex + bmi + children, data = ins, subset = ins.idx)
nb.fit
```

```{r}
insurance$smoker <- as.factor(insurance$smoker)
insurance$sex <- as.factor(insurance$sex)
insurance$region <- as.factor(insurance$region)


correlation_children_smoking <- cor(insurance$children, as.numeric(insurance$smoker == 'yes'))

table_sex_smoking <- table(insurance$sex, insurance$smoker)
chi_square_sex_smoking <- chisq.test(table_sex_smoking)

table_region_smoking <- table(insurance$region, insurance$smoker)
chi_square_region_smoking <- chisq.test(table_region_smoking)

model <- lm(expenses ~ smoker + age + bmi + children + sex + region, data = insurance)
summary(model)

```



```{r}
rm(list = setdiff(ls(), "insurance"))
```







