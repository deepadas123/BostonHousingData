library(MASS)
data(Boston)
head(Boston)
set.seed(1)
index <- sample(nrow(Boston),nrow(Boston)*0.75)
boston.train <- Boston[index,]
boston.test <- Boston[-index,]
install.packages('rpart')
install.packages('rpart.plot') 
library(rpart)
library(rpart.plot)
install.packages("tree")
library(tree)


################Linear Regression#############################
boston.lm <- lm(medv~., data=boston.train)
boston.train.pred.lm<- predict(boston.lm)
boston.test.pred.lm<- predict(boston.lm, boston.test)
MSE.lm<- mean((boston.train.pred.lm - boston.train$medv)^2)
MSPE.lm<- mean((boston.test.pred.lm - boston.test$medv)^2)

library(boot)
model.glm1 = glm(medv~., data = Boston)
cv.glm(data = Boston, glmfit = model.glm1, K = 10)$delta[2]


boston.rpart <- rpart(formula = medv ~ ., data = boston.train)
boston.rpart
prp(boston.rpart,digits = 4, extra = 1)

#Prediction:
#Insample
boston.train.pred.tree = predict(boston.rpart)

#Out of sample
boston.test.pred.tree = predict(boston.rpart,boston.test)

MSE.tree<- mean((boston.train.pred.tree - boston.train$medv)^2)
MSE.tree
MSPE.tree <- mean((boston.test.pred.tree - boston.test$medv)^2)
MSPE.tree

boston.largetree <- rpart(formula = medv ~ ., data = boston.train, cp = 0.001)
prp(boston.largetree)
plotcp(boston.largetree)
printcp(boston.largetree)

mean((predict(boston.largetree) - boston.train$medv)^2)

prune.tree1 <- prune(boston.largetree, cp = 0.007)
prune.tree1
prp(prune.tree1)

#Tree performance of pruned tree
boston.train.pred.tree = predict(prune.tree1)
boston.test.pred.tree = predict(prune.tree1, boston.test)
mean((boston.test.pred.tree - boston.test$medv)^2)
mean((boston.train.pred.tree - boston.train$medv)^2)

################Bagging#################################
install.packages("ipred")
library(ipred)

boston.bag<- bagging(medv~., data = boston.train, nbagg=100)
boston.bag

boston.bag.pred<- predict(boston.bag, newdata = boston.test)
mean((boston.test$medv-boston.bag.pred)^2)

boston.bag.pred.train<- predict(boston.bag, newdata = boston.train)
mean((boston.train$medv-boston.bag.pred.train)^2)

plot(boston.bag)


ntree<- c(1, 3, 5, seq(10, 200, 10))
MSE.test<- rep(0, length(ntree))
for(i in 1:length(ntree)){
  boston.bag1<- bagging(medv~., data = boston.train, nbagg=ntree[i])
  boston.bag.pred1<- predict(boston.bag1, newdata = boston.test)
  MSE.test[i]<- mean((boston.test$medv-boston.bag.pred1)^2)
}
plot(ntree, MSE.test, type = 'l', col=2, lwd=2, xaxt="n")
axis(1, at = ntree, las=1)

boston.bag.oob<- bagging(medv~., data = boston.train, coob=T, nbagg=100)
boston.bag.oob


#############################Random Forest############################

install.packages("randomForest")
library(randomForest)
set.seed(1)
boston.rf<- randomForest(medv~., data = boston.train,importance=TRUE)
boston.rf

boston.rf6<- randomForest(medv~., data = boston.train, mtry= 6, importance=TRUE)


boston.rf$importance

plot(boston.rf$mse, type='l', col=2, lwd=2, xlab = "ntree", ylab = "OOB Error")

boston.rf.pred<- predict(boston.rf, boston.test)
mean((boston.test$medv-boston.rf.pred)^2)

boston.rf.pred.train<- predict(boston.rf, boston.train)
mean((boston.train$medv-boston.rf.pred.train)^2)

boston.rf.100tree<- randomForest(medv~., data = boston.train,ntree= 100, importance=TRUE)
boston.rf.100tree.pred<- predict(boston.rf.100tree, boston.test)
mean((boston.test$medv-boston.rf.100tree.pred)^2)

oob.err<- rep(0, 13)
test.err<- rep(0, 13)
for(i in 1:13){
  fit<- randomForest(medv~., data = boston.train, mtry=i)
  oob.err[i]<- fit$mse[500]
  test.err[i]<- mean((boston.test$medv-predict(fit, boston.test))^2)
  cat(i, " ")
}

matplot(cbind(test.err, oob.err), pch=15, col = c("red", "blue"), type = "b", ylab = "MSE", xlab = "mtry")
legend("topright", legend = c("test Error", "OOB Error"), pch = 15, col = c("red", "blue"))

###########################Boosting#############################
install.packages("gbm")
library(gbm)
boston.boost<- gbm(medv~., data = boston.train, distribution = "gaussian", n.trees = 10000, shrinkage = 0.01, interaction.depth = 8)
summary(boston.boost)

pred.boost=predict(boston.boost,newdata=boston.test,n.trees=10000)
mean((pred.boost-boston.test$medv)^2)

pred.boost=predict(boston.boost,newdata=boston.train,n.trees=10000)
mean((pred.boost-boston.train$medv)^2)

par(mfrow=c(1,2))
plot(boston.boost, i="lstat")
plot(boston.boost, i="rm")
set.seed(1)
ntree<- seq(100, 10000, 100)
predmat<- predict(boston.boost, newdata = boston.test, n.trees = ntree)
err<- apply((predmat-boston.test$medv)^2, 2, mean)
plot(ntree, err, type = 'l', col=2, lwd=2, xlab = "n.trees", ylab = "Test MSE")
abline(h=min(test.err), lty=2)
