install.packages("AmesHousing")
library(AmesHousing)
ames = make_ames()
ames = as.data.frame(ames)

nrow(ames)
summary(ames$Sale_Price)
ames_ord <- make_ordinal_ames()
ord_vars <- vapply(ames_ord, is.ordered, logical(1))
names(ord_vars)[ord_vars]

write.csv(ames_ord, "ames_R.csv", row.names = FALSE)
getwd()

#library(readr)
#ames = read_csv('ames.csv')
class(ames)
ames = as.data.frame(ames)
##########################
#####Regression Trees#####
##########################
#Inspecting the housing values in the suburbs of Boston.
#Creating a training set on 70% of the data.
set.seed(0)
train = sample(1:nrow(ames), 7*nrow(ames)/10)

#Training the tree to predict the median value of owner-occupied homes (in $1k).
tree.ames = tree(Sale_Price ~ ., data=ames, subset = train)
summary(tree.ames)

#Visually inspecting the regression tree.
par(mfrow = c(1, 1))
plot(tree.ames)
text(tree.ames, pretty = 0)

#Performing cross-validation.
#set.seed(0)
cv.ames = cv.tree(tree.ames)
par(mfrow = c(1, 2))
plot(cv.ames$size, cv.ames$dev, type = "b",
     xlab = "Terminal Nodes", ylab = "RSS")
plot(cv.ames$k, cv.ames$dev, type  = "b",
     xlab = "Alpha", ylab = "RSS")

#Pruning the tree to have 4 terminal nodes.
prune.ames = prune.tree(tree.ames, best = 10)
par(mfrow = c(1, 1))
#?prune.tree
plot(prune.ames)
text(prune.ames, pretty = 0)

#Calculating and assessing the MSE of the test data on the overall tree.
yhat = predict(tree.ames, newdata = ames[-train, ])
yhat
#class(yhat)
#class(ames.test[[1]])
ames.test = ames[-train, "Sale_Price"]
plot(yhat, ames.test[[1]])
abline(0, 1)
mean((yhat - ames.test[[1]])^2)

#Calculating and assessing the MSE of the test data on the pruned tree.
yhat = predict(prune.ames, newdata = ames[-train, ])
yhat
plot(yhat, ames.test[[1]])
abline(0, 1)
mean((yhat - ames.test[[1]])^2)

##################################
#####Bagging & Random Forests#####
##################################
library(randomForest)

#Fitting an initial random forest to the training subset.
set.seed(0)
rf.ames = randomForest(Sale_Price ~ ., data = ames, subset = train, importance = TRUE)

rf.ames
#Call:
#  randomForest(formula = Sale_Price ~ ., data = ames, importance = TRUE,      subset = train) 
#Type of random forest: regression
#Number of trees: 500
#No. of variables tried at each split: 26
#Mean of squared residuals: 653338445
#% Var explained: 89.89

#The MSE and percent variance explained are based on out-of-bag estimates,
#yielding unbiased error estimates. The model reports that mtry = 4, which is
#the number of variables randomly chosen at each split. Since we have 13 overall
#variables, we could try all 13 possible values of mtry. We will do so, record
#the results, and make a plot.

#Varying the number of variables used at each step of the random forest procedure.
set.seed(0)
oob.err = numeric(13)
for (mtry in 1:13) {
  fit = randomForest(medv ~ ., data = Boston[train, ], mtry = mtry)
  oob.err[mtry] = fit$mse[500]
  cat("We're performing iteration", mtry, "\n")
}

#Visualizing the OOB error.
plot(1:13, oob.err, pch = 16, type = "b",
     xlab = "Variables Considered at Each Split",
     ylab = "OOB Mean Squared Error",
     main = "Random Forest OOB Error Rates\nby # of Variables")

#Can visualize a variable importance plot.
importance(rf.ames)
varImpPlot(rf.ames)



##################
#####Boosting#####
##################
library(gbm)

#Fitting 10,000 trees with a depth of 4.
set.seed(0)
boost.ames = gbm(Sale_Price ~ ., data = ames[train, ],
                   distribution = "gaussian",
                   n.trees = 10000,
                   interaction.depth = 4)

#Inspecting the relative influence.
par(mfrow = c(1, 1))
summary(boost.ames)

#Let?€™s make a prediction on the test set. With boosting, the number of trees is
#a tuning parameter; having too many can cause overfitting. In general, we should
#use cross validation to select the number of trees. Instead, we will compute the
#test error as a function of the number of trees and make a plot for illustrative
#purposes.
n.trees = seq(from = 100, to = 10000, by = 100)
predmat = predict(boost.ames, newdata = ames[-train, ], n.trees = n.trees)

#Produces 100 different predictions for each of the 152 observations in our
#test set.
dim(predmat)

#Calculating the boosted errors.
par(mfrow = c(1, 1))
berr = with(ames[-train, ], apply((predmat - Sale_Price)^2, 2, mean))
plot(n.trees, berr, pch = 16,
     ylab = "Mean Squared Error",
     xlab = "# Trees",
     main = "Boosting Test Error")

#Include the best OOB error from the random forest.
abline(h = min(oob.err), col = "red")

#Increasing the shrinkage parameter; a higher proportion of the errors are
#carried over.
set.seed(0)
boost.ames2 = gbm(Sale_Price ~ ., data = ames[train, ],
                    distribution = "gaussian",
                    n.trees = 10000,
                    interaction.depth = 4,
                    shrinkage = 0.1) #shrinkage is lambda
predmat2 = predict(boost.ames2, newdata = ames[-train, ], n.trees = n.trees)

berr2 = with(ames[-train, ], apply((predmat2 - Sale_Price)^2, 2, mean))
plot(n.trees, berr2, pch = 16,
     ylab = "Mean Squared Error",
     xlab = "# Trees",
     main = "Boosting Test Error")
##################################################

### boosting
set.seed(0)
x = (1:1000 + rnorm(1000) - 500)/100
y = 10*x + 20*x^2 + 30*x^3

x0 = x
plot(x0,y)

y1 = y
fit = lm(y1 ~ x)
yhat1 = predict(fit)

y2 = fit$residuals
x = x^2
fit = lm(y2 ~ I(x))
yhat2 = predict(fit)

y3 = fit$residuals
x = x^3
fit = lm(y3 ~ I(x))
yhat3 = predict(fit)

plot(x0, y)
points(x0, yhat1, col = 'red', cex = .1)
points(x0, yhat1+yhat2, col = 'green', cex = .1)
points(x0, yhat1+yhat2+yhat3, col = 'blue', cex = .1)

#use a for loop to get yhat3+ , then plot to get points to the x0,y plot


