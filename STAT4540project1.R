#Question 1.1
#(i)
setwd("~/Downloads")
mov_train <- read.csv(file= "mov_train.csv")
mdl_a <- lm(rating ~ popularity + genre + mood, data=mov_train)
mdl_b <- lm(rating ~ popularity + genre + mood + popularity:genre + popularity:mood + genre:mood, data=mov_train)
mdl_c <- lm(rating ~ popularity + genre + mood + I(popularity^2) + I(genre^2) + popularity:genre + popularity:mood + genre:mood, data=mov_train)
coef(mdl_a)
coef(mdl_b)
coef(mdl_c)
#(ii)
plot(mdl_a)
plot(mdl_b)
plot(mdl_c)

#(iii)
library(boot)
boot.fn.cor = function(data, index) {
  res = cov2cor(vcov(lm(rating ~ popularity + genre + mood, data=data, subset = index)))
  return(c(res[lower.tri(res)]))
}
boot.cor= boot(mov_train, boot.fn.cor, 1000)

#(iv)
boot.cor
0.03563135 - 2*0.011158205
0.03563135 + 2*0.011158205
0.25670339 - 2*0.009881046
0.25670339 + 2*0.009881046
-0.67657681 - 2*0.004616508
-0.67657681 + 2*0.004616508
-0.11895624 - 2*0.011281647
-0.11895624+ 2*0.011281647
-0.11452647 - 2*0.010962150
-0.11452647+ 2*0.010962150
-0.01093495 - 2*0.011013426
-0.01093495 + 2*0.011013426


#10 fold CV
rm(list = ls())
library(ISLR)
library(FNN)

K = c(1, )
trainMseList = list()
testMseList = list()
foldId <- sample(1:10, nrow(mov_train), replace = TRUE)

testMse = matrix(NA, 10, 3)

for (cc in 1:10) {
  cat("Fold: ", cc, "\n")
  testIdx <- which(foldId == cc)
  trainIdx <- which(foldId != cc)

  testdf = mov_train[testIdx, ]
  traindf = mov_train[trainIdx, ]
  
  mdl1 <- lm(rating ~ popularity + genre + mood, data = traindf)
  mdl2 <- lm(rating ~ popularity + genre + mood + popularity:genre + popularity:mood + genre:mood, data=traindf)
  mdl3 <- lm(rating ~ popularity + genre + mood + I(popularity^2) + I(genre^2) + popularity:genre + popularity:mood + genre:mood, data=traindf)
  
  testMse[cc, 1] = mean((testdf$rating - predict(mdl1, testdf))^2)
  testMse[cc, 2] = mean((testdf$rating - predict(mdl2, testdf))^2)
  testMse[cc, 3] = mean((testdf$rating - predict(mdl3, testdf))^2)
}

colMeans(testMse)

#LOOCV
for (cc in 1:nrow(mov_train)) {
  cat("cc:", cc, "\n")
  trainIdx <- (1:nrow(mov_train))[-cc]
  
  traindf <- mov_train[trainIdx]
  testdf <- mov_train[cc]
  
  Klist[[cc]] <- seq(1, nrow(traindf), by = 1)
  
  K <- Klist[[cc]]
  testMse <- numeric(length(K))
  trainMse <- numeric(length(K))
  
  for (ii in K) {
    kresTest <- knn.reg(train = as.matrix(traindf),
                        test = as.matrix(testdf),
                        y = traindf$rating, k = K[ii])
    
    testMse[ii] <- ((testdf$rating - kresTest$pred)^2)
    kresTrain <- knn.reg(train = as.matrix(traindf),
                         test = as.matrix(traindf),
                         y = traindf$rating, k = K[ii])
    trainMse[ii] <- mean((traindf$rating - kresTrain$pred)^2)
  }
  
  trainMseList[[cc]] <- trainMse
  testMseList[[cc]] <- testMse
}


#Question 1.2
#(i)
testMse <- matrix(NA, 10, length(K))

foldId <- sample(1:10, nrow(mov_Train), replace = TRUE) 

for (cc in 1:10) {
  cat("Fold: ", cc, "\n")

  testIdx <- which(foldId == cc)
  trainIdx <- which(foldId != cc)
  
  traindf <- mov_Train[trainIdx, ]
  testdf <- mov_Train[testIdx, ]
  
  for (ii in 1:length(K)) {
    kresTest <- knn.reg(train = as.matrix(traindf[ , -1]),
                        test = as.matrix(testdf[ , -1]),
                        y = traindf$rating,
                        k = K[ii])
    testMse[cc, ii] <- mean((testdf$rating - kresTest$pred)^2)
  }
}



plot(rev(1 / K), colMeans(testMse), type = "l")


#(ii)
knn1.class <- knn(train = as.matrix(kresTest[ ,2:3]),
                  test = as.matrix(kresTrain[ ,2:3]),
                  cl = kresTrain, k = 1)
knn1.tab3 <- table(knn1.class, testdf)
knn1.tab <- knn1.tab3
colnames(knn1.tab3) <- c("true-1", "true-2", "true-3")
rownames(knn1.tab3) <- c("pred-1", "pred-2", "pred-3")
confusionMatrix(knn1.tab)

knn10.class <- knn(train = as.matrix(kresTest[ ,2:3]),
                  test = as.matrix(kresTrain[ ,2:3]),
                  cl = kresTrain, k = 1)
knn10.tab3 <- table(knn1.class, testdf)
knn10.tab <- knn1.tab3
colnames(knn10.tab3) <- c("true-1", "true-2", "true-3")
rownames(knn10.tab3) <- c("pred-1", "pred-2", "pred-3")
confusionMatrix(knn10.tab)

knn50.class <- knn(train = as.matrix(kresTest[ ,2:3]),
                  test = as.matrix(kresTrain[ ,2:3]),
                  cl = kresTrain, k = 1)
knn50.tab3 <- table(knn1.class, testdf)
knn50.tab <- knn1.tab3
colnames(knn50.tab3) <- c("true-1", "true-2", "true-3")
rownames(knn50.tab3) <- c("pred-1", "pred-2", "pred-3")
confusionMatrix(knn50.tab)

knn100.class <- knn(train = as.matrix(kresTest[ ,2:3]),
                  test = as.matrix(kresTrain[ ,2:3]),
                  cl = kresTrain, k = 1)
knn100.tab3 <- table(knn1.class, testdf)
knn100.tab <- knn1.tab3
colnames(knn100.tab3) <- c("true-1", "true-2", "true-3")
rownames(knn100.tab3) <- c("pred-1", "pred-2", "pred-3")
confusionMatrix(knn100.tab)

knn200.class <- knn(train = as.matrix(kresTest[ ,2:3]),
                  test = as.matrix(kresTrain[ ,2:3]),
                  cl = kresTrain, k = 1)
knn200.tab3 <- table(knn1.class, testdf)
knn200.tab <- knn1.tab3
colnames(knn200.tab3) <- c("true-1", "true-2", "true-3")
rownames(knn200.tab3) <- c("pred-1", "pred-2", "pred-3")
confusionMatrix(knn200.tab)

knn500.class <- knn(train = as.matrix(kresTest[ ,2:3]),
                  test = as.matrix(kresTrain[ ,2:3]),
                  cl = kresTrain, k = 1)
knn500.tab3 <- table(knn1.class, testdf)
knn500.tab <- knn1.tab3
colnames(knn500.tab3) <- c("true-1", "true-2", "true-3")
rownames(knn500.tab3) <- c("pred-1", "pred-2", "pred-3")
confusionMatrix(knn500.tab)

knn1000.class <- knn(train = as.matrix(kresTest[ ,2:3]),
                  test = as.matrix(kresTrain[ ,2:3]),
                  cl = kresTrain, k = 1)
knn1000.tab3 <- table(knn1.class, testdf)
knn1000.tab <- knn1.tab3
colnames(knn1000.tab3) <- c("true-1", "true-2", "true-3")
rownames(knn1000.tab3) <- c("pred-1", "pred-2", "pred-3")
confusionMatrix(knn1000.tab)

knn1500.class <- knn(train = as.matrix(kresTest[ ,2:3]),
                     test = as.matrix(kresTrain[ ,2:3]),
                     cl = kresTrain, k = 1)
knn1500.tab3 <- table(knn1.class, testdf)
knn1500.tab <- knn1.tab3
colnames(knn1500.tab3) <- c("true-1", "true-2", "true-3")
rownames(knn1500.tab3) <- c("pred-1", "pred-2", "pred-3")
confusionMatrix(knn1500.tab)

knn2000.class <- knn(train = as.matrix(kresTest[ ,2:3]),
                     test = as.matrix(kresTrain[ ,2:3]),
                     cl = kresTrain, k = 1)
knn2000.tab3 <- table(knn1.class, testdf)
knn2000.tab <- knn1.tab3
colnames(knn2000.tab3) <- c("true-1", "true-2", "true-3")
rownames(knn2000.tab3) <- c("pred-1", "pred-2", "pred-3")
confusionMatrix(knn2000.tab)

knn3000.class <- knn(train = as.matrix(kresTest[ ,2:3]),
                     test = as.matrix(kresTrain[ ,2:3]),
                     cl = kresTrain, k = 1)
knn3000.tab3 <- table(knn1.class, testdf)
knn3000.tab <- knn1.tab3
colnames(knn3000.tab3) <- c("true-1", "true-2", "true-3")
rownames(knn3000.tab3) <- c("pred-1", "pred-2", "pred-3")
confusionMatrix(knn3000.tab)

K <- sort(c(1, 10, 50, 100, 200, 500, 1000, 1500, 2000, 3000))
kres <- list()
for (ii in seq_along(K)) {
  kres[[ii]] <- knn.reg(train = as.matrix(mov_train$popularity, mov_train$genre, mov_train$mood),
                        test = as.matrix(hpgrid),
                        y = mov_train$rating, k = K[ii])
}


#(iii)
library(boot)
variance.fn <- function(data, index) {
  knn.fit <- knn.reg(train = as.matrix(data[index, c("popularity", "genre", "mood")]),
                     test = as.matrix(data[ , c("popularity", "genre", "mood")]),
                     y = data[index, "rating"],
                     k = 200)
  # get variance from predicted values
  variance <- var(data$rating - knn.fit$pred)
  return(variance)
}

boot.var <- boot(movTrain, variance.fn, 200)

boot.var.ci <- quantile(as.numeric(boot.var$t), prob = c(0.025, 0.975))


#Question 2
wisc <- read.csv(file="wisc.csv")
#(i)
pairs(wisc[,2:4])
cor(wisc[,2:4])

#(ii)
mdl2.1 <- glm(as.numeric(diagnosis == "M") ~ log_texture_mean +log_smoothness_mean + log_compactness_mean, data=wisc)
summary(mdl2.1)
0.63501 - 2*0.07515
0.63501 + 2*0.07515

#(iii)
mdl2.2 <- glm(as.numeric(diagnosis == "M") ~ log_texture_mean + log_smoothness_mean + log_compactness_mean + log_texture_mean:log_smoothness_mean +log_texture_mean:log_compactness_mean + log_smoothness_mean:log_compactness_mean, data=wisc)
summary(mdl2.2)

#(iv)
mdl2.3 <- glm(as.numeric(diagnosis == "M") ~ log_texture_mean + log_smoothness_mean + log_compactness_mean + I(log_texture_mean)^2 + I(log_smoothness_mean)^2 + I(log_compactness_mean)^2 + log_texture_mean:log_smoothness_mean + log_texture_mean:log_compactness_mean + log_smoothness_mean:log_compactness_mean, data=wisc)


#Question 2.2
#(iii)
library(MASS)
lda.fit <- lda(as.numeric(diagnosis == "M") ~ log_texture_mean + log_smoothness_mean + log_compactness_mean, data = wisc)
lda.fit
lda_pred = predict(lda.fit, wisc)
lda_class = lda_pred$diagnosis
table(lda_class,wisc$diagnosis)

qda_fit = qda(as.numeric(diagnosis == "M") ~ log_texture_mean + log_smoothness_mean + log_compactness_mean, data = wisc)
qda_pred = predict(qda_fit, wisc)
qda_class = qda_pred$diagnosis
table(qda_class,wisc$diagnosis)



#Question 2.3
#(i)
library(boot)
boot.fn.cor2 = function(data, index) {
  res = cov2cor(vcov(lm(as.numeric(diagnosis == "M") ~ log_texture_mean + log_smoothness_mean + log_compactness_mean, data = wisc, subset = index)))
  return(c(res[lower.tri(res)]))
}
boot.cor= boot(wisc_eval, boot.fn.cor2, 1000)

rm(list = ls())
K = c(1, )
trainMseList = list()
testMseList = list()
foldId <- sample(1:10, nrow(wisc_eval), replace = TRUE)

testMse = matrix(NA, 10, 3)


for (cc in 1:10) {
  cat("Fold: ", cc, "\n")
  testIdx <- which(foldId == cc)
  trainIdx <- which(foldId != cc)
  
  testdf = wisc_eval[testIdx, ]
  traindf = wisc_eval[trainIdx, ]
  
  mdl1 <- lm(as.numeric(diagnosis == "M") ~ log_texture_mean + log_smoothness_mean + log_compactness_mean, data = wisc)
  mdl2 <- lm(as.numeric(diagnosis == "M") ~ log_texture_mean + log_smoothness_mean + log_compactness_mean + log_texture_mean:log_smoothness_mean +log_texture_mean:log_compactness_mean + log_smoothness_mean:log_compactness_mean)
  mdl3 <- lm(as.numeric(diagnosis == "M") ~ log_texture_mean + log_smoothness_mean + log_compactness_mean + I(log_texture_mean)^2 + I(log_smoothness_mean)^2 + I(log_compactness_mean)^2 + log_texture_mean:log_smoothness_mean + log_texture_mean:log_compactness_mean + log_smoothness_mean:log_compactness_mean)
  
  testMse[cc, 1] = mean((testdf$rating - predict(mdl1, testdf))^2)
  testMse[cc, 2] = mean((testdf$rating - predict(mdl2, testdf))^2)
  testMse[cc, 3] = mean((testdf$rating - predict(mdl3, testdf))^2)
}

colMeans(testMse)



#Question 2.4
#logistic regression confusion matrix
wisc_eval <- read.csv(file= "wisc_eval.csv")
logistic.fit <- glm(das.numeric(diagnosis == "M") ~ log_texture_mean + log_smoothness_mean + log_compactness_mean, data = wisc, family = binomial)
summary(logistic.fit)
tab3 <- table(logistic.pred1, wisc_eval$diagnosis)
tab <- tab3
colnames(tab3) <- c("true-1", "true-2", "true-3")
rownames(tab3) <- c("pred-1", "pred-2", "pred-3")
conf1 <- matrix(NA, 2, 2)
colnames(conf1) <- c("true-1", "true-0")
rownames(conf1) <- c("pred-1", "pred-0")
conf1[1, 1] <- tab3[1, 1]
conf1[1, 2] <- tab3[1, 2] + tab3[1, 3]
conf1[2, 1] <- tab3[2, 1] + tab3[3, 1]
conf1[2, 2] <- tab3[2, 2] + tab3[2, 3] + tab3[3, 2] + tab3[3, 3]

#LDA
lda.fit <- lda(as.numeric(diagnosis == "M") ~ log_texture_mean + log_smoothness_mean + log_compactness_mean, data = wisc)
lda.pred <- predict(lda.fit, wisc_eval)
lda.prob <- lda.pred$posterior
lda.class <- lda.pred$class
lda.tab3 <- table(lda.class, wisc_eval$diagnosis)
lda.tab <- lda.tab3
colnames(lda.tab3) <- c("true-1", "true-2", "true-3")
rownames(lda.tab3) <- c("pred-1", "pred-2", "pred-3")
confusionMatrix(lda.tab)

#QDA
qda.fit <- qda(as.numeric(diagnosis == "M") ~ log_texture_mean + log_smoothness_mean + log_compactness_mean, data = wisc)
qda.pred <- predict(qda.fit, wisc_eval)
qda.prob <- qda.pred$posterior
qda.class <- qda.pred$diagnosis
qda.tab3 <- table(qda.class, qualityTest$category)
qda.tab <- qda.tab3
colnames(qda.tab3) <- c("true-1", "true-2", "true-3")
rownames(qda.tab3) <- c("pred-1", "pred-2", "pred-3")
confusionMatrix(qda.tab)

#KNN
knn1.class <- knn(train = as.matrix(qualityTrain[ , 2:3]),
                  test = as.matrix(qualityTest[ , 2:3]),
                  cl = qualityTrain$category, k = 1)
knn1.tab3 <- table(knn1.class, wisc_eval$diagnosis)
knn1.tab <- knn1.tab3
colnames(knn1.tab3) <- c("true-1", "true-2", "true-3")
rownames(knn1.tab3) <- c("pred-1", "pred-2", "pred-3")
confusionMatrix(knn1.tab)


knn5.class <- knn(train = as.matrix(qualityTrain[ , 2:3]),
                  test = as.matrix(qualityTest[ , 2:3]),
                  cl = qualityTrain$category, k = 5)
knn5.tab3 <- table(knn1.class, wisc_eval$diagnosis)
knn5.tab <- knn5.tab3
colnames(knn5.tab3) <- c("true-1", "true-2", "true-3")
rownames(knn5.tab3) <- c("pred-1", "pred-2", "pred-3")
confusionMatrix(knn5.tab)

knn10.class <- knn(train = as.matrix(qualityTrain[ , 2:3]),
                  test = as.matrix(qualityTest[ , 2:3]),
                  cl = qualityTrain$category, k = 10)
knn10.tab3 <- table(knn1.class, wisc_eval$diagnosis)
knn10.tab <- knn5.tab3
colnames(knn10.tab3) <- c("true-1", "true-2", "true-3")
rownames(knn10.tab3) <- c("pred-1", "pred-2", "pred-3")
confusionMatrix(knn10.tab)



