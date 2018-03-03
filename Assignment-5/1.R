#change to TRUE to install required packages
if(FALSE) {
	install.packages("caret")
	install.packages("mice")
	install.packages("e1071")
	install.packages("ROCR")
}

library(caret)
library(mice)
library(e1071)
library(ROCR)

#read CSV files
unftestd = read.csv("game_of_thrones_data/A5_Q1/got_character_test.csv")
unftraind = read.csv("game_of_thrones_data/A5_Q1/got_character_train.csv")

#attributes to consider
filter = c("book1", "book2", "book3", "book4", "book5", "male", "isAliveMother", "isAliveFather", "isAliveHeir", "isAliveSpouse", "numDeadRelations", "isNoble", "popularity", "isMarried", "age", "isAlive")

testd = unftestd[filter]
traind = unftraind[filter]

#replace missing age with median age
agemedian = median(traind$age[!is.na(traind$age)])
traind$age[is.na(traind$age)] = agemedian
testd$age[is.na(testd$age)] = median(testd$age[!is.na(testd$age)])

#replace other missing data with -1
traind[is.na(traind)] = -1
testd[is.na(testd)] = -1

#pie chart of imbalance of dead vs living characters
#png("imbalance_deaths.png")
#pie(data.frame(ftable(traind$isAlive))$Freq, labels = c("Dead", "Alive"))
#dev.off()

#pie chart after upsamling to balance dead vs living characters
up_traind = upSample(x = traind[, -ncol(traind)], y = factor(traind$isAlive))
up_traind = as.data.frame(up_traind)
colnames(up_traind) = colnames(traind)
#png("balance_deaths.png")
#pie(data.frame(ftable(up_traind$isAlive))$Freq, labels = c("Dead", "Alive"))
#dev.off()

#build model
model = glm(formula = isAlive ~., family = binomial(link='logit'), data = up_traind)
#summary(model)
#AIC = 2574.6

significant = c("isAlive", "book1", "book2", "book3", "book4", "male", "isAliveMother", "numDeadRelations", "popularity", "isMarried")


#extracting statistically significant attributes
extracted = up_traind[significant]
model2 = glm(formula = isAlive ~., family = binomial(link='logit'), data = extracted)
#summary(model2)
#AIC = 2588.1

#Second model is 0.00117088 times as probable as the first model to minimize information loss
#First model is better than second model!


#threshold probability for isAlive prediction
thresh = 0.5

#predict for test set with model 1
pAliveM1 = ifelse(predict(model, testd, type = 'response') > thresh, 1, 0)

#predict for test set with model 2
pAliveM2 = ifelse(predict(model2, testd, type = 'response') > thresh, 1, 0)

#add predicted values to test data frame
testd$pAliveM1 = pAliveM1
testd$pAliveM2 = pAliveM2

#confusion matrices for both
confMatrix1 = confusionMatrix(testd$pAliveM1, positive = "1", reference = testd$isAlive)
confMatrix2 = confusionMatrix(testd$pAliveM2, positive = "1", reference = testd$isAlive)


confMatrix1$table
confMatrix2$table


confMatrix1$overall[['Accuracy']]
confMatrix2$overall[['Accuracy']]


pred1 = prediction(testd$pAliveM1, testd$isAlive)
perf1 = performance(pred1, "tpr", "fpr")

png("ROC_model1.png")
plot(perf1)
dev.off()

pred2 = prediction(testd$pAliveM2, testd$isAlive)
perf2 = performance(pred2, "tpr", "fpr")
png("ROC_model2.png")
plot(perf2)
dev.off()