#install.packages("caret")
#install.packages("e1071")
library(caret)
library(e1071)

fBetaMeasure <- function(beta, p, r) {		#function to find F-beta Measure
	ret <- (1 + (beta*beta)) * p * r
	temp <- (beta * beta * p) + r
	ret <- ret / temp

	return(ret)
}

d <- read.csv("cancer_tailored.csv")	#import data
d <- d[complete.cases(d), ]				#drop rows with missing data

if(FALSE) {			#print scatter plot matrix of all 30 attributes
	td <- d[, !names(d) %in% c("diagnosis", "predicted")]
	for(i in names(td)) {
		for(j in names(td)) {
			if(i != j) {
				cor(d[c(i, j)], use = "pairwise.complete.obs")
			}
		}
	}
}

drops <- c('area_se', 'perimeter_worst', 'concave.points_mean', 'concavity_mean', 'perimeter_mean', 'area_mean', 'compactness_se', 'radius_se', 'smoothness_mean', 'fractal_dimension_mean', 'compactness_worst', 'concavity_worst', 'radius_worst', 'texture_mean', 'concave.points_se')
nd <- d[, !names(d) %in% drops]

png(file = "test.png", height = 10000, width = 10000, units = "px", pointsize = 12)	#print scatter plot matrix of remaining attribtues
pairs(nd[1:nrow(nd),])
dev.off()

partb <- d[c("predicted", "diagnosis")]		#get columns required

if(FALSE) {		#manually make confusion matrix
	m <- matrix(c(0,0,0,0), nrow = 2, ncol = 2)
	rownames(m) <- c("Actually M", "Actually B")
	colnames(m) <- c("Predicted M", "Predicted B")
	for(i in 1:nrow(partb)) {
		if(partb[["predicted"]][i] == partb[["diagnosis"]][i]) {
			if(partb[["predicted"]][i] == "M") {
				m[1, 1] <- m[1, 1] + 1
			}
			else {
				m[2, 2] <- m[2, 2] + 1
			}
		}
		else {
			if(partb[["predicted"]][i] == "M") {
				m[1, 2] <- m[1, 2] + 1
			}
			else {
				m[2, 1] <- m[2, 1] + 1
			}
		}
	}
	m

	print(paste("True Positive = ", m[2, 2]))
	print(paste("True Negative = ", m[1, 1]))
	print(paste("False Positive = ", m[2, 1]))
	print(paste("False Negative = ", m[1, 2]))
}

#make confusion matrix and print required values
m <- confusionMatrix(partb$predicted, positive = "M", reference = partb$diagnosis, dnn = c("Prediction", "Diagnosis"))
m$table
print(paste("True Positive = ", as.character(m$table[2, 2])))
print(paste("True Negative = ", as.character(m$table[1, 1])))
print(paste("False Positive = ", as.character(m$table[2, 1])))
print(paste("False Negative = ", as.character(m$table[1, 2])))
print(paste("Accuracy = ", as.character(m$overall[['Accuracy']])))
print(paste("Miscalculation Rate = ", as.character(1 - m$overall[['Accuracy']])))
print(paste("Recall = ", as.character(m$byClass[['Recall']])))
print(paste("Precision = ", as.character(m$byClass[['Precision']])))
print(paste("Specificity = ", as.character(m$byClass[['Specificity']])))
print(paste("F Score (beta = 2) = ", as.character(fBetaMeasure(2, m$byClass[['Precision']], m$byClass[['Recall']]))))
print(paste("F Score (beta = 0.5) = ", as.character(fBetaMeasure(0.5, m$byClass[['Precision']], m$byClass[['Recall']]))))

#pairs of variables considered for dropping
s_ <- "corelated with r > 0.75 :
area_se perimeter_se
area_se perimeter_worst
area_se area_worst
area_se radius_worst
fractal_dimension_worst fractal_dimension_mean
concave.points_worst concavity_worst
compactness_mean concavity_worst
compactness_mean concave.points_mean
compactness_mean compactness_worst
compactness_mean concavity_mean
fractal_dimension_se compactness_se
concavity_se compactness_se
concave.points_worst concave.points_mean
texture_worst texture_mean
concave.points_worst radius_worst
smoothness_worst smoothness_mean
radius_mean area_worst
radius_mean concave.points_mean
radius_mean radius_worst
radius_mean perimeter_worst
radius_mean area_mean
radius_mean perimeter_mean
area_se area_mean
concave.points_worst perimeter_mean
concave.points_worst perimeter_worst
concave.points_worst compactness_worst
fractal_dimension_worst compactness_worst
concave.points_worst concavity_mean
radius_se perimeter_se
radius_se area_worst
concavity_se concave.points_se
Dont drop 10 : {'fractal_dimension_se', 'smoothness_worst', 'concave.points_worst', 'area_se', 'texture_worst', 'concavity_se', 'radius_mean', 'compactness_mean', 'radius_se', 'fractal_dimension_worst'}
Drop 15 : {'concavity_mean', 'smoothness_mean', 'area_worst', 'concavity_worst', 'perimeter_se', 'concave.points_mean', 'compactness_worst', 'texture_mean', 'radius_worst', 'area_mean', 'perimeter_mean', 'concave.points_se', 'perimeter_worst', 'fractal_dimension_mean', 'compactness_se'}
Intersection set()
"