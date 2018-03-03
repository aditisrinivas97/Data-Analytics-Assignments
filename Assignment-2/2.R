d <- read.csv("pokemon.csv")
ptype <- unique(d$Type_1)

attributes <- c("HP", "Attack", "Defense", "Sp_Atk", "Sp_Def", "Speed")
for(a in attributes) {										#Range normalization for given attributes
	d[paste("N", a)] <- ((d[a] - min(d[a]))/(max(d[a]) - min(d[a])))
	#normalization
}

means <- list()
for(type in ptype) {					#find mean of each attribute for each type
	means[[type]] <- list()
	for(attribute in attributes) {
		means[[type]][[paste("N", attribute)]] <- mean(subset(d, Type_1 == type)[[paste("N", attribute)]])
	}
}

extract <- function(l) {		#function to extract items of list
	ret <- c()
	for(a in attributes) {
		item <- l[[paste("N", a)]]
		ret <- c(ret, item)
	}
	return(ret)
}

dist <- sum((as.vector(extract(means[["Grass"]])) - as.vector(extract(means[["Fire"]])))^2)^0.5		#distance between means
paste("Distance between means :", dist)

pdf(file = "range_transform.pdf")
for(type in ptype) {			#plot graphs
	plot_data <- c()
	for(a in attributes) {
		plot_data <- c(plot_data, means[[type]][[paste("N", a)]])
	}
	plot(1:length(plot_data), plot_data, type = "o", xlab = "Attributes", main = type, xaxt="n")
	axis(1, at=c(1:length(plot_data)), labels = attributes, las=2)
	#print(plot_data)
}

#install.packages("moments")
library(moments)
cat("Skewness of height:", skewness(d$Height_m), "\n")
cat("Skewness of weight:", skewness(d$Weight_kg), "\n")
cat("Kurtosis of height:", kurtosis(d$Height_m), "\n")
cat("Kurtosis of weight:", kurtosis(d$Weight_kg), "\n")

i = 0
d <- read.csv("pokemon.csv")
d <- na.omit(d)
attributes <- c("HP", "Attack", "Defense", "Sp_Atk", "Sp_Def", "Speed", "Total", "Height_m", "Weight_kg") #9 attributes
for(a in attributes) {										#normalization for given attributes
	d[paste("Z", a)] <- ((d[[a]] - mean(d[[a]]))/(sd(d[[a]])))
}
color = c('blue', 'green', 'pink', 'red', 'cyan', 'aquamarine', 'cadetblue', 'chocolate4', 'darkgoldenrod1') #9 colours
pdf(file = "normalized.pdf")
for(a in attributes) {
	if(i == 0){
		plot(density(d[[paste("Z", a)]]), main = "Transformed Values")
	}
	else{
		lines(density(d[[paste("Z", a)]]), col = color[i])
	}
	i = i + 1
}