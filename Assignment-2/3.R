data = read.csv("pokemon.csv")
att = c("Total", "HP", "Attack", "Defense", "Sp_Atk", "Sp_Def", "Speed", "Height_m", "Weight_kg", "Catch_Rate")
d = data[,att]

for(a in att) {
	d[a] = d[[a]] - mean(d[[a]])	# subtract mean
}

dmatrix = as.matrix(sapply(d, as.numeric))	# convert data to matrix form
cov = (nrow(dmatrix) - 1)^-1 * t(dmatrix) %*% (dmatrix)	# get the covariance matrix
e = eigen(cov)	# get the eigen vectors

evectors = data.frame(e$vectors)	# data frame of all eigen vectors
evals = data.frame(e$values)		# data frame of all eigen values

dataperc = ((evals$e.values[1] + evals$e.values[2])/sum(evals$e.values))*100
cat("Extent of information in percentage : ",dataperc, "%\n") 

totdata = dmatrix  %*% (as.matrix(sapply(evectors, as.numeric))) # data after applying PCA (considering all 10 eigen vectors)

variances = c()
for(a in colnames(totdata)){
	vals = totdata[, a]
	variances = c(variances, (sd(vals)^2))
}	
png(file = "variances.png")		#plot variance after applying PCA
plot(variances, type = "o", xlab = "Numerical Attributes", ylab = "Variance", xaxt = "n")
axis(1, at=c(1:length(variances)), labels = colnames(d), las=2)

m1evals = e$vectors[,1]	# eigen vector with largest eigen value
m2evals = e$vectors[,2]	# eigen vector with second largest eigen value

pc = data.frame(f1 = c(m1evals), f2 = c(m2evals))	#principal components

dexdata = dmatrix  %*% (as.matrix(sapply(pc, as.numeric))) #transform data
#print(dexdata)

png(file = 'pca.png')	
plot(dexdata[,colnames(dexdata)[1]], dexdata[,colnames(dexdata)[2]])	#Plot graph of two components