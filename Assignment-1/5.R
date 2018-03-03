d <- read.csv("ipl/deliveries.csv")
s1 <- subset(d, batsman == "SK Raina" & batting_team == "Gujarat Lions")	#get data of matches played by SK Raina for Gujarat Lions
s2 <- subset(d, batting_team == "Gujarat Lions")							#get data of all matches played by Gujarat Lions
x <- c()
y <- c()
matches <- unique(s1$match_id)
for(match in matches) {
	x <- c(x, sum(subset(s2, match_id == match)$total_runs))	#find total runs by gujarat lions
	y <- c(y, sum(subset(s1, match_id == match)$total_runs))	#find total runs scored by SK Raina
	#print(paste(match, x[length(x)], y[length(y)]))
}
png(file = "raina.png")
plot(x = x, y = y, xlab = "Gujarat Lions", ylab = "SK Raina")	#make scatter plot to see if there is a corelation
dev.off()