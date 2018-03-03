d <- read.csv("ipl/deliveries.csv")			#read CSV data
s <- subset(d, match_id == 577 & batting_team == "Sunrisers Hyderabad")		#filter for required data
tot <- 0
for(item in s$total_runs) {
	tot <- tot + item			#calculate total runs
}
paste("Target:", tot)
paste("Required run rate:", tot/20)		#display output

s <- subset(d, match_id == 577 & batting_team == "Royal Challengers Bangalore")		#filter for required data
overs <- unique(s$over)			#get overs
ret <- c()
for(o in overs) {
	temp <- subset(s, over == o)	#get runs scored in each over
	tot <- 0
	for(r in temp$total_runs) {		#find total runs scored in each over
		tot <- tot + r
	}
	ret <- c(ret, tot)
}
png(file = "runs.png")
barplot(ret, main = "Runs scored by RCB in each over", xlab = "Runs in an over", col = "blue")	#make barplot
dev.off()