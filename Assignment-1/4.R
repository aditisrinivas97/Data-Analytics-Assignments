d <- read.csv("ipl/deliveries.csv")
players <- c("MS Dhoni", "V Kohli")				#players to consider
d <- subset(d, batsman %in% players)			#get data of required players

dat <- data.frame(
	runs = c(),
	players = c()
)
perc <- data.frame(
	player = c(),
	percentile90 = c()
)
for(player in players) {
	sub <- subset(d, batsman == player)				#get data of one player at a time
	matches <- unique(sub$match_id)
	s <- c()
	for(match in matches) {
		tot <- sum(subset(sub, match_id == match)$total_runs)	#get total runs scored by each player in each match
		s <- c(s, tot)
	}
	dat <- rbind(dat, data.frame(
		runs = s,
		players = c(player)
	))

	perc <- rbind(perc, data.frame(player = player, percentile90 = unname(quantile(s, 0.9))))	#get 90th percentile score
}
png(file = "box.png")
boxplot(runs ~ players, data = dat, xlab = "Player", ylab = "Runs", main = "Runs")		#plot the boxplot with both players' data
dev.off()
print(perc)