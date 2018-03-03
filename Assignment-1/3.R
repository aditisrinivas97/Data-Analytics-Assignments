d <- read.csv("ipl/deliveries.csv")
players <- c("AB de Villiers", "MS Dhoni", "RA Jadeja", "SK Raina", "V Kohli")		#players to analyze
d <- subset(d, batsman %in% players)				#get data of required players

dat <- data.frame(
	batsman = c(),
	total_runs = c(),
	mean = c(),
	median = c(),
	mode = c(),
	q1 = c(),
	q3 = c(),
	iqr = c(),
	sdv = c()
)
for(player in players) {
	sub <- subset(d, batsman == player)				#get data of each player
	matches <- unique(sub$match_id)
	s <- c()
	for(match in matches) {
		tot <- sum(subset(sub, match_id == match)$total_runs)		#get total runs scored by the player in each match
		s <- c(s, tot)											#list of total runs scored by a player in each match
	}
	s <- sort(s)
	pos <- 0.25 * length(s)							#find quartiles
	tq1 <- 0
	if(pos == as.integer(pos)) {
		tq1 <- mean(c(s[c(pos)], s[c(pos+1)]))
	}
	else {
		tq1 <- s[c(ceiling(pos))]
	}
	pos <- 0.75 * length(s)							#find 3rd quartile
	tq3 <- 0
	if(pos == as.integer(pos)) {
		tq3 <- mean(c(s[c(pos)], s[c(pos+1)]))
	}
	else {
		tq3 <- s[c(ceiling(pos))]
	}
	m = table(s)				#to find mode, make table of frequencies and select the highest frequency element
	temp <- data.frame(
		batsman = c(player),
		total_runs = sum(s),
		mean = mean(s),
		median = median(s),
		mode = names(m[which.max(m)]),
		q1 = tq1,
		q3 = tq3,
		iqr = tq3 - tq1,
		sdv = sd(s)
	)
	dat <- rbind(dat, temp)
}
print(dat)