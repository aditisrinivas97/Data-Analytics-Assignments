d <- read.csv("ipl/deliveries.csv")		#read csv data
ret <- data.frame(
	match_id = c(),
	inning = c(),
	batting_team = c(),
	total_runs = c()
)									#make data frame to return
ids = unique(d$match_id)			#get all unique IDs
for(i in ids) {
	s <- subset(d, match_id == i)			#get data of each match
	innings = unique(s$inning)				#get all innings played
	for(inn in innings) {
		ii <- subset(s, inning == inn)				#get all runs scored in each inning
		teams = unique(ii$batting_team)				#get all unique teams
		for(team in teams) {
			t <- subset(ii, batting_team == team)
			tot <- 0
			for(run in t$total_runs) {				#find total runs scored in each inning by each team
				tot <- tot + run
			}
			temp <- data.frame(						#temporary data frame
				match_id = c(i),
				inning = c(inn),
				batting_team = c(team),
				total_runs = c(tot)
			)
			ret <- rbind(ret, temp)					#merge with final data frame
		}
	}
}

print(subset(ret, match_id %in% c(7, 27, 67, 171, 414)))		#print required subset
d <- read.csv("ipl/matches.csv")
not_considered <- unique(subset(d, dl_applied != 0 | (result != "normal" & result != "tie"))$id)	#get IDs of matches not to be considered

tempset <- subset(ret, !(match_id %in% not_considered))		#get all matches that are to be considered
set <- data.frame(
	match_id = c(),
	batting_team = c(),
	total_runs = c()
)
ids = unique(tempset$match_id)
for(i in ids) {
	s <- subset(tempset, match_id == i)				#get runs scored in each match
	teams = unique(s$batting_team)
	for(team in teams) {
		t <- subset(s, batting_team == team)		#get runs scored by each team in each match
		tot <- 0
		for(run in t$total_runs) {					#get total runs scored by each team in each match
			tot <- tot + run
		}
		temp <- data.frame(
			match_id = c(i),
			batting_team = c(team),
			total_runs = c(tot)
		)
		set <- rbind(set, temp)						#merge temporary and final data sets
	}
}
paste("")
paste("Team that scored least number of runs in a match :")
print(subset(set, total_runs == min(set$total_runs)))
paste("")
paste("Team that scored most number of runs in a match :")
print(subset(set, total_runs == max(set$total_runs)))