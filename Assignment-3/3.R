d <- read.csv("business_rankings.csv")

#pdf(file = "3.pdf")
par(mar=c(12,4,4,2))
ranks <- c("Ease.of.Doing.Business.Rank", "Starting.a.Business", "Dealing.with.Construction.Permits", "Protecting.Minority.Investors")
#for(r in ranks) {
#	top <- subset(d, d[[r]] <= 20)
#	top <- top[order(top[[r]]),,drop = FALSE]
#	for(ind in 1:length(top[[r]])) {
#		top[r][ind,] <- 21 - top[r][ind,]
#	}
#	barplot(top[[r]], xlab = "", ylab = r, names.arg = top$Economy, las = 3, yaxt = "n", col = "blue")
#	axis(2, at = c(20:1), labels = c(1:20), las = 1)
#	mtext("Country", side=1, line=10)
#}
#dev.off()

countries <- c()
for(r in ranks) {
  top <- subset(d, d[[r]] <= 20)
  countries <- c(countries, as.character(top$Economy))
}
countries <- c(unique(countries))

data <- subset(d, Economy %in% countries)
data$Economy <- droplevels(data$Economy)

library(plotly)
#p <- plot_ly(data, x = data$Economy, y = ~Ease.of.Doing.Business.Rank, type = "bar", name = "Ease of Doing Business Rank") %>%
  #add_trace(y = ~Starting.a.Business, name = "Starting a Business") %>%
  #add_trace(y = ~Dealing.with.Construction.Permits, name = "Dealing with Construction Permits") %>%
  #add_trace(y = ~Protecting.Minority.Investors, name = "Protecting Minority Investors") %>%
  #layout(yaxis = list(title = 'Ranking'), barmode = 'stack', margin = list(b = 150))
#p

p <- plot_ly(data, x = data$Economy, y = ~Ease.of.Doing.Business.Rank, type = "bar", name = "Ease of Doing Business Rank") %>%
  add_trace(y = ~Starting.a.Business, name = "Starting a Business") %>%
  add_trace(y = ~Dealing.with.Construction.Permits, name = "Dealing with Construction Permits") %>%
  add_trace(y = ~Protecting.Minority.Investors, name = "Protecting Minority Investors") %>%
  layout(xaxis = list(title = 'Countries'), yaxis = list(title = 'Ranking'), barmode = 'group', margin = list(b = 150))
p