data = read.csv("flight_delay/flights.csv")             # read input from csv
airline_names = read.csv("flight_delay/airlines.csv")   # read input from csv
airlines = unique(data$AIRLINE)     
airlines = sort(airlines)       # sorted IATA_CODES of all airlines

anames = c()    # sorted names of all airlines

for(x in airlines){     # obtain names of the airlines 
    for(y in 1:nrow(airline_names)){
        if(x == toString(airline_names$IATA_CODE[y])){
            anames = c(anames, toString(airline_names$AIRLINE[y]))
            break
        }
    }
}

flights = data.frame("months" = c(), "AA" = c(), "AS" = c(), "B6" = c(), "DL" = c(), "EV" = c(), "F9" = c(), "HA" = c(), "MQ" = c(), "NK" = c(), "OO" = c(), "UA" = c(), "US" = c(), "VX" = c(), "WN" = c())    # df to hold the number of flights operating per month of each airline

udates = unique(data[c("MONTH")])   # list of all months

for(y in 1:nrow(udates)){
  temp = c()
  for(x in airlines){
    fcount = nrow(subset(data, MONTH == udates$MONTH[y] & AIRLINE == x))    # number of flights operating per month of airline "x"
    temp = c(temp, fcount)
  }
  flights = rbind(flights, data.frame("months" = c(udates$MONTH[y]), "AA" = c(temp[1]), "AS" = c(temp[2]), "B6" = c(temp[3]), "DL" = c(temp[4]), "EV" = c(temp[5]), "F9" = c(temp[6]), "HA" = c(temp[7]), "MQ" = c(temp[8]), "NK" = c(temp[9]), "OO" = c(temp[10]), "UA" = c(temp[11]), "US" = c(temp[12]), "VX" = c(temp[13]), "WN" = c(temp[14])))
}

#install.packages("plotly")

library(plotly)

flights_graph <- plot_ly(flights, x = ~flights$months) %>%    # plot a line graph
  add_lines(y = ~flights$AA, name = anames[1], line = list(color = 'rgb(120, 96, 10)'))%>%
  add_lines(y = ~flights$AS, name = anames[2], line = list(color = 'rgb(225, 96, 167)'))%>%
  add_lines(y = ~flights$B6, name = anames[3], line = list(color = 'rgb(22, 196, 167)'))%>%
  add_lines(y = ~flights$DL, name = anames[4], line = list(color = 'rgb(22, 100, 100)'))%>%
  add_lines(y = ~flights$EV, name = anames[5], line = list(color = 'rgb(255, 0, 0)'))%>%
  add_lines(y = ~flights$F9, name = anames[6], line = list(color = 'rgb(0, 255, 0)'))%>%
  add_lines(y = ~flights$HA, name = anames[7], line = list(color = 'rgb(0, 0, 255)'))%>%
  add_lines(y = ~flights$MQ, name = anames[8], line = list(color = 'rgb(255, 255, 0)'))%>%
  add_lines(y = ~flights$NK, name = anames[9], line = list(color = 'rgb(255, 0, 255)'))%>%
  add_lines(y = ~flights$OO, name = anames[10], line = list(color = 'rgb(0, 255, 255)'))%>%
  add_lines(y = ~flights$UA, name = anames[11], line = list(color = 'rgb(255, 90, 90)'))%>%
  add_lines(y = ~flights$US, name = anames[12], line = list(color = 'rgb(20, 160, 190)'))%>%
  add_lines(y = ~flights$VX, name = anames[13], line = list(color = 'rgb(222, 120, 167)'))%>%
  add_lines(y = ~flights$WN, name = anames[14], line = list(color = 'rgb(2, 200, 150)'))%>%
  layout(title = "Flights operating in the year 2015",
         xaxis = list(title = "Months"),
         yaxis = list (title = "Number of flights operating per month"))
flights_graph

print("From the graph, it is clear that American Airlines Inc.(X) merged with US Airways(Y). This can be seen in the radical increase in number of flights of AA and decrease in the number of flights of US.")  # result 

