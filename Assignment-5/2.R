# Function that returns Root Mean Squared Error
rmse = function(error){
    sqrt(mean(error^2))
}

data = read.csv("game_of_thrones_data/A5_Q2/got_viewership.csv")

# data preprocessing 
# combine season and episode number
for(x in 1:nrow(data)){
    if(data$Epsisode[x] < 10){
        data$episode_num[x] = as.double(paste(data$Season[x],data$Epsisode[x], sep=".0"))
    }
    else{
        data$episode_num[x] = as.double(paste(data$Season[x],data$Epsisode[x], sep="."))
    }
}

ndata = data[ , -which(names(data) %in% c("Epsisode", "Season", "Episode.Name"))]    # drop the season and episode column
ndata = ndata[c(1, 5, 2, 3, 4)]  # reorder the columns

regmod1 = lm(Viewership..in.million ~ Year + episode_num + Number.of.Major.Deaths + Critic.Ratings, data = ndata)   # model 1

regmod2 = lm(Viewership..in.million ~ Year + episode_num + Number.of.Major.Deaths, data = ndata)    # model 2
 

season_7 = read.csv("game_of_thrones_data/A5_Q2/got_season7_ratings.csv")
season_7$Year = 2017
season_7$Critic.Ratings = min(ndata$Critic.Ratings)
season_7$Number.of.Major.Deaths = mean(ndata$Number.of.Major.Deaths)

# combine season and episode number
for(x in 1:nrow(season_7)){
    if(season_7$episode[x] < 10){
        season_7$episode_num[x] = as.double(paste(season_7$season[x],season_7$episode[x], sep=".0"))
    }
    else{
        season_7$episode_num[x] = as.double(paste(season_7$season[x],season_7$episode[x], sep="."))
    }
}
season_7 = season_7[ , -which(names(season_7) %in% c("episode","season"))]    # drop the season and episode column
season_7 = season_7[c(2, 5, 4, 3, 1)]
names(season_7)[names(season_7) == "ratings"] = "Viewership..in.million"

# predict the viewships
for(x in 1:nrow(season_7)){
    season_7$predicted_viewership_1[x] = predict(regmod1, data.frame(Year = season_7$Year[x], episode_num = season_7$episode_num[x], Number.of.Major.Deaths = season_7$Number.of.Major.Deaths[x], Critic.Ratings = season_7$Critic.Ratings[x]))  # predict values
    season_7$predicted_viewership_2[x] = predict(regmod2, data.frame(Year = season_7$Year[x], episode_num = season_7$episode_num[x], Number.of.Major.Deaths = season_7$Number.of.Major.Deaths[x]))  # predict values
}

rmse_1 = abs(rmse(season_7$predicted_viewership_1 - season_7$Viewership..in.million))   # Root mean squared error for model 1
rmse_2 = abs(rmse(season_7$predicted_viewership_2 - season_7$Viewership..in.million))   # Root mean squared error for model 2

print(paste(rmse_1, rmse_2))    

library(plotly)

# Plot line graph for viewership
graph <- plot_ly(season_7, x = ~episode_num) %>%
  add_lines(y = ~season_7$Viewership..in.million, name = "Actual Viewership")%>%
  add_lines(y = ~season_7$predicted_viewership_1, name = "Model 1 : Predicted viewership")%>%
  add_lines(y = ~season_7$predicted_viewership_2, name = "Model 2 : Predicted viewership")%>%
  layout(title = "Viewership",
         xaxis = list(title = "Episode number"),
         yaxis = list (title = "Viewership in Millions"))
graph
