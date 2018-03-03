data = read.csv("demonetization.csv", stringsAsFactors = FALSE) # read input from csv file

#install.packages("plotly")
#install.packages("tidyr")
#install.packages("dplyr")
#install.packages("tm")
#install.packages("SnowballC")
#install.packages("wordcloud")

library(plotly)
library(tidyr)
library(dplyr)
library(tm)
library(SnowballC)
library(RColorBrewer)
library(wordcloud)

tweets = data$text    # consider the text column
tweetcorpus = Corpus(VectorSource(tweets))  # create a corpus

tweetcorpus <- tm_map(tweetcorpus, stripWhitespace) # remove white space 
tweetcorpus <- tm_map(tweetcorpus, tolower)         # convert text to lower case
tweetcorpus <- tm_map(tweetcorpus, removeNumbers)   # remove all numbers
tweetcorpus <- tm_map(tweetcorpus, removePunctuation) # remove punctuation
tweetcorpus <- tm_map(tweetcorpus, removeWords, stopwords('english')) # remove stop words
tweetcorpus <- gsub("[[:digit:]]", "", tweetcorpus) # remove digits
tweetcorpus <- Corpus(VectorSource(tweetcorpus))    
tweetcorpus <- tm_map(tweetcorpus, PlainTextDocument)
tweetcorpus <- Corpus(VectorSource(tweetcorpus))


wordcloud(tweetcorpus, max.words = 300, random.order = FALSE, colors=brewer.pal(8,"Dark2")) # plot a wordcloud


source <- data %>%            # creates a donut chart to show the different sources of tweets
  group_by(statusSource) %>%
  summarize(count = n()) %>%
  plot_ly(labels = ~statusSource, values = ~count) %>%
  add_pie(hole = 0.3) %>%
  layout(title = "Sources of tweets",  showlegend = T,
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
source
