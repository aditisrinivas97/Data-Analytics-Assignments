library("arules")   # load library

data = read.csv("handwriting_recognition.csv")  # read csv file

ndata = data.frame("Gender" = c(), "Profession" = c(), "Recognition" = c())     # create a new df to convert to transactions

for(x in 1:nrow(data)){
  f = data$Freq[x]
  while(f != 0){
    ndata = rbind(ndata, data.frame("Gender" = c(toString(data$Gender[x])), "Profession" = c(toString(data$Profession[x])), "Recognition" = c(toString(data$Recognition[x]))))
    f = f - 1
  }
}

write.csv(ndata, file = "ndata.csv")
data = read.transactions("ndata.csv", sep = ",")    # read data as transactions

rules_default = apriori(data)   # find the deault rules
inspect(rules_default)          # print the default rules

rules_recognition = apriori(data,parameter = list( support = 0.01, confidence = 0.7, minlen = 2, maxlen = 5),appearance = list( rhs = c('Recognized', 'Unrecognized'), default = 'lhs'))    # rules which have recognition on rhs
inspect(rules_recognition)  # print the rules which have recognition on rhs

rules_gender <- apriori(data,parameter = list( support = 0.01, confidence = 0.6, minlen = 2, maxlen = 5),appearance = list( rhs = c( 'Male', 'Female'), default = 'lhs')) # rules which have gender on rhs
inspect(rules_gender)   # print the rules which have gender on rhs


# Association rules with default settings
#     lhs                    rhs            support   confidence lift     count
#[1] {Engineer}          => {Male}         0.1237022 0.9572650  1.610382 560  
#[2] {Teacher}           => {Unrecognized} 0.1475591 0.9355742  1.528453 668  
#[3] {Artist}            => {Male}         0.1822399 0.8842444  1.487542 825  
#[4] {Artist,Recognized} => {Male}         0.1130992 0.8519135  1.433152 512  

# Answers
#    lhs                    rhs            support   confidence lift     count
#[1] {Artist,Female}    => {Recognized}   0.01965982 0.8240741  2.125689  89
#[2] {Engineer}         => {Male}         0.1237022  0.9572650  1.610382  560
#[3] {Actor,Recognized} => {Female}       0.04462116 0.6273292  1.547640  202
#[4] {Doctor,Male}      => {Unrecognized} 0.03048376 0.7225131  1.180374  138  