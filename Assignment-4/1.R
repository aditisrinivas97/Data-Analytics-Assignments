data = read.csv("cancer_2015.csv")

data$Outpatients.TMH = ifelse(is.na(data$Outpatients.TMH), mean(data$Outpatients.TMH, na.rm = TRUE), data$Outpatients.TMH)
data$New.Registrations.ACTREC = ifelse(is.na(data$New.Registrations.ACTREC), mean(data$New.Registrations.ACTREC, na.rm = TRUE), data$New.Registrations.ACTREC)
data$Laboratory.Investigations.ACTREC = ifelse(is.na(data$Laboratory.Investigations.ACTREC), mean(data$Laboratory.Investigations.ACTREC, na.rm = TRUE), data$Laboratory.Investigations.ACTREC)

fillmean = data

data = read.csv("cancer_2015.csv")

attributes = c("Outpatients.TMH","New.Registrations.ACTREC","Laboratory.Investigations.ACTREC")

for(attribute in attributes){
  d = subset(data, select=c(attribute))
  y1 = mean(d[3,], d[8,])
  y2 = mean(d[3,], d[8,])
  y3 = mean(d[3,], d[8,])
  y4 = mean(d[3,], d[8,])
  i = 1
  for(x in 1:nrow(data)){
    if(is.na(data[x,attribute]) == TRUE){
      val = paste("y",i,sep ='')
      data[x,attribute] = get(val)
      i = i + 1
    }
  }
}

linear_interpolation = data

data = read.csv("cancer_2015.csv")

for(attribute in attributes){
  d = subset(data, select=c(attribute))
  mat1 = matrix(c(4, 9, 64, 2, 3, 8, 1, 1, 1), nrow = 3, ncol = 3)
  mat2 = matrix(c(d[2,], d[3,], d[8,]), nrow = 3, ncol = 1)
  ans = solve(mat1, mat2)
  y1 = ans[1,1]*(16) + ans[2,1]*(4) + ans[3,1]
  y2 = ans[1,1]*(25) + ans[2,1]*(5) + ans[3,1]
  y3 = ans[1,1]*(36) + ans[2,1]*(6) + ans[3,1]
  y4 = ans[1,1]*(49) + ans[2,1]*(7) + ans[3,1]
  i = 1
  for(x in 1:nrow(data)){
    if(is.na(data[x,attribute]) == TRUE){
      val = paste("y",i,sep ='')
      data[x,attribute] = get(val)
      i = i + 1
    }
  }
}

quadratic_interpolation = data


data = read.csv("cancer_2015.csv")
data$Month = c(1,2,3,4,5,6,7,8,9,10,11,12)

#png(file = "test.png", height = 1000, width = 1000, units = "px", pointsize = 12)
#pairs(data[1:nrow(data),])
#dev.off()

regmod = lm(Outpatients.TMH ~ Month + Laboratory.Investigations.TMH, data = data)
val1 = predict(regmod, data.frame(Month = 4, Laboratory.Investigations.TMH = data$Laboratory.Investigations.TMH[4]))
val2 = predict(regmod, data.frame(Month = 5, Laboratory.Investigations.TMH = data$Laboratory.Investigations.TMH[5]))
val3 = predict(regmod, data.frame(Month = 6, Laboratory.Investigations.TMH = data$Laboratory.Investigations.TMH[6]))
val4 = predict(regmod, data.frame(Month = 7, Laboratory.Investigations.TMH = data$Laboratory.Investigations.TMH[7]))
i = 1
for(x in 1:nrow(data)){
  if(is.na(data[x,"Outpatients.TMH"]) == TRUE){
    v = paste("val",i,sep ='')
    data[x,"Outpatients.TMH"] = get(v)
    i = i + 1
  }
}

regmod = lm(New.Registrations.ACTREC ~ No..of.surgeries.TMH + Outpatients.ACTREC + No..of.surgeries.TMC.Total, data = data)
val1 = predict(regmod, data.frame(No..of.surgeries.TMH = data$No..of.surgeries.TMH[4], Outpatients.ACTREC = data$Outpatients.ACTREC[4], No..of.surgeries.TMC.Total = data$No..of.surgeries.TMC.Total[4]))
val2 = predict(regmod, data.frame(No..of.surgeries.TMH = data$No..of.surgeries.TMH[5], Outpatients.ACTREC = data$Outpatients.ACTREC[5], No..of.surgeries.TMC.Total = data$No..of.surgeries.TMC.Total[5]))
val3 = predict(regmod, data.frame(No..of.surgeries.TMH = data$No..of.surgeries.TMH[6], Outpatients.ACTREC = data$Outpatients.ACTREC[6], No..of.surgeries.TMC.Total = data$No..of.surgeries.TMC.Total[6]))
val4 = predict(regmod, data.frame(No..of.surgeries.TMH = data$No..of.surgeries.TMH[7], Outpatients.ACTREC = data$Outpatients.ACTREC[7], No..of.surgeries.TMC.Total = data$No..of.surgeries.TMC.Total[7]))
print(paste(val1, val2, val3, val4))
i = 1
for(x in 1:nrow(data)){
  if(is.na(data[x,"New.Registrations.ACTREC"]) == TRUE){
    v = paste("val",i,sep ='')
    data[x,"New.Registrations.ACTREC"] = get(v)
    i = i + 1
  }
}

regmod = lm(Laboratory.Investigations.ACTREC ~ New.Registrations.TMC.Total + New.Registrations.TMH, data = data)
val1 = predict(regmod, data.frame(New.Registrations.TMC.Total= data$New.Registrations.TMC.Total[4], New.Registrations.TMH = data$New.Registrations.TMH[4]))
val2 = predict(regmod, data.frame(New.Registrations.TMC.Total= data$New.Registrations.TMC.Total[5], New.Registrations.TMH = data$New.Registrations.TMH[5]))
val3 = predict(regmod, data.frame(New.Registrations.TMC.Total= data$New.Registrations.TMC.Total[6], New.Registrations.TMH = data$New.Registrations.TMH[6]))
val4 = predict(regmod, data.frame(New.Registrations.TMC.Total= data$New.Registrations.TMC.Total[7], New.Registrations.TMH = data$New.Registrations.TMH[7]))
print(paste(val1, val2, val3, val4))
i = 1
for(x in 1:nrow(data)){
  if(is.na(data[x,"Laboratory.Investigations.ACTREC"]) == TRUE){
    v = paste("val",i,sep ='')
    data[x,"Laboratory.Investigations.ACTREC"] = get(v)
    i = i + 1
  }
}
linear_regression = data


data = read.csv("cancer_2015.csv")

library("mice")

temp_data = mice(data,m=5,maxit=50,meth='pmm',seed=500)

mice1 <- complete(temp_data,1)
mice2 <- complete(temp_data,2)
mice3 <- complete(temp_data,3)
mice4 <- complete(temp_data,4)
mice5 <- complete(temp_data,5)

#install.packages("plotly")
data = read.csv("cancer_2015.csv")

anames = c("Mean fill", "Linear Interpolation", "Quadratic Interpolation", "Linear Regression", "Mice 1", "Mice 2", "Mice 3", "Mice 4", "Mice 5")

library(plotly)

data$Month <- factor(data$Month, levels=unique(data$Month)) 

graphs = data.frame("months" = data$Month, "mean_fill" = fillmean$Outpatients.TMH, "linearint_fill" = linear_interpolation$Outpatients.TMH, "quadratic_fill" = quadratic_interpolation$Outpatients.TMH, "regression_fill" = linear_regression$Outpatients.TMH, "mice1_fill" = mice1$Outpatients.TMH, "mice2_fill" = mice2$Outpatients.TMH, "mice3_fill" = mice3$Outpatients.TMH, "mice4_fill" = mice4$Outpatients.TMH, "mice5_fill" = mice5$Outpatients.TMH)

graph <- plot_ly(graphs, x = ~graphs$months) %>%
  add_lines(y = ~graphs$mean_fill, name = anames[1], line = list(color = 'rgb(120, 96, 10)'))%>%
  add_lines(y = ~graphs$linearint_fill, name = anames[2], line = list(color = 'rgb(225, 96, 167)'))%>%
  add_lines(y = ~graphs$quadratic_fill, name = anames[3], line = list(color = 'rgb(22, 196, 167)'))%>%
  add_lines(y = ~graphs$regression_fill, name = anames[4], line = list(color = 'rgb(22, 100, 100)'))%>%
  add_lines(y = ~graphs$mice1_fill, name = anames[5], line = list(color = 'rgb(255, 0, 80)'))%>%
  add_lines(y = ~graphs$mice2_fill, name = anames[6], line = list(color = 'rgb(0, 255, 0)'))%>%
  add_lines(y = ~graphs$mice3_fill, name = anames[7], line = list(color = 'rgb(120, 96, 10)'))%>%
  add_lines(y = ~graphs$mice4_fill, name = anames[8], line = list(color = 'rgb(0, 0, 255)'))%>%
  add_lines(y = ~graphs$mice4_fill, name = anames[9], line = list(color = 'rgb(255, 198, 0)'))%>%
  layout(title = "Outpatients.TMH",
         xaxis = list(title = "Months"),
         yaxis = list (title = ""))
graph

graph <- plot_ly(graphs, x = ~graphs$months) %>%
  add_lines(y = ~graphs$mean_fill, name = anames[1], line = list(color = 'rgb(120, 96, 10)'))%>%
  add_lines(y = ~graphs$linearint_fill, name = anames[2], line = list(color = 'rgb(225, 96, 167)'))%>%
  add_lines(y = ~graphs$quadratic_fill, name = anames[3], line = list(color = 'rgb(22, 196, 167)'))%>%
  add_lines(y = ~graphs$regression_fill, name = anames[4], line = list(color = 'rgb(22, 100, 100)'))%>%
  add_lines(y = ~graphs$mice1_fill, name = anames[5], line = list(color = 'rgb(255, 0, 80)'))%>%
  add_lines(y = ~graphs$mice2_fill, name = anames[6], line = list(color = 'rgb(0, 255, 0)'))%>%
  add_lines(y = ~graphs$mice3_fill, name = anames[7], line = list(color = 'rgb(120, 96, 10)'))%>%
  add_lines(y = ~graphs$mice4_fill, name = anames[8], line = list(color = 'rgb(0, 0, 255)'))%>%
  add_lines(y = ~graphs$mice4_fill, name = anames[9], line = list(color = 'rgb(255, 198, 0)'))%>%
  layout(title = "New.Registrations.ACTREC",
         xaxis = list(title = "Months"),
         yaxis = list (title = ""))
graph

graph <- plot_ly(graphs, x = ~graphs$months) %>%
  add_lines(y = ~graphs$mean_fill, name = anames[1], line = list(color = 'rgb(120, 96, 10)'))%>%
  add_lines(y = ~graphs$linearint_fill, name = anames[2], line = list(color = 'rgb(225, 96, 167)'))%>%
  add_lines(y = ~graphs$quadratic_fill, name = anames[3], line = list(color = 'rgb(22, 196, 167)'))%>%
  add_lines(y = ~graphs$regression_fill, name = anames[4], line = list(color = 'rgb(22, 100, 100)'))%>%
  add_lines(y = ~graphs$mice1_fill, name = anames[5], line = list(color = 'rgb(255, 0, 80)'))%>%
  add_lines(y = ~graphs$mice2_fill, name = anames[6], line = list(color = 'rgb(0, 255, 0)'))%>%
  add_lines(y = ~graphs$mice3_fill, name = anames[7], line = list(color = 'rgb(120, 96, 10)'))%>%
  add_lines(y = ~graphs$mice4_fill, name = anames[8], line = list(color = 'rgb(0, 0, 255)'))%>%
  add_lines(y = ~graphs$mice4_fill, name = anames[9], line = list(color = 'rgb(255, 198, 0)'))%>%
  layout(title = "Laboratory.Investigations.ACTREC",
         xaxis = list(title = "Months"),
         yaxis = list (title = ""))
graph


print("Mean fill")
print(fillmean[4:7, c("Outpatients.TMH","New.Registrations.ACTREC","Laboratory.Investigations.ACTREC")])
print("Linear Interpolation")
print(linear_interpolation[4:7, c("Outpatients.TMH","New.Registrations.ACTREC","Laboratory.Investigations.ACTREC")])
print("Quadratic Interpolation")
print(quadratic_interpolation[4:7, c("Outpatients.TMH","New.Registrations.ACTREC","Laboratory.Investigations.ACTREC")])
print("Multiple Linear Regression")
print(linear_regression[4:7, c("Outpatients.TMH","New.Registrations.ACTREC","Laboratory.Investigations.ACTREC")])
print("Mice 1")
print(mice1[4:7, c("Outpatients.TMH","New.Registrations.ACTREC","Laboratory.Investigations.ACTREC")])
print("Mice 2")
print(mice2[4:7, c("Outpatients.TMH","New.Registrations.ACTREC","Laboratory.Investigations.ACTREC")])
print("Mice 3")
print(mice3[4:7, c("Outpatients.TMH","New.Registrations.ACTREC","Laboratory.Investigations.ACTREC")])
print("Mice 4")
print(mice4[4:7, c("Outpatients.TMH","New.Registrations.ACTREC","Laboratory.Investigations.ACTREC")])
print("Mice 5")
print(mice5[4:7, c("Outpatients.TMH","New.Registrations.ACTREC","Laboratory.Investigations.ACTREC")])


abs_errors1 = data.frame("Method" = c("Mean","Linear_Interpolation","Quadratic_Interpolation", "Linear_Regression"), "April" = c(0,0,0,0), "May" = c(0,0,0,0), "June" = c(0,0,0,0), "July" = c(0,0,0,0), "Average" = c(0,0,0,0))

avg_abserrors <- data.frame("methods/attributes" = c("Mean","Linear_Interpolation","Quadratic_Interpolation", "Linear_Regression"))
for(a in attributes) {
  avg_abserrors[a] <- c(0, 0, 0, 0)
}

for(a in attributes) {
  print(a)
  for(x in 1:4){
    abs_errors1$April[x] = abs(fillmean[[a]][x+3] - mice1[[a]][x+3])
    abs_errors1$May[x] = abs(linear_interpolation[[a]][x+3] - mice1[[a]][x+3])
    abs_errors1$June[x] = abs(quadratic_interpolation[[a]][x+3] - mice1[[a]][x+3])
    abs_errors1$July[x] = abs(linear_regression[[a]][x+3] - mice1[[a]][x+3])
    abs_errors1$Average[x] = mean(c(abs_errors1$April[x],abs_errors1$May[x],abs_errors1$June[x],abs_errors1$July[x]))
    avg_abserrors[[a]][x] = avg_abserrors[[a]][x] + abs_errors1$Average[x]
  }
  print(abs_errors1)
  avg_abserrors[[a]][x] = avg_abserrors[[a]][x] / 4
}
avg_abserrors