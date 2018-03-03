library(plotly)
d <- read.csv("MER_T12_06.csv")
d <- d[c("YYYYMM", "Value", "Description")]

for(i in 1:length(d$YYYYMM)) {
  d$YYYYMM[i] <- paste(substring(d$YYYYMM[i], 1, 4), substring(d$YYYYMM[i], 5, 6), "01", sep = "-")
}
d <- subset(d, substring(YYYYMM, 6, 7) != "13")
d$YYYYMM <- as.Date(d$YYYYMM)

sectors <- as.character(unique(d$Description))
formatted_d <- subset(d, Description == sectors[1])
formatted_d <- formatted_d[c("YYYYMM", "Value")]
colnames(formatted_d) <- c("YYYYMM", sectors[1])

for(i in 2:length(sectors)) {
  formatted_d[sectors[i]] <- subset(d, Description == sectors[i])$Value
}
for(s in sectors) {
  formatted_d[[s]] = as.numeric(as.character(formatted_d[[s]]))
  for(ind in 1:length(formatted_d[[s]])) {
    if(is.na(formatted_d[s][ind,])) {
      formatted_d[s][ind,] <- 0
    }
  }
}

p <- plot_ly(formatted_d, x = ~YYYYMM) %>%
  add_lines(y = ~formatted_d[[sectors[1]]], name = "CO2") %>%
  add_lines(y = ~formatted_d[[sectors[2]]], name = "CO2", visible = F) %>%
  add_lines(y = ~formatted_d[[sectors[3]]], name = "CO2", visible = F) %>%
  add_lines(y = ~formatted_d[[sectors[4]]], name = "CO2", visible = F) %>%
  add_lines(y = ~formatted_d[[sectors[5]]], name = "CO2", visible = F) %>%
  add_lines(y = ~formatted_d[[sectors[6]]], name = "CO2", visible = F) %>%
  add_lines(y = ~formatted_d[[sectors[7]]], name = "CO2", visible = F) %>%
  add_lines(y = ~formatted_d[[sectors[8]]], name = "CO2", visible = F) %>%
  add_lines(y = ~formatted_d[[sectors[9]]], name = "CO2", visible = F) %>%
  layout(
    title = "CO2 Emissions",
    xaxis = list(domain = c(min(formatted_d$YYYYMM), max(formatted_d$YYYYMM)), rangeslider = list(type = "date", xanchor = "right")),
    yaxis = list(title = "Million Metric Tons"),
    updatemenus = list(
      list(
        y = 0.8,
        buttons = list(
          list(method = "restyle",
               args = list("visible", list(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)),
               label = sectors[1]),
          list(method = "restyle",
               args = list("visible", list(FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)),
               label = sectors[2]),
          list(method = "restyle",
               args = list("visible", list(FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)),
               label = sectors[3]),
          list(method = "restyle",
               args = list("visible", list(FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE)),
               label = sectors[4]),
          list(method = "restyle",
               args = list("visible", list(FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE)),
               label = sectors[5]),
          list(method = "restyle",
               args = list("visible", list(FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE)),
               label = sectors[6]),
          list(method = "restyle",
               args = list("visible", list(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE)),
               label = sectors[7]),
          list(method = "restyle",
               args = list("visible", list(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE)),
               label = sectors[8]),
          list(method = "restyle",
               args = list("visible", list(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE)),
               label = sectors[9])
        )
      )
    )
  )

p
