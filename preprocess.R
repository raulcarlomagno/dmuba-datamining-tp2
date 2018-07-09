options("encoding" = "UTF-8")
library(data.table)
library(arules)

d <- fread("Shark_Attack_Data.csv")

d$href <- NULL
d$`href formula` <- NULL
d$pdf <- NULL
d$V23 <- NULL
d$V24 <- NULL
d$`Investigator or Source` <- NULL
d$Species <- NULL
d$Name <- NULL
d$Injury <- NULL
d$Location <- NULL
d$Area <- NULL
d$Activity <- NULL

d[d$Sex != "M" & d$Sex != "F", "Sex"] <- "Unknown"

d$Fatal <- d$`Fatal (Y/N)`
d$`Fatal (Y/N)` <- NULL
d[d$Fatal == "F", "Fatal"] <- "Y"
d[d$Fatal == "", "Fatal"] <- "UNKNOWN"

View(d)

d <- d[d$Year > 500,]
d$year2 <- as.integer(substr(d$`Case Number`, 1, 4))

summary(d$year2)
ggplot(d) +
  aes(x = year2) +
  geom_histogram()

d[d$Year != d$year2, c("original order", "Case Number", "Year", "year2")]
d$Year <- d$year2
d$year2 <- NULL



d$month <- as.integer(substr(d$`Case Number`, 6, 7))
ggplot(d) +
  aes(x = month) +
  geom_bar()


getMonth <- function(monthIdx){
  if(monthIdx >= 1 & monthIdx <= 12)
    return(month.abb[monthIdx])
  else
    return("Unknown")
}

d$month2 <- sapply(d$month, getMonth)
d[d$month == 0, "month2"]
ggplot(d) +
  aes(x = month2) +
  geom_bar()
d$month <- d$month2
d$month2 <- NULL


d$`Case Number` <- NULL
d$`Case Number` <- NULL
d$`Case Number` <- NULL
d$Date <- NULL
d$`original order` <- NULL

unique(d$Age)
d$age2 <- as.numeric(d$Age)

sum(is.na(d$age2)) / length(d$age2) #40% missing... mas del 5%, no la puedo salvar
ggplot(d) +
  aes(x = age2) +
  geom_histogram()
summary(d$age2)

d$age3 <- discretize(d$age2, method = "cluster", 6)
ggplot(d) +
  aes(x = age3) +
  geom_bar()
levels(d$age3) <- c("niño", "adolescente", "joven", "adulto", "mediana edad", "tercera edad", "unknown")
d[is.na(d$age3), "age3"] <- "unknown"

d$Age <- d$age3
d$age3 <- NULL
d$age2 <- NULL


######################################################trabajar el time si queda tiempo
#unique(d$Time)
d$Time <- NULL


fwrite(d, "data.csv")
