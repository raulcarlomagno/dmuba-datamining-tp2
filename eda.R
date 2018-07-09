library(data.table)
library(DataExplorer)
library(ggplot2)
library(GoodmanKruskal)
library("PerformanceAnalytics")


d <- fread("Shark_Attack_Data.csv")

dim(d)
str(d)
summary(d)

View(d)

d$href <- NULL
d$`href formula` <- NULL
d$pdf <- NULL
d$V23 <- NULL
d$V24 <- NULL

plot_missing(d)
plot_bar(d)
plot_histogram(d)


ggplot(d, aes(x=Year))+
  geom_histogram(binwidth = 8) +
  theme_minimal()

ggplot(d, aes(x=Type))+
  geom_bar() +
  theme_minimal()

table(d$Sex)
d[d$Sex != "M" & d$Sex != "F", "Sex"] <- "Unknown"
ggplot(d, aes(x=Sex))+
  geom_bar() +
  theme_minimal()

d$Fatal <- d$`Fatal (Y/N)`
d$`Fatal (Y/N)` <- NULL
table(d$Fatal)
d[d$Fatal == "F", "Fatal"] <- "Y"
d[d$Fatal == "", "Fatal"] <- "UNKNOWN"
ggplot(d, aes(x=Fatal))+
  geom_bar() +
  theme_minimal()



varSet1 <- c("Type", "Sex", "Fatal")
dFrame1 <- subset(d, select = varSet1)
GKmatrix1 <- GKtauDataframe(dFrame1)
plot(GKmatrix1)

#chart.Correlation(d$Year, histogram=TRUE, pch=19)


table(d$Type)/sum(table(d$Type))
table(d$Year)/sum(table(d$Year))
table(d$Sex)/sum(table(d$Sex))
table(d$Age)/sum(table(d$Age))
table(d$`Fatal (Y/N)`)/sum(table(d$`Fatal (Y/N)`))

length(unique(d$Country))
length(unique(d$Location))
length(unique(d$Area))
