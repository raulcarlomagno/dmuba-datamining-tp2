library(data.table)

d <- fread("Shark_Attack_Data.csv", stringsAsFactors = TRUE)

summary(d)
dim(d)

View(d)

table(d$Type)/sum(table(d$Type))
table(d$Year)/sum(table(d$Year))
table(d$Sex)/sum(table(d$Sex))
table(d$Age)/sum(table(d$Age))
table(d$`Fatal (Y/N)`)/sum(table(d$`Fatal (Y/N)`))

length(unique(d$Country))
length(unique(d$Location))
length(unique(d$Area))
