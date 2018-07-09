library(data.table)
library(arules)
library(arulesViz)

data <- fread("data.csv", stringsAsFactors = TRUE)
str(data)
data$Year <- as.factor(data$Year)
trans <- as(data, "transactions")
trans
summary(trans)
dim(data)
dim(trans)
itemLabels(trans)
itemFrequencyPlot(trans, topN=25)


trans_female <- subset(trans, items %in% "Sex=F" & !items %in% "Country=USA")
itemFrequencyPlot(trans_female, topN = 25, population = trans, cex.names=.75)
itemFrequencyPlot(trans_female, topN = 25, population = trans, lift=TRUE, cex.names=.75)


#Mine Frequent Itemsets
#Find an interesting support (have at least 100 observations)
100/nrow(trans)
itemsets <- apriori(trans, parameter = list(target = "frequent", supp=0.01732, minlen = 2, maxlen = 4))
inspect(head(sort(itemsets), n=20))

quality(itemsets)$lift <- interestMeasure(itemsets, measure="lift", trans = trans)
inspect(head(sort(itemsets, by = "lift"), n=10))

#Plot itemsets as a graph. Different subgroups with items that are related to each other can be identified.
plot(head(sort(itemsets, by = "lift"), n=25), method = "graph", control=list(cex=.8))


#Mine Association Rules
r <- apriori(trans, parameter = list(supp=0.02, maxlen=4))
inspect(head(sort(r, by="lift"), n=10))

plot(r)

r_female <- subset(r, subset = items %in% "Sex=F")
r_female
#dev.off()
inspect(head(sort(r_female, by="lift"), 10))
itemFrequencyPlot(items(r_female), topN=30, cex.names=.6)

plot(head(sort(r_female, by="lift"), 50), method="graph", control=list(cex=.7))
