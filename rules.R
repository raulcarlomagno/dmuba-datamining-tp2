library(data.table)
library(arules)
library(arulesViz)

data <- fread("data.csv", stringsAsFactors = TRUE, na.strings = "")
#ggplot(data) + aes(x = Sex) + geom_bar()
str(data)
data$Year <- as.factor(data$Year)
trans <- as(data, "transactions")
trans
summary(trans)
dim(data)
dim(trans)
itemLabels(trans)
itemFrequencyPlot(trans, topN=25, cex.names=.75)

#Error in plot.new() : figure margins too large
#par("mar")
#par(mar=c(1,1,1,1))



reglas <- apriori(trans, parameter = list(support=0.1, confidence = 0.75))
inspect(head(sort(reglas, by="lift", decreasing = TRUE),50))


reglas <- apriori(trans, parameter = list(support=0.002, maxlen=4, minlen = 2))
inspect(head(sort(reglas, by="lift", decreasing = TRUE),200))
print(reglas)
plot(reglas)
dev.off()


reglasfatales <- apriori(trans, parameter = list(support=0.005, confidence=0.01, minlen = 2), appearance = list(rhs="Age=niño"))
inspect(head(sort(reglasfatales, by="lift", decreasing = TRUE),100))
plot(head(sort(reglasfatales, by = "lift"), n=15), method = "graph", control=list(cex=.75))


reglastercedad <- apriori(trans, parameter = list(support=0.008, confidence=0.008, minlen = 2), appearance = list(rhs="Type=Provoked")) 
inspect(head(sort(reglastercedad, by="lift", decreasing = TRUE),100))
plot(head(sort(reglastercedad, by = "lift"), n=15), method = "graph", control=list(cex=.75))


reglastercedad <- apriori(trans, parameter = list(support=0.009, maxlen=4, minlen = 1))
subreglastercedad <- subset(reglastercedad, subset = !items %in% "Country=USA" & !items %in% "Country=AUSTRALIA" & !items %in% "Country=SOUTH AFRICA")
inspect(head(sort(subreglastercedad, by="lift", decreasing = TRUE),100))
plot(head(sort(subreglastercedad, by = "lift"), n=15), method = "graph", control=list(cex=.75))
plot(subreglastercedad)




trans_female <- subset(trans, items %in% "Sex=F" & !items %in% "Country=USA" & !items %in% "Age=unknown")
itemFrequencyPlot(trans_female, topN = 25, population = trans, cex.names=.75)
itemFrequencyPlot(trans_female, topN = 25, population = trans, lift=TRUE, cex.names=.75)

transConEdad <- subset(trans, !items %in% "Age=unknown")

#Mine Frequent Itemsets
#Find an interesting support (have at least 100 observations)
100/nrow(transConEdad)
itemsets <- apriori(transConEdad, parameter = list(target = "frequent", supp=0.03, minlen = 2, maxlen = 4))
itemFrequencyPlot(transConEdad, topN = 25, population = transConEdad, cex.names=.75)
inspect(head(sort(itemsets), n=20))

quality(itemsets)$lift <- interestMeasure(itemsets, measure="lift", trans = transConEdad)
inspect(head(sort(itemsets, by = "lift"), n=10))

#Plot itemsets as a graph. Different subgroups with items that are related to each other can be identified.
plot(head(sort(itemsets, by = "lift"), n=25), method = "graph", control=list(cex=.8))


r <- apriori(transConEdad, parameter = list(supp=0.25, maxlen=4))
inspect(head(sort(r, by="lift"), n=30))
plot(r)


#Mine Association Rules
r <- apriori(trans, parameter = list(supp=0.03, maxlen=4))
inspect(head(sort(r, by="lift"), n=10))

plot(r)

r_female <- subset(r, subset = items %in% "Sex=F")
r_female
#dev.off()
inspect(head(sort(r_female, by="lift"), 10))
itemFrequencyPlot(items(r_female), topN=30, cex.names=.6)

plot(head(sort(r_female, by="lift"), 50), method="graph", control=list(cex=.7))






#accidentes fatales:
fatales <- subset(trans, items %in% "Fatal=Y" & !items %in% "Age=unknown")
colsDelete <- which(colnames(fatales)=="Fatal=Y")
fatales <- fatales[,-colsDelete]

itemFrequencyPlot(fatales, topN = 25, population = trans, cex.names=.75)
itemFrequencyPlot(fatales, topN = 25, population = trans, lift=TRUE, cex.names=.75)

rfatales <- apriori(fatales, parameter = list(supp=0.20, maxlen=4))
rfatales
inspect(head(sort(rfatales, by="lift"), n=10))
inspect(head(sort(rfatales, by="lift"), n=20))
plot(head(sort(rfatales, by="lift"), 10), method="graph", control=list(cex=.7))
plot(rfatales)



#mujeres:
mujeres <- subset(trans, items %in% "Sex=F" & !items %in% "Age=unknown")
colsDelete <- which(colnames(fatales)=="Sex=F")
mujeres <- mujeres[,-colsDelete]

itemFrequencyPlot(mujeres, topN = 25, population = trans, cex.names=.75)
itemFrequencyPlot(mujeres, topN = 25, population = trans, lift=TRUE, cex.names=.75)

rmujeres <- apriori(mujeres, parameter = list(supp=0.20, maxlen=4))
rmujeres
inspect(head(sort(rmujeres, by="lift"), n=10))
plot(head(sort(rmujeres, by="lift"), 10), method="graph", control=list(cex=.7))
plot(rmujeres)



#nofatales:
nofatales <- subset(trans, items %in% "Fatal=N" & !items %in% "Age=unknown" & !items %in% "Country=USA" )
colsDelete <- which(colnames(nofatales)=="Fatal=N")
nofatales <- nofatales[,-colsDelete]

itemFrequencyPlot(nofatales, topN = 25, population = trans, cex.names=.75)
itemFrequencyPlot(nofatales, topN = 25, population = trans, lift=TRUE, cex.names=.75)

rnofatales <- apriori(nofatales, parameter = list(supp=0.20, maxlen=4))
rnofatales
inspect(head(sort(rnofatales, by="lift"), n=10))
plot(head(sort(rnofatales, by="lift"), 10), method="graph", control=list(cex=.7))
plot(rnofatales)


#sacar un provoked

#3ra edad
tercedad <- subset(trans, items %in% "Age=tercera edad")
colsDelete <- which(colnames(tercedad)=="Age=tercera edad")
tercedad <- tercedad[,-colsDelete]
rtercedad <- apriori(tercedad, parameter = list(supp=0.20, maxlen=4))
inspect(head(sort(rtercedad, by="lift"), n=10))
plot(head(sort(rtercedad, by="lift"), 10), method="graph", control=list(cex=.7))
