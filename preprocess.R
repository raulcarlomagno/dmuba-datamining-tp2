options("encoding" = "UTF-8")
library(data.table)
library(arules)
library(stringr)

d <- fread("Shark_Attack_Data.csv")

d$href <- NULL
d$`href formula` <- NULL
d$pdf <- NULL
d$V23 <- NULL
d$V24 <- NULL
d$`Investigator or Source` <- NULL
#d$Species <- NULL
d$Name <- NULL
d$Injury <- NULL
d$Location <- NULL
d$Area <- NULL
d$Activity <- NULL

d[d$Sex != "M" & d$Sex != "F", "Sex"] <- NA
ggplot(d) + aes(x = Sex) + geom_bar()

d$Fatal <- d$`Fatal (Y/N)`
d$`Fatal (Y/N)` <- NULL
d[d$Fatal == "F", "Fatal"] <- "Y"
d[d$Fatal == "" | d$Fatal == "UNKNOWN", "Fatal"] <- NA
ggplot(d) + aes(x = Fatal) + geom_bar()


#View(d)


d$year2 <- as.integer(substr(d$`Case Number`, 1, 4))
d <- d[d$year2 >= 1845,]

#summary(d$year2)
ggplot(d) + aes(x = year2) + geom_histogram()

d[d$Year != d$year2, c("original order", "Case Number", "Year", "year2")]
d$Year <- d$year2
d$year2 <- NULL
ggplot(d) + aes(x = Year) + geom_histogram()


d$month <- as.integer(substr(d$`Case Number`, 6, 7))
ggplot(d) + aes(x = month) + geom_bar()

getMonth <- function(monthIdx){
  if(monthIdx >= 1 & monthIdx <= 12)
    return(month.abb[monthIdx])
  else
    return(NA)
}

d$month2 <- sapply(d$month, getMonth)
d[d$month == 0, "month2"]
ggplot(d) + aes(x = month2) + geom_bar()
d$month <- d$month2
d$month2 <- NULL


d$`Case Number` <- NULL
d$`Case Number` <- NULL
d$`Case Number` <- NULL
d$Date <- NULL
d$`original order` <- NULL

#unique(d$Age)
d$age2 <- as.numeric(d$Age)

#sum(is.na(d$age2)) / length(d$age2) #40% missing... mas del 5%, no la puedo salvar
ggplot(d) + aes(x = age2) + geom_histogram()
#summary(d$age2)
#boxplot(d$age2)

d$age3 <- discretize(d$age2, method = "fixed", breaks = c(-Inf, 12, 18, 25, 40, 65, Inf))
ggplot(d) + aes(x = age3) + geom_bar()
levels(d$age3) <- c("niño", "adolescente", "joven", "adulto", "mediana edad", "tercera edad")
ggplot(d) + aes(x = age3) + geom_bar()
ggplot(d) + aes(x = Age) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
d$Age <- d$age3
d$age3 <- NULL
d$age2 <- NULL


######################################################trabajar el time si queda tiempo
#unique(d$Time)
d$Time <- NULL

d$Species <- str_to_lower(d$Species)
d$shark <- str_replace_all(d$Species, "shark", "")
d$shark <- str_replace_all(d$shark, "sharks", "")
d$shark <- str_trim(d$shark, "both")
View(d[Species != ""])

d$sizepulgada <- str_replace(str_extract(d$shark, "(?:[0-9]+\\.[0-9]+|[0-9]+)'"), "'", "")
d[like(shark, "foot") | like(shark, "feet") & is.na(sizepulgada), sizepulgada := str_extract(shark, "(?:[0-9]+\\.[0-9]+|[0-9]+)")]
d$shark <- str_replace_all(d$shark, "(?:[0-9]+\\.[0-9]+|[0-9]+)'", "")
d$shark <- str_replace_all(d$shark, "feet", "")
d$shark <- str_replace_all(d$shark, "foot", "")

d$sizemetro <- str_extract(d$shark, "(?:[0-9]+\\.[0-9]+|[0-9]+)\\s?m")
d$sizemetro <- str_trim(str_replace(d$sizemetro, "m", ""), "both")
d$shark <- str_replace_all(d$shark, "(?:[0-9]+\\.[0-9]+|[0-9]+)\\s?m", "")

d$shark <- str_replace_all(d$shark, ",", "")
d$shark <- str_replace_all(d$shark, "to", "")
d$shark <- str_replace_all(d$shark, "\\[", "")
d$shark <- str_replace_all(d$shark, "]", "")
d$shark <- gsub('[[:digit:]]+', '', d$shark)
d$shark <- str_replace_all(d$shark, "\\.", "")
d$shark <- str_trim(d$shark, "both")
View(d)

#head(sort(table(d[shark != "" & shark != "involvement not confirmed" & shark != "\"\"" & shark != "\"\"a small \"\"" & shark != "-lb" & shark != "s" & shark != "no  involvement"]$shark), decreasing=T),20)

species <- c("white", "tiger", "bull", "blacktip", "bronze_whaler", "blacktip", "wobbegong",
             "nurse", "raggedoth", "nurse", "hammerhead", "mako", "blue", "lemon", "zambesi",
             "spinner", "reef", "caribbean", "sandtiger"
)

#words <- unlist(strsplit(d$shark,' '))
#words <- words[!words %in% c("", "a", "\"\"", "confirmed", "not", "or", "s", "-lb", "by", "possibly", "involvement", "involve", "small", "in", "-kg", "the", "thought", "oth", "identified", "&", "?", "was", "said", "on", "\"\"a", "of", "recovered")]
#head(sort(table(words), decreasing=T),20)

fixBronze <- str_replace_all(d$shark, "bronze whaler", "bronze_whaler")  

d$specie <- species[match(strsplit(fixBronze,' '),species)]
ggplot(d[!is.na(specie)]) + aes(x = specie) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
sum(is.na(d$specie))/nrow(d)
#View(d)
#table(d$specie)

d$Species <- NULL
d$shark <- NULL

#dback <- d
#d <- dback

d$sizemetro <- as.numeric(d$sizemetro)
d$size <- d$sizemetro
d[is.na(d$sizemetro) & !is.na(d$sizepulgada)]$size <- as.numeric(d[is.na(d$sizemetro) & !is.na(d$sizepulgada)]$sizepulgada) * 0.3048 
View(d)

d$sizemetro <- NULL
d$sizepulgada <- NULL

d$size2 <- discretize(d$size, method = "fixed", breaks = c(-Inf, 1.8, 3, Inf))
ggplot(d) + aes(x = size2) + geom_bar()
#levels(d$size2) <- c("< 1.8m", "1.8m a 3m", "> 3m")
levels(d$size2) <- c("chico", "mediano", "grande")
ggplot(d) + aes(x = size2) + geom_bar()
d$size <- d$size2
d$size2 <- NULL

head(sort(table(d$Country), decreasing=T),50)
head(sort(table(d$Country), decreasing=F),50)
d[Country == "", Country := NA]
ggplot(d) + aes(x = Country) + geom_bar()

fwrite(d, "data.csv")

