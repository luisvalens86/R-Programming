install.packages(c("arules", "arulesViz")) # APPEARS ON CONSOLE
library('arules')
library('arulesViz')
data(Groceries)
summary(Groceries) # INDICATES 9835 ROWS

Groceries@itemInfo[1:10,]

#apply() takes Data frame or matrix as an input and gives output in vector, list or array. 
apply(Groceries@data[,10:20],2,function(r) paste(Groceries@itemInfo[r,"labels"],collapse=", "))

itemsets<-apriori(Groceries,parameter=list(minlen=1,maxlen=1,support=0.02,target="frequent itemsets"))
summary(itemsets)
inspect(head(sort(itemsets,by="support"),10)) # LISTS TOP 10

#SECOND, GET ITEMSETS OF LENGTH 2
itemsets<-apriori(Groceries,parameter=list(minlen=2,maxlen=2,support=0.02,target="frequent itemsets"))
summary(itemsets) # FOUND 61 ITEMSETS
inspect(head(sort(itemsets,by="support"),10)) # LISTS TOP 10

#THIRD, GET ITEMSETS OF LENGTH 3
itemsets<-apriori(Groceries,parameter=list(minlen=3,maxlen=3,support=0.02,target="frequent itemsets"))
summary(itemsets) # FOUND 2 ITEMSETS
inspect(head(sort(itemsets,by="support"),10)) # LISTS TOP 10

rules <- apriori(Groceries,parameter=list(support=0.001,confidence=0.6,target="rules"))
summary(rules) # FINDS 2918 RULES
plot(rules) # DISPLAYS SCATTERPLOT
plot(rules@quality) # DISPLAYS SCATTERPLOT MATRIX

slope <-sort(round(rules@quality$lift/rules@quality$confidence,2))
unlist(lapply(split(slope,f=slope),length))
inspect(head(sort(rules,by="lift"),10))

confidentrules<-rules[quality(rules)$confidence>0.9]
confidentrules # SET OF 127 RULES
plot(confidentrules, method = "matrix", measure = c("lift", "confidence"))

highliftrules<-head(sort(rules,by="lift"),5)
plot(highliftrules,method="graph")


#1. According to the summary, how many transactions have yogurt? 
#answer: yogurt 1327
summary(Groceries) # INDICATES 9835 ROWS




