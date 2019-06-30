getwd()
setwd("E:/SUVEN DATA ANALYSIS/Project with R")
install.packages(arules)
library(arules)
Groc <- read.transactions(file = "groceries.csv",format = "basket",sep = ",")
head(Groc)
nrow(Groc)
ncol(Groc)
Groc
colnames(Groc)
#colSums(is.na(Groc))
Groc[1:5]
summary(Groc)




#inspect the first 5 transactions
inspect(Groc[1:5])
itemFrequency(Groc[, 1:5]) #relative
itemFrequency(Groc[, 1:5],type="absolute")#absolute


#Rules
Grocrules <- apriori(Groc, parameter = list(support =0.005, confidence = 0.2,maxlen=2, minlen = 2))
Grocrules
summary(Grocrules)
inspect(Grocrules[1:3])
inspect(sort(Grocrules, by = "lift")[1:5])
inspect(sort(Grocrules, by = "confidence")[1:5])
#inspect the rules for particular item
herbsrules <- subset(Grocrules, items %in% "herbs")
inspect(herbsrules)

install.packages(dplyr)
library(dplyr)

#find the top 20 "sold items" that occur in the dataset

result<-data.frame("items_name"=itemLabels(Groc),"item_count"=itemFrequency(Groc,type='absolute'),"item_perc"=itemFrequency(Groc,type='relative'))
(df<-result%>%arrange(desc(item_count))%>%top_n(n=20))
itemFrequencyPlot(Groc, topN = 20,col=rainbow(20),main="Top 20",xlab="Items")

x <-df$item_count
labels<- df$items_name
(piepercent=(x/sum(x))*100)
library(plotrix)
pie3D(x,labels= labels,col =
        rainbow(length(result$items_name)),labelcex=1,explode=0.1)

#total sales account for

(total<-sum(result$item_count))
vol1<-data.frame("items_name"=itemLabels(Groc),"item_count"=itemFrequency(Groc,type='absolute'),"item_perc"=itemFrequency(Groc,type='absolute')/total)
View(vol1%>%arrange(desc(item_count))%>%top_n(n=20))




total_sales_perc<-sum((vol1%>%arrange(desc(item_count))%>%top_n(n=20))$item_perc*100)
paste("The total sales account for ",total_sales_perc)
x <-c(total_sales_perc,100-total_sales_perc)
labels<- c('Top 20 Items',"Remaining Items")

library(plotrix)
pie3D(x,labels= labels,col =
        c('red',"yellow"),labelcex=1,explode=0.1)


write(Grocrules, file = "Grocrules.csv",sep = ",", quote = TRUE, row.names = FALSE)

groceryrules_df <- as(Grocrules, "data.frame")
str(groceryrules_df)
head(groceryrules_df)

#Conclusion:
#whole milk is bought what are the other products that customer will buy
rules<-apriori(data=Groc, parameter=list(supp=0.005,conf = 0.15,minlen=2), appearance = list(default="rhs",lhs="whole milk"),
               control = list(verbose=F))
rules<-sort(rules, decreasing=TRUE,by="confidence")
inspect(rules[1:5])

rules<-apriori(data=Groc, parameter=list(supp=0.005,conf = 0.15),appearance = list(default="lhs",rhs="whole milk"),
               control = list(verbose=F))
rules<-sort(rules, decreasing=TRUE,by="confidence")
inspect(rules[1:5])

