getwd()
setwd("/Users/joshrotuna/Loyola Marymount")
data.df <- read.csv("group_project.csv")
head(data.df)

#market basket analysis
library(arules)
library(arulesViz)

head(data.df)
length(unique(data.df$Sub.Category))

data.df_min <- data.df[,c("Order.ID","Sub.Category")]
head(data.df_min)
write.csv(data.df_min,"transdata",row.names = F) #force the dataframe into csv

transdata<-read.transactions(file="transdata",format="single",sep=",",cols=c("Order.ID","Sub.Category"), rm.duplicates = T, header = T)

data.df_transaction<-as(transdata,"transactions")
data.df_transaction
summary(data.df_transaction)

# Finding frequency of Sub category
itemFrequencyPlot(arm_transactions,topN=20,type="absolute")

data.df.rules <- apriori(data.df_transaction, parameter=list(support=0.0005, conf=0.5, 
                                                      target="rules"))

summary(data.df.rules)


#Highest Lift
df_basket <- as(data.df.rules,"data.frame")
df_lift_desc<-arrange(df_basket,desc(lift)) 
head(df_lift_desc,10)

# Most confident ones
top_conf<-head(data.df.rules, n=10, by= "confidence")
top_conf<-as(top_conf,"data.frame")
top_conf

#Plot
data.df_plot<-head(sort(data.df.rules,by="lift"),30)
plot(data.df_plot,method="graph",control=list(type="items"))

#--------------------------------------------------------------------------------



