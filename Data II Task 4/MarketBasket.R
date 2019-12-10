#DISCOVER ASSOCIATIONS BETWEEN PRODUCTS

#Set library
library(arules)
library(arulesViz) 


#Upload data and set seed + set number of displayed significant
Transactions<- read.transactions("ElectronidexTransactions2017.csv")
set.seed(123)
options(digits = 2)
productCategory <- read.csv("ProductCategories.csv")
productCategory<- as.data.frame(productCategory)

#Inspect dataset
inspect(Transactions)
length (Transactions) # Number of transactions.
size(Transactions) # Number of items per transaction
LIST(Transactions) # Lists the transactions by conversion
itemLabels(Transactions)# To see the item labels
summary(Transactions)
head(Transactions@itemInfo)

#Visualize dataset
itemFrequencyPlot(Transactions, support=relative)
image(Transactions)
image(sample(Transactions)) #add more things to this badboy

# Adding the labels
Transactions@itemInfo <- productCategory


# TOP 10 most frequent products
itemFrequencyPlot(Transactions, topN = 10, col = rainbow(4), type="absolute")

#Association rules
rules <- apriori(Transactions, parameter=list(support=0.001, confidence=0.5))
rules


inspect(rules) #takes forever-created + 400 000 rules
inspect(head(rules, n = 3, by ="lift"))

#Plot rules

head(quality(rules))


plot(rules, measure = c("support", "lift"), shading = "confidence")
plot(rules, method = "two-key plot")


     