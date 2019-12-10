#DISCOVER ASSOCIATIONS BETWEEN PRODUCTS

#Set library
library(arules)
library(arulesViz) 
library(plyr)
library(ggplot2)

#Upload data and set seed + set number of displayed significant
Transactions<- read.transactions("ElectronidexTransactions2017.csv",format = "basket", 
                                 sep = ",", rm.duplicates = TRUE)
Transactions <-as(Transactions, "transactions")
Transactions
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


#Visualize dataset
itemFrequencyPlot(Transactions,topN=10, type="absolute")
image(Transactions)
image(sample(Transactions)) #add more things to this badboy
ncol(Transactions)

### Compare Blackwell and Electronidex product offering

# Prepare Blackwell product data
blackwellNew <- read.csv("blackwellNew.csv")
blackwellNew <- blackwellNew[c(2,3)]
blackwellOld <- read.csv("blackwellOld.csv")
blackwellOld <- blackwellOld[c(1,2)]

blackwellProductTypes <- rbind(blackwellOld, blackwellNew)
blackwellProductTypes <- data.frame(blackwellProductTypes$ProductType, blackwellProductTypes$ProductNum)
View(blackwellProductTypes)

blackwellProductTypes$Company <- "Blackwell"
colnames(blackwellProductTypes)[1] <- "ProductType"
colnames(blackwellProductTypes)[2] <- "Product"


# Adding the labels
Transactions@itemInfo$category <- productCategory$Category


# TOP 10 most frequent products
itemFrequencyPlot(Transactions, topN = 10, col = rainbow(4), type="absolute")
head(Transactions@itemInfo)

#Association rules
rules <- apriori(Transactions, parameter=list(support=0.001, confidence=0.8))
rules

inspect(rules) 
inspect(head(rules, n = 3, by ="lift"))
head(quality(rules))


#Top 10 rules
inspect(rules[1:10])
top.rules <-inspect(rules[1:10])


#Plot rules
plot(rules, measure = c("support", "lift"), shading = "confidence",jitter=10)
plot(rules, method = "two-key plot")
plot (top.rules, method ="grouped")


#Improve and subset model
inspect(sort(Transactions, by = "support"))
