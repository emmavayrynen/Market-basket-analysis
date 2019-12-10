#DISCOVER ASSOCIATIONS BETWEEN PRODUCTS

#Set library
library(arules)
library(arulesViz) 


#Upload data and set seed + set number of displayed significant
Transactions<- read.transactions("ElectronidexTransactions2017.csv",format = "basket", 
                                 sep = ",", rm.duplicates = TRUE)
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
itemFrequencyPlot(Transactions, support=relative)
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


#Association rules
rules <- apriori(Transactions, parameter=list(minlen=2)(support=0.001, confidence=0.5))
rules
inspect(rules) #takes forever-created + 400 000 rules
inspect(head(rules, n = 3, by ="lift"))
head(quality(rules))


#Plot rules
plot(rules, measure = c("support", "lift"), shading = "confidence",jitter=10)
plot(rules, method = "two-key plot")

#Sorting association
top.support <- sort(rules, decreasing = TRUE, na.last = NA, by = "support")
top.ten.support <- sort.list(top.support, partial=10)
inspect(top.ten.support)

top.confidence <- sort(rules, decreasing = TRUE, na.last = NA, by = "confidence")
top.ten.confidence <- sort.list(top.support,partial=10)
inspect(top.ten.confidence)

#?rules2 <- apriori(Transactions, parameter=list(supp = 0.5, conf = 0.8), appearance = income)

#top.lift <- sort(rules2, decreasing = TRUE, na.last = NA, by = "lift")
#top.ten.lift <- sort.list(top.lift, partial=10)
#inspect(top.ten.lift)

#Improve and subset model
inspect(sort(Transactions, by = "Type of Measurement"))
