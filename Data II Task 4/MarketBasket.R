#DISCOVER ASSOCIATIONS BETWEEN PRODUCTS

#Set library
library(arules)
library(arulesViz) 
library(ggplot2)
library(dplyr)

#Upload data and set seed + set number of displayed significant
Transactions<- read.transactions("ElectronidexTransactions2017.csv",format = "basket", 
                                 sep = ",", rm.duplicates = TRUE)
Transactions

set.seed(123)
options(digits = 2)

# Upload the product category data
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
colnames(blackwellProductTypes)[1] <- "Category"
colnames(blackwellProductTypes)[2] <- "Product"


# Prepare Electronidex product data 

productCategory$Company <- "Electronidex"

# Merge dataframes

ProductTypes <- rbind(productCategory, blackwellProductTypes)

# Visually compare Blackwell and Electronidex product offering 

ggplot(tally(group_by(ProductTypes, Category, Company)),aes(Category, n, fill = Category)) + 
  geom_col() + facet_grid(Company ~ .) + theme(axis.text.x = element_text(angle = 90, hjust = 1, size =15))+
  guides(fill=FALSE) + xlab("Products") + ylab("Count") + 
  ggtitle("Visual Representation of Product Offering between BW and EN")

# Adding the labels
Transactions@itemInfo$category <- productCategory$Category


# TOP 10 most frequent products by product

itemFrequencyPlot(Transactions, topN = 20, col = rainbow(4), type="absolute")
head(Transactions@itemInfo)

# Plot frequency categories 

ggplot(productCategory, aes(Category, fill=Category)) + geom_bar()+coord_flip()

# Association rules
rules <- apriori(Transactions, parameter=list(support=0.001, confidence=0.4))
rules

inspect(rules)
inspect(head(rules, n = 3, by ="lift"))

# Plot rules

plot(rules, measure = c("support", "lift"), shading = "confidence")
plot(rules, method = "two-key plot")

# Remove redundant rules
rules_nonRedundant <- rules[!is.redundant(rules)]
summary(rules_nonRedundant)
inspect(head(rules_nonRedundant,n=5,by="lift"))  


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

