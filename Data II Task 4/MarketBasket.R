#DISCOVER ASSOCIATIONS BETWEEN PRODUCTS

#Set library
library(arules)
library(arulesViz) 
library(ggplot2)
library(dplyr)


#Upload data and set seed + set number of displayed significant
Transactions<- read.transactions("ElectronidexTransactions2017.csv",format = "basket", 
                                 sep = ",", rm.duplicates = TRUE)
#Transactions <-as(Transactions, "transactions")
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
itemFrequencyPlot(Transactions,topN=10, type="absolute")
image(Transactions)
image(sample(Transactions))


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

itemFrequencyPlot(Transactions, topN = 10, col = rainbow(4), type="absolute")
head(Transactions@itemInfo)

# Plot frequency categories 
inspect(rules) 
ggplot(productCategory, aes(Category, fill=Category)) + geom_bar()+coord_flip()




################################################
##                 Association rules         ##
################################################


rules <- apriori(Transactions, parameter=list(minlen=2, support=0.001, confidence=0.4))
rules
inspect(head(rules, n = 10, by ="lift"))
inspect(head(rules, n = 3, by ="lift"))



#Top 10 rules

inspect(rules[1:10])
top.rules <-(rules[1:10])
inspect(head(top.rules, by = "confidence"))
inspect(head(top.rules, by = "lift"))
inspect(head(top.rules, by = "support"))


#Plot rules

plot(rules, measure = c("support", "lift"), shading = "confidence",jitter=10)
plot(rules, method = "two-key plot")
plot (top.rules, method = "graph", engine = "htmlwidget")


#Improve by inspect model
inspect(sort(Transactions, by = "support"))
inspect(sort(Transactions, by = "confidence"))
inspect(sort(Transactions, by = "lift"))


#Improve and subset model

inspect(sort(top.rules, by = "lift"))
is.redundant(top.rules)

##########################################
# #        RULES BY PRODUCTS ON LHS      ##
##########################################


# iMac Rules

iMacrules<-apriori(Transactions, parameter=list(supp=0.001,conf = 0.2), 
               appearance = list(default="rhs",lhs="iMac"))
inspect(iMacrules)
plot (iMacrules, method = "graph", engine = "htmlwidget")

# HP laptop rules

hpLaptoprules<-apriori(Transactions, parameter=list(supp=0.001,conf = 0.2), 
                   appearance = list(default="rhs",lhs="HP Laptop"))
inspect(hpLaptoprules)
plot (hpLaptoprules, method = "graph", engine = "htmlwidget")

# CYBERPOWER Gamer Desktop Rules

CPGamingLaptoprules<-apriori(Transactions, parameter=list(supp=0.001,conf = 0.2), 
                       appearance = list(default="rhs",lhs="CYBERPOWER Gamer Desktop"))
inspect(CPGamingLaptoprules)
plot (CPGamingLaptoprules, method = "scatterplot", engine = "htmlwidget")


############################################################

# Remove redundant rules
rules_nonRedundant <- rules[!is.redundant(rules)]
summary(rules_nonRedundant)
inspect(head(rules_nonRedundant,n=5,by="lift"))  


