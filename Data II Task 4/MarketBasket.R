#DISCOVER ASSOCIATIONS BETWEEN PRODUCTS

# lLoad libraries
if(require("pacman")=="FALSE"){
  install.packages("pacman")
}
pacman::p_load("readr","ggplot2","arules","arulesViz","plotly","RColorBrewer","dplyr")

# Upload data and set seed + set number of displayed significant
Transactions<- read.transactions("ElectronidexTransactions2017.csv",format = "basket", 
                                 sep = ",", rm.duplicates = TRUE)
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

##########################################################
##   Compare Blackwell and Electronidex portofolio      ##
##########################################################


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

#########################################################################################

# Adding the labels
Transactions@itemInfo$category <- productCategory$Category


# TOP 10 most frequent products by product

itemFrequencyPlot(Transactions, topN = 10, col = rainbow(4), type="absolute")
head(Transactions@itemInfo)

# Plot nr items in a transaction

transactionSize <- data.frame(size(Transactions))

ggplot(transactionSize,aes(size.Transactions.))+
      geom_bar(fill="red")+
      labs(title = "Size of transactions")

################################################
##                 Association rules         ##
################################################


rules <- apriori(Transactions, parameter=list(minlen=2, support=0.001, confidence=0.4))
inspect(head(rules, n = 3, by ="lift"))
is.redundant(rules)
rules <- rules[!is.redundant(rules)]

#Top 10 rules

inspect(rules[1:10])
top.rules <-rules[1:10]


#Plot rules

plot(rules, measure = c("support", "lift"), shading = "confidence",jitter=10)
plot(rules, measure = c("support", "lift"), shading = "confidence")
plot(rules, method = "two-key plot")
plot (top.rules, method = "graph", engine = "htmlwidget")


#Improve and subset model

inspect(sort(top.rules, by = "lift"))
is.redundant(top.rules) #no redundant rules

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



#########################################################
##          Split data by categories                   ##
#########################################################

Data_catsplit <- aggregate(Transactions, by = 'category')
itemLabels(Data_catsplit)
library(RColorBrewer)

# Plotting frequency by category

itemFrequencyPlot(Data_catsplit, topN = 10, type = "relative", 
                             col = colorRampPalette(brewer.pal(9, "Paired"))(10), 
                             main = "Categories Relative Item Frequency Plot")

##########################################################
###             Rules by Category                       ##
##########################################################

