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

# Removing transaction size 0

Transactions <-Transactions[which(size(Transactions)!=0)]

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
##          Association rules                  ##
################################################


rules <- apriori(Transactions, parameter=list(minlen=2, support=0.001, confidence=0.4))
rules
inspect(head(rules, n = 10, by ="lift"))
inspect(head(rules, n = 3, by ="lift"))
is.redundant(rules)
rules <- rules[!is.redundant(rules)]
nrow(Transactions)

#Rules without laptops,desktops and computers

`%!in%` = Negate(`%in%`)
NoLaptopRules <- subset(rules,items %!in% c("iMac","HP Laptop", "Apple MacBook Air", "CYBERPOWER Gamer Desktop", 
                                            "Lenovo Desktop Computer","Dell Desktop","ViewSonic Monitor","Eluktronics Pro Gaming Laptop"))
inspect(NoLaptopRules[1:10])


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


# Rules without laptops

rulesNoLaptops <- apriori(Transactions, parameter=list(supp=0.001,conf = 0.2),
                          appearance = items %in% "CYBERPOWER Gamer Desktop")

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



###########################################################
##                Split Data by B2B AND B2C            ####
###########################################################

transactions_B2C <- Transactions[which(size(Transactions)<=4)] #B3B Transactions
transactions_B2B <- Transactions[which(size(Transactions)>4)]  #B2C Transactions
transactions_oneItem <- Transactions[which(size(Transactions)==1)] #Transactions with only one item

# Piechart proportion of customers that are individuals or businesses

transactionSize$customerType <- ifelse(transactionSize$size.Transactions.>4, transactionSize$customerType <- "Business", transactionSize$customerType <- "Private" )

ggplot(transactionSize,aes(x="",fill=customerType))+
      geom_bar()+
      coord_polar(theta = "y")

##########################################################
###             Rules by Category                       ##
##########################################################


##########################################################
###             Rules by Type of Customer               ##
##########################################################

### Rules B2B ###

rule_B2B <- apriori(transactions_B2B, parameter=list(supp=0.02,conf = 0.2) )
inspect(rule_B2B)

# Rules no Laptops, desktops and computers

rule_B2BnoLaptops <- subset(rule_B2B,items %!in% c("iMac","HP Laptop", "Apple MacBook Air", "CYBERPOWER Gamer Desktop", 
                                                   "Lenovo Desktop Computer","Dell Desktop","ViewSonic Monitor","Eluktronics Pro Gaming Laptop","Samsung Monitor",
                                                   "Acer Desktop","Apple MacBook Pro","HP Monitor"))
inspect(rule_B2BnoLaptops)

#Rule Apple Earpods B2B

rule_B2BappleEarpods <- apriori(transactions_B2B,parameter=list(supp=0.002,conf = 0.2), 
                                appearance = list(default="rhs",lhs="Apple Earpods"))
inspect(rule_B2BappleEarpods)

# Rule B2B iMac

rule_B2BiMac <- apriori(transactions_B2B,parameter=list(supp=0.002,conf = 0.2), 
            appearance = list(default="rhs",lhs="iMac"))
inspect(rule_B2BiMac)

### Rules B2C ###

rule_B2C <- apriori(transactions_B2C, parameter=list(supp=0.005,conf = 0.1) )
inspect(rule_B2C)

# Rules no Laptops, desktops and computers

rule_B2CnoLaptops <- subset(rule_B2C,items %!in% c("iMac","HP Laptop", "Apple MacBook Air", "CYBERPOWER Gamer Desktop", 
                                                   "Lenovo Desktop Computer","Dell Desktop","ViewSonic Monitor","Eluktronics Pro Gaming Laptop","Samsung Monitor",
                                                   "Acer Desktop","Apple MacBook Pro","HP Monitor"))
inspect(rule_B2CnoLaptops)

#Rule Apple Earpods B2B

rule_B2CappleEarpods <- apriori(transactions_B2C,parameter=list(supp=0.001,conf = 0.1), 
                                appearance = list(default="rhs",lhs="Apple Earpods"))
inspect(rule_B2CappleEarpods)

# Rule iMac B2C

rule_B2CiMac <- apriori(transactions_B2C,parameter=list(supp=0.001,conf = 0.1), 
                        appearance = list(default="rhs",lhs="iMac"))
inspect(rule_B2CiMac)


