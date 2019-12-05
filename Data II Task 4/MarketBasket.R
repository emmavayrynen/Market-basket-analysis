#DISCOVER ASSOCIATIONS BETWEEN PRODUCTS

#Set library
library(arules)
library(arulesViz) 


#Upload data and set seed + set number of displayed significant
Transactions<- read.transactions("ElectronidexTransactions2017.csv")
set.seed(123)
options(digits = 2)

#Inspect dataset
inspect(Transactions)
length (Transactions) # Number of transactions.
size(Transactions) # Number of items per transaction
LIST(Transactions) # Lists the transactions by conversion
itemLabels(Transactions)# To see the item labels
summary(Transactions)

#Visualize dataset
itemFrequencyPlot(Transactions)
image(Transactions)
image(sample(Transactions))



