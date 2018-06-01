### Importing necessary libraries and Packages ####
library(dplyr)
library(ggplot2)
library(cowplot)
library(woe)

creditBureauData <- read.csv("Credit Bureau data.csv", stringsAsFactors = F)
demoData <- read.csv("Demographic data.csv", stringsAsFactors = F)

#### Data Definition ####
### Demographic Data ###
# Application ID:	Unique ID of the customers
# Age:	Age of customer
# Gender:	Gender of customer
# Marital Status:	Marital status of customer (at the time of application)
# No of dependents:	No. of childrens of customers
# Income:	Income of customers
# Education:	Education of customers
# Profession:	Profession of customers
# Type of residence:	Type of residence of customers
# No of months in current residence:	No of months in current residence of customers
# No of months in current company:	No of months in current company of customers
# Performance Tag:	Status of customer performance (1 represents "Default")

### Credit Card Data ###
# Application ID:	Customer application ID
# No of times 90 DPD or worse in last 6 months:	Number of times customer has not payed dues since 90days in last 6 months
# No of times 60 DPD or worse in last 6 months:	Number of times customer has not payed dues since 60 days last 6 months
# No of times 30 DPD or worse in last 6 months:	Number of times customer has not payed dues since 30 days days last 6 months
# No of times 90 DPD or worse in last 12 months:	Number of times customer has not payed dues since 90 days days last 12 months
# No of times 60 DPD or worse in last 12 months:	Number of times customer has not payed dues since 60 days days last 12 months
# No of times 30 DPD or worse in last 12 months:	Number of times customer has not payed dues since 30 days days last 12 months
# Avgas CC Utilization in last 12 months:	Average utilization of credit card by customer
# No of trades opened in last 6 months:	Number of times the customer has done the trades in last 6 months
# No of trades opened in last 12 months:	Number of times the customer has done the trades in last 12 months
# No of PL trades opened in last 6 months:	No of PL trades in last 6 month of customer
# No of PL trades opened in last 12 months:	No of PL trades in last 12 month of customer
# No of Inquiries in last 6 months (excluding home & auto loans):	Number of times the customers has inquired in last 6 months
# No of Inquiries in last 12 months (excluding home & auto loans):	Number of times the customers has inquired in last 12 months
# Presence of open home loan:	Is the customer has home loan (1 represents "Yes")
# Outstanding Balance:	Outstanding balance of customer
# Total No of Trades:	Number of times the customer has done total trades
# Presence of open auto loan:	Is the customer has auto loan (1 represents "Yes")
# Performance Tag:	Status of customer performance (1 represents "Default")
                                                 
### Exploring the data ####
head(creditBureauData)
str(creditBureauData)
dim(creditBureauData)
sum(is.na(creditBureauData)) ## 3028 nulls in total
colSums(is.na(select_if(creditBureauData, colSums(is.na(creditBureauData))>0))) ## Column having nulls
## The predictor Variable Performance.Tag has nulls, removing the records
creditBureauData <- creditBureauData[is.na(creditBureauData$Performance.Tag)==F,]
sapply(creditBureauData, function(x)
  length(which(x == ""))) # checking for blank "" values; there are none
sum(duplicated(creditBureauData$Application.ID)) ## 3 duplicates

head(demoData)
str(demoData)
dim(demoData)
sum(is.na(demoData)) ## 1428 nulls in total
colSums(is.na(select_if(demoData, colSums(is.na(demoData))>0))) ## Column having nulls
## The predictor Variable Performance.Tag has nulls, removing the records
demoData <- demoData[is.na(demoData$Performance.Tag)==F,]
sapply(demoData, function(x)
  length(which(x == ""))) # checking for blank "" values; there are lots
sum(duplicated(demoData$Application.ID)) # 3 duplicates

demoData[duplicated(demoData$Application.ID)==TRUE,]$Application.ID
creditBureauData[duplicated(creditBureauData$Application.ID)==TRUE,]$Application.ID # We have duplicates for 
# the same Application ID

### EDA and Data Cleansing (For Demographic Data) ###
names(demoData)
## Few column names are big, renaming the same
colnames(demoData)[which(colnames(demoData) %in% c("Marital.Status..at.the.time.of.application.",
                                       "No.of.months.in.current.residence",
                                       "No.of.months.in.current.company") )] <- 
  c("MaritalStatus","currResidenceTenure","currJobTenure")
names(demoData)
## Univariate Analysis can say a lot
p1 <- ggplot(demoData %>% group_by(Performance.Tag) %>% summarise(Count = n())) + 
  geom_bar(aes(Performance.Tag, Count), stat = "identity", fill = "blue2") +
  geom_label(aes(Performance.Tag, Count, label = Count), vjust = 0.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p2 <- ggplot(demoData %>% group_by(Gender) %>% summarise(Count = n())) + 
  geom_bar(aes(Gender, Count), stat = "identity", fill = "blue2") +
  geom_label(aes(Gender, Count, label = Count), vjust = 0.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p3 <- ggplot(demoData %>% group_by(MaritalStatus) %>% summarise(Count = n())) + 
  geom_bar(aes(MaritalStatus, Count), stat = "identity", fill = "blue2") +
  geom_label(aes(MaritalStatus, Count, label = Count), vjust = 0.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p4 <- ggplot(demoData %>% group_by(Education) %>% summarise(Count = n())) + 
  geom_bar(aes(Education, Count), stat = "identity", fill = "blue2") +
  geom_label(aes(Education, Count, label = Count), vjust = 0.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p5 <- ggplot(demoData %>% group_by(Profession) %>% summarise(Count = n())) + 
  geom_bar(aes(Profession, Count), stat = "identity", fill = "blue2") +
  geom_label(aes(Profession, Count, label = Count), vjust = 0.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p6 <- ggplot(demoData %>% group_by(Type.of.residence) %>% summarise(Count = n())) + 
  geom_bar(aes(Type.of.residence, Count), stat = "identity", fill = "blue2") +
  geom_label(aes(Type.of.residence, Count, label = Count), vjust = 0.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot_grid(p1,p2,p3,p5)
plot_grid(p4,p6)
## There seems to be data quality issues, in here, which needs to be fixed
## For few of the columns, there exists blank values, which needs to be taken care of



