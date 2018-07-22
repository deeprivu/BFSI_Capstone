########################## Importing necessary libraries and Packages ##############
library(data.table) # used for reading and manipulation of data
library(dplyr)      # used for data manipulation and joining
library(ggplot2)    # used for ploting
library(lattice)    # used as dependency with caret
library(caret)      # used for modeling
library(corrplot)   # used for making correlation plot
library(cowplot)    # used for combining multiple plots
library(caTools)    # used for train, test spli
library(MASS)       # used for stepAIC
library(ROCR)       # used for ROCR metrics
library(lift)       # used for lift and gain charts
library(car)        # used for vif function
library(woe)        # used for imputing missing values

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

########## Data Quality Issues ###########
"There are 3 duplicates for Application ID, both in credit and Demographic data (same IDs).
There are 1425 istances, where Performance Tag is not available. These records correspond to rejecting the 
application itself without going into the loan
"
##########################################


################### Basic Data Quality Checks and Understanding (For Demographic Data) #############################
head(demoData)
str(demoData)
dim(demoData) # 71295*12
sum(is.na(demoData)) ## 1428 nulls in total
colSums(is.na(select_if(demoData, colSums(is.na(demoData))>0))) ## Column having nulls
## The predictor Variable PerformanceTag has nulls, removing the records (~2%)
names(demoData)
## Few column names are big, renaming the same
colnames(demoData) <- c("ApplicationID","Age","Gender","MaritalStatus","Dependents","Income","Education","Profession",
                        "TypeofResidence","currResidenceTenure","currJobTenure","PerformanceTag")
names(demoData) ## Checking on the exisitng columns names
demoData[is.na(demoData$PerformanceTag)==T,]$PerformanceTag <- 0
sapply(demoData, function(x)
  length(which(x == ""))) # checking for blank "" values; there are lots
sum(duplicated(demoData$ApplicationID)) # 3 duplicates

demoData[duplicated(demoData$ApplicationID)==TRUE,]$ApplicationID
creditBureauData[duplicated(creditBureauData$Application.ID)==TRUE,]$Application.ID # We have duplicates for 
# the same Application ID


################### EDA and Data Cleansing (For Demographic Data) #######################
sapply(demoData, function(x) length(unique(x))) ## Checking the unique values present in the Dataset
sapply(demoData[,c(3,4,5,7,8,9,12)], function(x) unique(x)) ## Looking out for unique values in specific columns

## Converting all the empty strings with NAs ##
demoData <- as.data.table(apply(demoData, 2, function(x) ifelse(x %in% c("", " ", "NA"), NA, x)))
sapply(demoData, function(x) length(which(x == ""))) ## Checking, if all the "" has been replaced with NA

## Converting the required columns to numerical type
demoData$Age <- as.numeric(demoData$Age)
demoData$Income <- as.numeric(demoData$Income)
demoData$currResidenceTenure <- as.numeric(demoData$currResidenceTenure)
demoData$currJobTenure <- as.numeric(demoData$currJobTenure)

## Univariate Analysis can say a lot, checking, if there are any 
p1 <- ggplot(demoData %>% group_by(PerformanceTag) %>% summarise(Count = n()) %>% mutate(freq = format((Count * 100) / sum(Count)))) +  
  geom_bar(aes(PerformanceTag, Count), stat = "identity", fill = "blue2") +
  geom_label(aes(PerformanceTag, Count, label = freq), vjust = 0.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p2 <- ggplot(demoData %>% group_by(Gender, PerformanceTag) %>% summarise(Count = n()) %>% mutate(freq = format((Count * 100) / sum(Count)))) + 
  geom_bar(aes(Gender, Count), stat = "identity", fill = "blue2") +
  geom_label(aes(Gender, Count, label = freq), vjust = 0.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p3 <- ggplot(demoData %>% group_by(MaritalStatus, PerformanceTag) %>% summarise(Count = n()) %>% mutate(freq = format((Count * 100) / sum(Count)))) + 
  geom_bar(aes(MaritalStatus, Count), stat = "identity", fill = "blue2") +
  geom_label(aes(MaritalStatus, Count, label = freq), vjust = 0.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p4 <- ggplot(demoData %>% group_by(Education, PerformanceTag) %>% summarise(Count = n()) %>% mutate(freq = format((Count * 100) / sum(Count)))) + 
  geom_bar(aes(Education, Count), stat = "identity", fill = "blue2") +
  geom_label(aes(Education, Count, label = freq), vjust = 0.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p5 <- ggplot(demoData %>% group_by(Profession, PerformanceTag) %>% summarise(Count = n()) %>% mutate(freq = format((Count * 100) / sum(Count)))) +  
  geom_bar(aes(Profession, Count), stat = "identity", fill = "blue2") +
  geom_label(aes(Profession, Count, label = freq), vjust = 0.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p6 <- ggplot(demoData %>% group_by(TypeofResidence, PerformanceTag) %>% summarise(Count = n()) %>% mutate(freq = format((Count * 100) / sum(Count)))) + 
  geom_bar(aes(TypeofResidence, Count), stat = "identity", fill = "blue2") +
  geom_label(aes(TypeofResidence, Count, label = freq), vjust = 0.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot_grid(p1,p2,p3,p5)
plot_grid(p4,p6)

ggplot(demoData %>% group_by(Dependents, PerformanceTag) %>% summarise(Count = n()) %>% mutate(freq = format((Count * 100) / sum(Count)))) + 
  geom_bar(aes(Dependents, Count), stat = "identity", fill = "blue2", width = .5) +
  geom_label(aes(Dependents, Count, label = freq), vjust = 0.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## There seems to be data quality issues, in here, which needs to be fixed
## For few of the columns, there exists blank values, which needs to be taken care of

## The target Variable PerformanceTag is unbalanced. 0: 66922; 1: 2948; So, ratio is 1:22. Will have to use different sampling techniques
## to balance the data

## Visualisations for the continuous Variables. Looking into the distribution of the variable
p1 <- ggplot(demoData, aes(x=Age)) + geom_histogram(aes(y=..density..),  binwidth=.5,
    colour="black", fill="white") +  geom_density(alpha=.2, fill="#FF6666") +
    geom_vline(aes(xintercept=mean(Age, na.rm=T)), color="red", linetype="dashed", size=1)

p2 <- ggplot(demoData, aes(x=Income)) + geom_histogram(aes(y=..density..),  binwidth=.5,
  colour="black", fill="white") +  geom_density(alpha=.2, fill="#FF6666") +
  geom_vline(aes(xintercept=mean(Income, na.rm=T)), color="red", linetype="dashed", size=1)

p3 <- ggplot(demoData, aes(x=currResidenceTenure)) + geom_histogram(aes(y=..density..),  binwidth=.5,
  colour="black", fill="white") +  geom_density(alpha=.2, fill="#FF6666") +
  geom_vline(aes(xintercept=mean(currResidenceTenure, na.rm=T)), color="red", linetype="dashed", size=1)

p4 <- ggplot(demoData, aes(x=currJobTenure)) + geom_histogram(aes(y=..density..),  binwidth=.5,
  colour="black", fill="white") +  geom_density(alpha=.2, fill="#FF6666") +
  geom_vline(aes(xintercept=mean(currJobTenure, na.rm=T)), color="red", linetype="dashed", size=1)

plot_grid(p1,p2,p3,p4)

## Bivariate Analysis using Violin + Box plots
p1 <- ggplot(demoData, aes(x=PerformanceTag, y=Age)) + geom_violin(trim=FALSE, fill='#A4A4A4', color="darkred") + geom_boxplot(width=0.1)
p2 <- ggplot(demoData, aes(x=PerformanceTag, y=Income)) + geom_violin(trim=FALSE, fill='#A4A4A4', color="darkred") + geom_boxplot(width=0.1)
p3 <- ggplot(demoData, aes(x=PerformanceTag, y=currResidenceTenure)) + geom_violin(trim=FALSE, fill='#A4A4A4', color="darkred") + geom_boxplot(width=0.1)
p4 <- ggplot(demoData, aes(x=PerformanceTag, y=currJobTenure)) + geom_violin(trim=FALSE, fill='#A4A4A4', color="darkred") + geom_boxplot(width=0.1)

plot_grid(p1,p2,p3,p4)
demoData_woe <- demoData
############################ Treating Data Quality Issues #############################
woe_data <- demoData
names(woe_data)

#### Imputing with normal Methodologies #########
colSums(is.na(select_if(demoData, colSums(is.na(demoData))>0)))
getmode <- function(v) { ## Function to calculate Mode
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

demoData[is.na(demoData$Gender)==TRUE]$Gender <- getmode(demoData$Gender)
demoData[is.na(demoData$MaritalStatus)==TRUE]$MaritalStatus <- getmode(demoData$MaritalStatus)
demoData[is.na(demoData$Dependents)==TRUE]$Dependents <- getmode(demoData$Dependents)
demoData[is.na(demoData$Education)==TRUE]$Education <- getmode(demoData$Education)
demoData[is.na(demoData$TypeofResidence)==TRUE]$TypeofResidence <- getmode(demoData$TypeofResidence)
demoData[is.na(demoData$Profession)==TRUE]$Profession <- getmode(demoData$Profession)
sum(is.na(demoData))
### Outlier Treatment ###
sapply(select_if(demoData, is.numeric), function(x)
  quantile(x, seq(0, 1, 0.01))) ## Function to understand the outliers
demoData[(which(demoData$Age < 27)), ]$Age <- 27
demoData[(which(demoData$Income < 4.5)), ]$Income <- 4.5
demoData[(which(demoData$currResidenceTenure > 122)), ]$currResidenceTenure <- 122
demoData[(which(demoData$currJobTenure > 74)), ]$currJobTenure <- 74

### Deriving new variables/Attributes ###
head(demoData)

## Visualisation to help binning out continuous Variables
p1 <- ggplot(demoData %>% group_by(Age, PerformanceTag) %>% summarise(Count_By_PerformanceTag = n())) + geom_point(aes(x=Age, y=Count_By_PerformanceTag, color = PerformanceTag))
p2 <- ggplot(demoData %>% group_by(Income, PerformanceTag) %>% summarise(Count_By_PerformanceTag = n())) + geom_point(aes(x=Income, y=Count_By_PerformanceTag, color = PerformanceTag))
p3 <- ggplot(demoData %>% group_by(currResidenceTenure, PerformanceTag) %>% summarise(Count_By_PerformanceTag = n())) + geom_point(aes(x=currResidenceTenure, y=Count_By_PerformanceTag, color = PerformanceTag))
p4 <- ggplot(demoData %>% group_by(currJobTenure, PerformanceTag) %>% summarise(Count_By_PerformanceTag = n())) + geom_point(aes(x=currJobTenure, y=Count_By_PerformanceTag, color = PerformanceTag))

plot_grid(p1,p2,p3,p4)
## Binning up of COntinuous Variables (On the basis of Quartiles)
sapply(select_if(demoData, is.numeric), function(x)
  quantile(x, seq(0, 1, 0.25)))
demoData$Age_bucket_Quartile <- as.factor(cut(demoData$Age, breaks = c(26, 37, 45, 53, 66)))
demoData$Income_bucket_Quartile <- as.factor(cut(demoData$Income, breaks = c(4.4, 14, 27, 40, 61)))
demoData$currResidenceTenure_bucket_Quartile <- as.factor(cut(demoData$currResidenceTenure, breaks = c(5.9, 10, 61, 123)))
demoData$currJobTenure_bucket_Quartile <- as.factor(cut(demoData$currJobTenure, breaks = c(2.9, 17, 34, 51, 74)))

## Binning up of COntinuous Variables (On the basis of distribution w.r.t PerformanceTag)
demoData$Age_bucket_Response <- as.factor(cut(demoData$Age, breaks = c(26, 36, 56, 66)))
demoData$Income_bucket_Response <- as.factor(cut(demoData$Income, breaks = c(4.4, 45, 61)))
demoData$currJobTenure_bucket_Response <- as.factor(cut(demoData$currJobTenure, breaks = c(2.9, 60, 74)))

## There is a huge peak at some values, we can bin on then basis on that (Performance Tag=0 is high, in there)
demoData$isIncomelessthan5 <- ifelse(demoData$Income<5, 1, 0)
demoData$iscurrResidenceTenurelessthan8 <- ifelse(demoData$currResidenceTenure<8, 1, 0)
demoData$iscurrJobTenurelessthan4 <- ifelse(demoData$currJobTenure<4, 1, 0)

## COnverting columns into required datatypes
ApplicationIDdemo <- demoData[,1]           ## Storing Application ID from Demographic Data
demoData <- demoData[,-1] ## Application_ID will not be required
demoData$Gender <- as.factor(demoData$Gender)
demoData$MaritalStatus <- as.factor(demoData$MaritalStatus)
demoData$Education <- as.factor(demoData$Education)
demoData$Profession <- as.factor(demoData$Profession)
demoData$PerformanceTag <- as.factor(demoData$PerformanceTag)
demoData$TypeofResidence <- as.factor(demoData$TypeofResidence)
demoData$Dependents <- as.numeric(demoData$Dependents)

demoDataCleaned <- demoData ## Shall be used along with Credit Card Bureau Data for final model evaluation 

### Since, no. of categories in different categorical attributes is not high enough, hence, not
## Binnning up the categorical attributes

# One hot encoding
ohe = dummyVars("~.", data = demoData[,c("Gender", "MaritalStatus", "Education", "Profession", "TypeofResidence", "PerformanceTag",
                                          "Age_bucket_Quartile", "Income_bucket_Quartile", "currResidenceTenure_bucket_Quartile",
                                          "currJobTenure_bucket_Quartile", "Age_bucket_Response", "Income_bucket_Response",
                                          "currJobTenure_bucket_Response")], fullRank = T)
ohe_df = data.table(predict(ohe, demoData[,c("Gender", "MaritalStatus", "Education", "Profession", "TypeofResidence", "PerformanceTag",
                                              "Age_bucket_Quartile", "Income_bucket_Quartile", "currResidenceTenure_bucket_Quartile",
                                              "currJobTenure_bucket_Quartile", "Age_bucket_Response", "Income_bucket_Response",
                                              "currJobTenure_bucket_Response")]))
demoData = cbind(demoData[,-c("Gender", "MaritalStatus", "Education", "Profession", "TypeofResidence", "PerformanceTag",
                             "Age_bucket_Quartile", "Income_bucket_Quartile", "currResidenceTenure_bucket_Quartile",
                             "currJobTenure_bucket_Quartile", "Age_bucket_Response", "Income_bucket_Response",
                             "currJobTenure_bucket_Response")], ohe_df)

## Scaling numeric predictors
num_vars = which(sapply(demoData, is.numeric)) # index of numeric features
num_vars_names = names(num_vars)
demoData_numeric = demoData[,setdiff(num_vars_names, "PerformanceTag.1"), with = F]
prep_num = preProcess(demoData_numeric, method=c("center", "scale"))
demoData_numeric_norm = predict(prep_num, demoData_numeric)
demoData[,setdiff(num_vars_names, "PerformanceTag.1") := NULL] # removing numeric independent variables
demoData = cbind(demoData, demoData_numeric_norm)

## Looking into the correlation amongst the variables
cor_matrix = cor(demoData[,-c("PerformanceTag.1")])
#corrplot(cor_matrix, method = "pie", type = "lower", tl.cex = 0.9)

############################## Data Understanding (For Credit Data) ###################################3
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

######################### Basic Data Quality Checks and Understanding (For Credit Data) ####################################
head(creditBureauData)
str(creditBureauData)
dim(creditBureauData) # 71295*19
sum(is.na(creditBureauData)) ## 3028 nulls in total
colSums(is.na(select_if(creditBureauData, colSums(is.na(creditBureauData))>0))) ## Column having nulls
## The predictor Variable PerformanceTag has nulls, replacing the records with 0(~2%)
creditBureauData[is.na(creditBureauData$Performance.Tag)==T,]$Performance.Tag <- 0
sum(sapply(creditBureauData, function(x)
  length(which(x == "")))) # checking for blank "" values; there are none
sum(duplicated(creditBureauData$Application.ID)) ## 3 duplicates

### Quality Issues in the Credit data ###
"Check, if the Performance.Tag is same in both the data sets, corresponding to the same Application ID"

######### Exploratory Data Analysis (For Credit Card Bureau Data) ###########
creditBureauData$Performance.Tag <- as.factor(creditBureauData$Performance.Tag)
## Univariate Analysis
p1 <- ggplot(creditBureauData %>% group_by(Performance.Tag) %>% summarise(Count = n()) %>% mutate(freq = format((Count * 100) / sum(Count)))) +  
  geom_bar(aes(Performance.Tag, Count), stat = "identity", fill = "blue2") +
  geom_label(aes(Performance.Tag, Count, label = freq), vjust = 0.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p2 <- ggplot(creditBureauData %>% group_by(No.of.times.90.DPD.or.worse.in.last.6.months, Performance.Tag) %>% summarise(Count = n()) %>% mutate(freq = format((Count * 100) / sum(Count)))) + 
  geom_bar(aes(No.of.times.90.DPD.or.worse.in.last.6.months, Count), stat = "identity", fill = "blue2") +
  geom_label(aes(No.of.times.90.DPD.or.worse.in.last.6.months, Count, label = freq), vjust = 0.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p3 <- ggplot(creditBureauData %>% group_by(No.of.times.60.DPD.or.worse.in.last.6.months, Performance.Tag) %>% summarise(Count = n()) %>% mutate(freq = format((Count * 100) / sum(Count)))) + 
  geom_bar(aes(No.of.times.60.DPD.or.worse.in.last.6.months, Count), stat = "identity", fill = "blue2") +
  geom_label(aes(No.of.times.60.DPD.or.worse.in.last.6.months, Count, label = freq), vjust = 0.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p4 <- ggplot(creditBureauData %>% group_by(No.of.times.30.DPD.or.worse.in.last.6.months, Performance.Tag) %>% summarise(Count = n()) %>% mutate(freq = format((Count * 100) / sum(Count)))) + 
  geom_bar(aes(No.of.times.30.DPD.or.worse.in.last.6.months, Count), stat = "identity", fill = "blue2") +
  geom_label(aes(No.of.times.30.DPD.or.worse.in.last.6.months, Count, label = freq), vjust = 0.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p5 <- ggplot(creditBureauData %>% group_by(No.of.times.90.DPD.or.worse.in.last.12.months, Performance.Tag) %>% summarise(Count = n()) %>% mutate(freq = format((Count * 100) / sum(Count)))) +  
  geom_bar(aes(No.of.times.90.DPD.or.worse.in.last.12.months, Count), stat = "identity", fill = "blue2") +
  geom_label(aes(No.of.times.90.DPD.or.worse.in.last.12.months, Count, label = freq), vjust = 0.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p6 <- ggplot(creditBureauData %>% group_by(No.of.times.60.DPD.or.worse.in.last.12.months, Performance.Tag) %>% summarise(Count = n()) %>% mutate(freq = format((Count * 100) / sum(Count)))) + 
  geom_bar(aes(No.of.times.60.DPD.or.worse.in.last.12.months, Count), stat = "identity", fill = "blue2") +
  geom_label(aes(No.of.times.60.DPD.or.worse.in.last.12.months, Count, label = freq), vjust = 0.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p7 <- ggplot(creditBureauData %>% group_by(No.of.times.30.DPD.or.worse.in.last.12.months, Performance.Tag) %>% summarise(Count = n()) %>% mutate(freq = format((Count * 100) / sum(Count)))) +  
  geom_bar(aes(No.of.times.30.DPD.or.worse.in.last.12.months, Count), stat = "identity", fill = "blue2") +
  geom_label(aes(No.of.times.30.DPD.or.worse.in.last.12.months, Count, label = freq), vjust = 0.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p8 <- ggplot(creditBureauData %>% group_by(No.of.trades.opened.in.last.6.months, Performance.Tag) %>% summarise(Count = n()) %>% mutate(freq = format((Count * 100) / sum(Count)))) + 
  geom_bar(aes(No.of.trades.opened.in.last.6.months, Count), stat = "identity", fill = "blue2") +
  geom_label(aes(No.of.trades.opened.in.last.6.months, Count, label = freq), vjust = 0.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot_grid(p1,p2,p3,p4)
plot_grid(p5,p6,p7,p8)

p1 <- ggplot(creditBureauData, aes(x=Performance.Tag, y=No.of.trades.opened.in.last.12.months)) + geom_violin(trim=FALSE, fill='#A4A4A4', color="darkred") + geom_boxplot(width=0.1)
p2 <- ggplot(creditBureauData, aes(x=Performance.Tag, y=No.of.PL.trades.opened.in.last.6.months)) + geom_violin(trim=FALSE, fill='#A4A4A4', color="darkred") + geom_boxplot(width=0.1)
p3 <- ggplot(creditBureauData, aes(x=Performance.Tag, y=No.of.PL.trades.opened.in.last.12.months)) + geom_violin(trim=FALSE, fill='#A4A4A4', color="darkred") + geom_boxplot(width=0.1)
p4 <- ggplot(creditBureauData, aes(x=Performance.Tag, y=No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.)) + geom_violin(trim=FALSE, fill='#A4A4A4', color="darkred") + geom_boxplot(width=0.1)
p5 <- ggplot(creditBureauData, aes(x=Performance.Tag, y=No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.)) + geom_violin(trim=FALSE, fill='#A4A4A4', color="darkred") + geom_boxplot(width=0.1)

p6 <- ggplot(creditBureauData %>% group_by(Presence.of.open.home.loan, Performance.Tag) %>% summarise(Count = n()) %>% mutate(freq = format((Count * 100) / sum(Count)))) + 
  geom_bar(aes(Presence.of.open.home.loan, Count), stat = "identity", fill = "blue2") +
  geom_label(aes(Presence.of.open.home.loan, Count, label = freq), vjust = 0.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p7 <- ggplot(creditBureauData, aes(x=Performance.Tag, y=Total.No.of.Trades)) + geom_violin(trim=FALSE, fill='#A4A4A4', color="darkred") + geom_boxplot(width=0.1)

p8 <- ggplot(creditBureauData %>% group_by(Presence.of.open.auto.loan, Performance.Tag) %>% summarise(Count = n()) %>% mutate(freq = format((Count * 100) / sum(Count)))) + 
  geom_bar(aes(Presence.of.open.auto.loan, Count), stat = "identity", fill = "blue2") +
  geom_label(aes(Presence.of.open.auto.loan, Count, label = freq), vjust = 0.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p9 <- ggplot(creditBureauData, aes(x=Performance.Tag, y=Avgas.CC.Utilization.in.last.12.months)) + geom_violin(trim=FALSE, fill='#A4A4A4', color="darkred") + geom_boxplot(width=0.1)
p10 <- ggplot(creditBureauData, aes(x=Performance.Tag, y=Outstanding.Balance)) + geom_violin(trim=FALSE, fill='#A4A4A4', color="darkred") + geom_boxplot(width=0.1)


plot_grid(p2,p3,p4,p5)
plot_grid(p6,p8)
plot_grid(p1,p7,p9,p10)

## Checking for the normallity of the Continuous Variables
p1 <- ggplot(creditBureauData, aes(x=Avgas.CC.Utilization.in.last.12.months)) + geom_histogram(aes(y=..density..),  binwidth=.5,
                                                    colour="black", fill="white") +  geom_density(alpha=.2, fill="#FF6666") +
  geom_vline(aes(xintercept=mean(Avgas.CC.Utilization.in.last.12.months, na.rm=T)), color="red", linetype="dashed", size=1)

p2 <- ggplot(creditBureauData, aes(x=(Outstanding.Balance/10000))) + geom_histogram(aes(y=..density..),  binwidth=.5,
                                                       colour="black", fill="white") +  geom_density(alpha=.2, fill="#FF6666") +
  geom_vline(aes(xintercept=mean(Outstanding.Balance, na.rm=T)/10000), color="red", linetype="dashed", size=1)

plot_grid(p1,p2)
creditBureauData_woe <- creditBureauData
### Imputation using normal Methodologies (Mean, Median, Mode)###
sapply(creditBureauData[, colnames(creditBureauData) %in% c("Avgas.CC.Utilization.in.last.12.months","No.of.trades.opened.in.last.6.months",
                                                            "Presence.of.open.home.loan")], function(x) unique(x))

## Changing the column names, replacing the '.' with '_'
colnames(creditBureauData) <- gsub("\\.","_",names(creditBureauData))
colSums(is.na(select_if(creditBureauData, colSums(is.na(creditBureauData))>0))) ## Column having nulls
creditBureauData <- as.data.table(creditBureauData)
creditBureauData[is.na(creditBureauData$Avgas_CC_Utilization_in_last_12_months)==TRUE]$Avgas_CC_Utilization_in_last_12_months <- getmode(creditBureauData$Avgas_CC_Utilization_in_last_12_months)
creditBureauData[is.na(creditBureauData$No_of_trades_opened_in_last_6_months)==TRUE]$No_of_trades_opened_in_last_6_months <- getmode(creditBureauData$No_of_trades_opened_in_last_6_months)
creditBureauData[is.na(creditBureauData$Presence_of_open_home_loan)==TRUE]$Presence_of_open_home_loan <- getmode(creditBureauData$Presence_of_open_home_loan)
creditBureauData[is.na(creditBureauData$Outstanding_Balance)==TRUE]$Outstanding_Balance <- as.integer(median(creditBureauData$Outstanding_Balance, na.rm = TRUE))
sum(is.na(creditBureauData)) ## All missing Values imputed

## ## Checking for the Outliers
sapply(creditBureauData, function(x) length(unique(x)))
ApplicationIDcredit <- creditBureauData[,1] ## Storing Application ID from Credit Bureau Data
creditBureauData <- creditBureauData[,-1] ## Removing Application_ID
summary(creditBureauData)
sapply(creditBureauData[,-18], function(x) quantile(x, seq(0, 1, 0.01)))
creditBureauData[(which(creditBureauData$Total_No_of_Trades > 44)), ]$Total_No_of_Trades <- 44
creditBureauData[(which(creditBureauData$Outstanding_Balance < 1251.07)), ]$Outstanding_Balance <- 1251
creditBureauData[(which(creditBureauData$No_of_Inquiries_in_last_12_months__excluding_home___auto_loans_ > 15)), ]$No_of_Inquiries_in_last_12_months__excluding_home___auto_loans_ <- 20
creditBureauData[(which(creditBureauData$No_of_trades_opened_in_last_12_months > 21)), ]$No_of_trades_opened_in_last_12_months <- 21


## Binning up of Continuous Variables and Feature Engineering
sapply(creditBureauData[,15], function(x) quantile(x, seq(0, 1, 0.25)))
creditBureauData$Outstanding_Balance_Quartile <- as.factor(cut(creditBureauData$Outstanding_Balance, breaks = c(1250, 209061, 774234, 2924615, 5218802)))

## One Hot Encoding
ohe = dummyVars("~.", data = creditBureauData[,c("Outstanding_Balance_Quartile", "Performance_Tag")], fullRank = T)
ohe_df = data.table(predict(ohe, creditBureauData[,c("Outstanding_Balance_Quartile", "Performance_Tag")]))
creditBureauData = cbind(creditBureauData[,-c("Outstanding_Balance_Quartile", "Performance_Tag")], ohe_df)
creditBureauData <- as.data.table(sapply( creditBureauData, as.numeric ))

## Scaling numeric predictors
num_vars = which(sapply(creditBureauData, is.numeric)) # index of numeric features
num_vars_names = names(num_vars)
creditBureauData_numeric = creditBureauData[,setdiff(num_vars_names, "Performance_Tag.1"), with = F]
prep_num = preProcess(creditBureauData_numeric, method=c("center", "scale"))
creditBureauData_numeric_norm = predict(prep_num, creditBureauData_numeric)
creditBureauData[,setdiff(num_vars_names, "Performance_Tag.1") := NULL] # removing numeric independent variables
creditBureauData = cbind(creditBureauData, creditBureauData_numeric_norm)

## Looking into the correlation amongst the variables
cor_matrix = cor(creditBureauData[,-c("Performance_Tag.1")])
#corrplot(cor_matrix, method = "pie", type = "lower", tl.cex = 0.9)
## From the plot, we can see that, there exists a strong correlation amongst the variables

############################# Preparing the final Data for Modelling ##########################
### Initial Checks ###
dim(ApplicationIDcredit); dim(ApplicationIDdemo)
nrow(demoData); nrow(creditBureauData)

### Joining the datasets to prepare the final DataSet for final Modelling
finalData <- as.data.table(cbind(ApplicationIDcredit,demoData,creditBureauData))
str(finalData)

## Checking the Values in the target Attribute
setdiff(finalData$Performance_Tag.1, finalData$PerformanceTag.1) ## No Difference :)
finalData <- finalData[,-c("Performance_Tag.1","Application_ID")]
cor_matrix = cor(finalData[,-c("PerformanceTag.1")])
#corrplot(cor_matrix, method = "pie", type = "lower", tl.cex = 0.9) ## Correlation in the Final Data Set


############################### Model Building Phase for Demo Data (using Logistic Regression) ########################
# separate training and testing data
set.seed(100)
trainindices= sample(1:nrow(demoData), 0.7*nrow(demoData))
train = demoData[trainindices,]
test = demoData[-trainindices,]

## Logistic Regression ##
## Initial Model using all the variables of Demo_Data ##
model_1 <-glm(PerformanceTag.1~.,family = "binomial", data=train)
summary(model_1)

## Running stepAID
step <- stepAIC(model_1, direction="both")
summary(step)

## Fine tuning model 
model_2 <- glm(formula = PerformanceTag.1 ~ Income + currResidenceTenure + 
                 currJobTenure + iscurrResidenceTenurelessthan8 + iscurrJobTenurelessthan4 + 
                 Profession.SE + `Age_bucket_Quartile.(37,45]` + `Income_bucket_Quartile.(40,61]` + 
                 `currResidenceTenure_bucket_Quartile.(10,61]` + `currResidenceTenure_bucket_Quartile.(61,123]` + 
                 `currJobTenure_bucket_Quartile.(17,34]` + `currJobTenure_bucket_Response.(60,74]`, 
               family = "binomial", data = train)
summary(model_2)
vif(model_2)

model_3 <- glm(formula = PerformanceTag.1 ~ Income + currResidenceTenure + 
                 currJobTenure + iscurrResidenceTenurelessthan8 + iscurrJobTenurelessthan4 + 
                 Profession.SE + `Age_bucket_Quartile.(37,45]` + `Income_bucket_Quartile.(40,61]` + 
                 `currResidenceTenure_bucket_Quartile.(10,61]` + `currJobTenure_bucket_Quartile.(17,34]` + `currJobTenure_bucket_Response.(60,74]`, 
               family = "binomial", data = train)
summary(model_3)
vif(model_3)

model_4 <- glm(formula = PerformanceTag.1 ~ Income + currResidenceTenure + 
                 currJobTenure + iscurrResidenceTenurelessthan8 + iscurrJobTenurelessthan4 + 
                 Profession.SE + `Age_bucket_Quartile.(37,45]` +  
                 `currResidenceTenure_bucket_Quartile.(10,61]` + `currJobTenure_bucket_Quartile.(17,34]` + `currJobTenure_bucket_Response.(60,74]`, 
               family = "binomial", data = train)
summary(model_4)
vif(model_4)

model_5 <- glm(formula = PerformanceTag.1 ~ Income + currResidenceTenure + 
                 currJobTenure + iscurrResidenceTenurelessthan8 + iscurrJobTenurelessthan4 + 
                 Profession.SE + `Age_bucket_Quartile.(37,45]` + `currJobTenure_bucket_Quartile.(17,34]` + `currJobTenure_bucket_Response.(60,74]`, 
               family = "binomial", data = train)
summary(model_5)
vif(model_5)

model_6 <- glm(formula = PerformanceTag.1 ~ Income + currResidenceTenure + 
                 currJobTenure + iscurrResidenceTenurelessthan8 + iscurrJobTenurelessthan4 + 
                 Profession.SE + `currJobTenure_bucket_Quartile.(17,34]` + `currJobTenure_bucket_Response.(60,74]`, 
               family = "binomial", data = train)
summary(model_6)

model_7 <- glm(formula = PerformanceTag.1 ~ Income + currResidenceTenure + 
                 currJobTenure + iscurrResidenceTenurelessthan8 + iscurrJobTenurelessthan4 + 
                 `currJobTenure_bucket_Quartile.(17,34]` + `currJobTenure_bucket_Response.(60,74]`, 
               family = "binomial", data = train)
summary(model_7)

model_8 <- glm(formula = PerformanceTag.1 ~ Income + currResidenceTenure + 
                 currJobTenure + iscurrResidenceTenurelessthan8 + iscurrJobTenurelessthan4 + 
                 `currJobTenure_bucket_Response.(60,74]`, family = "binomial",
               data = train)
summary(model_8)

model_9 <- glm(formula = PerformanceTag.1 ~ Income + currResidenceTenure + 
                 currJobTenure + iscurrResidenceTenurelessthan8 + `currJobTenure_bucket_Response.(60,74]`, 
               family = "binomial", data = train)
summary(model_9)

final_model_demoData <- model_9
# predicting the results in test dataset
test$test_predict = predict(final_model_demoData, 
                    newdata = test[,-1])

# Let's see the summary 
summary(test$test_predict)

# Let's use the probability cutoff of 5%.

test_pred <- factor(ifelse(test$test_predict >= 0.05, "1", "0"))
test_actual <- factor(test$PerformanceTag.1)

# Matrix
table(test_pred,test_actual)

library(e1071)
test_conf <- confusionMatrix(test_pred, test_actual, positive = "1")
test_conf

## Finding the best threshold value
perform_fn <- function(cutoff) 
{
  predicted <- factor(ifelse(test$test_predict >= cutoff, "1", "0"))
  conf <- confusionMatrix(predicted, test_actual, positive = "1")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

# Summary of test probability
summary(test$test_predict)
s = seq(.01,.5,length=100)
OUT = matrix(0,100,3)
for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 

plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"lightgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))


cutoff <- s[which.min(abs(OUT[, 1] - OUT[, 2]))]
cutoff 
# Let's choose a cutoff value of 0.04464646 for final model

test_cutoff <- factor(ifelse(test$test_predict >= 0.04464646, "1", "0"))

conf_final <- confusionMatrix(test_cutoff, test_actual, positive = "1")
acc <- conf_final$overall[1]
sens <- conf_final$byClass[1]
spec <- conf_final$byClass[2]

acc
# Accuracy 
# 0.6192453
sens
# Sensitivity 
# 0.5210643 
spec
# Specificity 
# 0.6236602 

######################## Model Building for finalData (using Logistic Regression) ##########################
set.seed(100)
trainindices= sample(1:nrow(finalData), 0.7*nrow(finalData))
train = finalData[trainindices,]
test = finalData[-trainindices,]

## Logistic Regression ##
## Initial Model using all the variables of Demo_Data ##
model_1 <-glm(PerformanceTag.1~.,family = "binomial", data=train)
summary(model_1)

## Running stepAIC
step <- stepAIC(model_1, direction="both")
summary(step)
vif(step)

### Fine tuning the Model ###
model_2 <- glm(formula = PerformanceTag.1 ~ Age + iscurrResidenceTenurelessthan8 + 
                 Gender.M + Profession.SE + `Age_bucket_Quartile.(37,45]` + 
                 `Age_bucket_Quartile.(45,53]` + `Age_bucket_Quartile.(53,66]` + 
                 `currResidenceTenure_bucket_Quartile.(10,61]` + `currResidenceTenure_bucket_Quartile.(61,123]` + 
                 `currJobTenure_bucket_Quartile.(17,34]` + `currJobTenure_bucket_Quartile.(34,51]` + 
                 `currJobTenure_bucket_Quartile.(51,74]` + `Age_bucket_Response.(36,56]` + 
                 No_of_times_90_DPD_or_worse_in_last_6_months + 
                 No_of_times_30_DPD_or_worse_in_last_6_months + No_of_times_90_DPD_or_worse_in_last_12_months + 
                 Avgas_CC_Utilization_in_last_12_months + No_of_PL_trades_opened_in_last_6_months + 
                 No_of_PL_trades_opened_in_last_12_months + No_of_Inquiries_in_last_6_months__excluding_home___auto_loans_ + 
                 No_of_Inquiries_in_last_12_months__excluding_home___auto_loans_ + 
                 Total_No_of_Trades + `Outstanding_Balance_Quartile.(2.09e+05,7.74e+05]`, 
               family = "binomial",data = train)
summary(model_2)
vif(model_2)

model_3 <- glm(formula = PerformanceTag.1 ~ Age +  
                 Gender.M + Profession.SE + `Age_bucket_Quartile.(37,45]` + 
                 `Age_bucket_Quartile.(45,53]` + `Age_bucket_Quartile.(53,66]` + 
                 `currResidenceTenure_bucket_Quartile.(10,61]` + `currResidenceTenure_bucket_Quartile.(61,123]` + 
                 `currJobTenure_bucket_Quartile.(17,34]` + `currJobTenure_bucket_Quartile.(34,51]` + 
                 `currJobTenure_bucket_Quartile.(51,74]` + `Age_bucket_Response.(36,56]` + 
                 No_of_times_90_DPD_or_worse_in_last_6_months + 
                 No_of_times_30_DPD_or_worse_in_last_6_months + No_of_times_90_DPD_or_worse_in_last_12_months + 
                 Avgas_CC_Utilization_in_last_12_months + No_of_PL_trades_opened_in_last_6_months + 
                 No_of_PL_trades_opened_in_last_12_months + No_of_Inquiries_in_last_6_months__excluding_home___auto_loans_ + 
                 No_of_Inquiries_in_last_12_months__excluding_home___auto_loans_ + 
                 Total_No_of_Trades + `Outstanding_Balance_Quartile.(2.09e+05,7.74e+05]`, 
               family = "binomial", data = train)
summary(model_3)
vif(model_3)

model_4 <- glm(formula = PerformanceTag.1 ~   
                 Gender.M + Profession.SE + `Age_bucket_Quartile.(37,45]` + 
                 `Age_bucket_Quartile.(45,53]` + `Age_bucket_Quartile.(53,66]` + 
                 `currResidenceTenure_bucket_Quartile.(10,61]` + `currResidenceTenure_bucket_Quartile.(61,123]` + 
                 `currJobTenure_bucket_Quartile.(17,34]` + `currJobTenure_bucket_Quartile.(34,51]` + 
                 `currJobTenure_bucket_Quartile.(51,74]` + `Age_bucket_Response.(36,56]` + 
                 No_of_times_90_DPD_or_worse_in_last_6_months + 
                 No_of_times_30_DPD_or_worse_in_last_6_months + No_of_times_90_DPD_or_worse_in_last_12_months + 
                 Avgas_CC_Utilization_in_last_12_months + No_of_PL_trades_opened_in_last_6_months + 
                 No_of_PL_trades_opened_in_last_12_months + No_of_Inquiries_in_last_6_months__excluding_home___auto_loans_ + 
                 No_of_Inquiries_in_last_12_months__excluding_home___auto_loans_ + 
                 Total_No_of_Trades + `Outstanding_Balance_Quartile.(2.09e+05,7.74e+05]`, 
               family = "binomial", data = train)
summary(model_4)
vif(model_4)

model_5 <- glm(formula = PerformanceTag.1 ~   
                 Gender.M + Profession.SE + `Age_bucket_Quartile.(37,45]` + 
                 `Age_bucket_Quartile.(45,53]` + `Age_bucket_Quartile.(53,66]` + 
                 `currResidenceTenure_bucket_Quartile.(10,61]` + `currResidenceTenure_bucket_Quartile.(61,123]` + 
                 `currJobTenure_bucket_Quartile.(17,34]` + `currJobTenure_bucket_Quartile.(34,51]` + 
                 `currJobTenure_bucket_Quartile.(51,74]` + `Age_bucket_Response.(36,56]` + 
                 No_of_times_90_DPD_or_worse_in_last_6_months + 
                 No_of_times_30_DPD_or_worse_in_last_6_months + No_of_times_90_DPD_or_worse_in_last_12_months + 
                 Avgas_CC_Utilization_in_last_12_months + No_of_PL_trades_opened_in_last_12_months + No_of_Inquiries_in_last_12_months__excluding_home___auto_loans_ + 
                 Total_No_of_Trades + `Outstanding_Balance_Quartile.(2.09e+05,7.74e+05]`, 
               family = "binomial", data = train)
summary(model_5)
vif(model_5)

model_6 <- glm(formula = PerformanceTag.1 ~   
                 Gender.M + Profession.SE + `Age_bucket_Quartile.(45,53]` + `Age_bucket_Quartile.(53,66]` + 
                 `currResidenceTenure_bucket_Quartile.(10,61]` + `currResidenceTenure_bucket_Quartile.(61,123]` + 
                 `currJobTenure_bucket_Quartile.(17,34]` + `currJobTenure_bucket_Quartile.(34,51]` + 
                 `currJobTenure_bucket_Quartile.(51,74]` + `Age_bucket_Response.(36,56]` + 
                 No_of_times_90_DPD_or_worse_in_last_6_months + 
                 No_of_times_30_DPD_or_worse_in_last_6_months + No_of_times_90_DPD_or_worse_in_last_12_months + 
                 Avgas_CC_Utilization_in_last_12_months + No_of_PL_trades_opened_in_last_12_months + No_of_Inquiries_in_last_12_months__excluding_home___auto_loans_ + 
                 Total_No_of_Trades + `Outstanding_Balance_Quartile.(2.09e+05,7.74e+05]`, 
               family = "binomial", data = train)
summary(model_6)
vif(model_6)

model_7 <- glm(formula = PerformanceTag.1 ~ Profession.SE + `Age_bucket_Quartile.(45,53]` + `Age_bucket_Quartile.(53,66]` + 
                 `currResidenceTenure_bucket_Quartile.(10,61]` + `currResidenceTenure_bucket_Quartile.(61,123]` + 
                 `currJobTenure_bucket_Quartile.(17,34]` + `currJobTenure_bucket_Quartile.(34,51]` + 
                 `currJobTenure_bucket_Quartile.(51,74]` + `Age_bucket_Response.(36,56]` + 
                 No_of_times_90_DPD_or_worse_in_last_6_months + 
                 No_of_times_30_DPD_or_worse_in_last_6_months + No_of_times_90_DPD_or_worse_in_last_12_months + 
                 Avgas_CC_Utilization_in_last_12_months + No_of_PL_trades_opened_in_last_12_months + No_of_Inquiries_in_last_12_months__excluding_home___auto_loans_ + 
                 Total_No_of_Trades + `Outstanding_Balance_Quartile.(2.09e+05,7.74e+05]`, 
               family = "binomial", data = train)
summary(model_7)

model_8 <- glm(formula = PerformanceTag.1 ~ `Age_bucket_Quartile.(45,53]` + `Age_bucket_Quartile.(53,66]` + 
                 `currResidenceTenure_bucket_Quartile.(10,61]` + `currResidenceTenure_bucket_Quartile.(61,123]` + 
                 `currJobTenure_bucket_Quartile.(17,34]` + `currJobTenure_bucket_Quartile.(34,51]` + 
                 `currJobTenure_bucket_Quartile.(51,74]` + `Age_bucket_Response.(36,56]` + 
                 No_of_times_90_DPD_or_worse_in_last_6_months + 
                 No_of_times_30_DPD_or_worse_in_last_6_months + No_of_times_90_DPD_or_worse_in_last_12_months + 
                 Avgas_CC_Utilization_in_last_12_months + No_of_PL_trades_opened_in_last_12_months + No_of_Inquiries_in_last_12_months__excluding_home___auto_loans_ + 
                 Total_No_of_Trades + `Outstanding_Balance_Quartile.(2.09e+05,7.74e+05]`, 
               family = "binomial", data = train)
summary(model_8)

model_9 <- glm(formula = PerformanceTag.1 ~ `Age_bucket_Quartile.(53,66]` + 
                 `currResidenceTenure_bucket_Quartile.(10,61]` + `currResidenceTenure_bucket_Quartile.(61,123]` + 
                 `currJobTenure_bucket_Quartile.(17,34]` + `currJobTenure_bucket_Quartile.(34,51]` + 
                 `currJobTenure_bucket_Quartile.(51,74]` + `Age_bucket_Response.(36,56]` + 
                 No_of_times_90_DPD_or_worse_in_last_6_months + 
                 No_of_times_30_DPD_or_worse_in_last_6_months + No_of_times_90_DPD_or_worse_in_last_12_months + 
                 Avgas_CC_Utilization_in_last_12_months + No_of_PL_trades_opened_in_last_12_months + No_of_Inquiries_in_last_12_months__excluding_home___auto_loans_ + 
                 Total_No_of_Trades + `Outstanding_Balance_Quartile.(2.09e+05,7.74e+05]`, 
               family = "binomial", data = train)
summary(model_9)

model_10 <- glm(formula = PerformanceTag.1 ~ `currResidenceTenure_bucket_Quartile.(10,61]` + `currResidenceTenure_bucket_Quartile.(61,123]` + 
                 `currJobTenure_bucket_Quartile.(17,34]` + `currJobTenure_bucket_Quartile.(34,51]` + 
                 `currJobTenure_bucket_Quartile.(51,74]` + `Age_bucket_Response.(36,56]` + 
                 No_of_times_90_DPD_or_worse_in_last_6_months + 
                 No_of_times_30_DPD_or_worse_in_last_6_months + No_of_times_90_DPD_or_worse_in_last_12_months + 
                 Avgas_CC_Utilization_in_last_12_months + No_of_PL_trades_opened_in_last_12_months + No_of_Inquiries_in_last_12_months__excluding_home___auto_loans_ + 
                 Total_No_of_Trades + `Outstanding_Balance_Quartile.(2.09e+05,7.74e+05]`, 
                family = "binomial", data = train)
summary(model_10)

model_11 <- glm(formula = PerformanceTag.1 ~ `currResidenceTenure_bucket_Quartile.(61,123]` + 
                  `currJobTenure_bucket_Quartile.(17,34]` + `currJobTenure_bucket_Quartile.(34,51]` + 
                  `currJobTenure_bucket_Quartile.(51,74]` + `Age_bucket_Response.(36,56]` + 
                  No_of_times_90_DPD_or_worse_in_last_6_months + 
                  No_of_times_30_DPD_or_worse_in_last_6_months + No_of_times_90_DPD_or_worse_in_last_12_months + 
                  Avgas_CC_Utilization_in_last_12_months + No_of_PL_trades_opened_in_last_12_months + No_of_Inquiries_in_last_12_months__excluding_home___auto_loans_ + 
                  Total_No_of_Trades + `Outstanding_Balance_Quartile.(2.09e+05,7.74e+05]`, 
                family = "binomial", data = train)
summary(model_11)

model_12 <- glm(formula = PerformanceTag.1 ~ `currResidenceTenure_bucket_Quartile.(61,123]` + 
                  `currJobTenure_bucket_Quartile.(17,34]` + `currJobTenure_bucket_Quartile.(34,51]` + 
                  `currJobTenure_bucket_Quartile.(51,74]` + No_of_times_90_DPD_or_worse_in_last_6_months + 
                  No_of_times_30_DPD_or_worse_in_last_6_months + No_of_times_90_DPD_or_worse_in_last_12_months + 
                  Avgas_CC_Utilization_in_last_12_months + No_of_PL_trades_opened_in_last_12_months + No_of_Inquiries_in_last_12_months__excluding_home___auto_loans_ + 
                  Total_No_of_Trades + `Outstanding_Balance_Quartile.(2.09e+05,7.74e+05]`, 
                family = "binomial", data = train)
summary(model_12)
vif(model_12)

model_13 <- glm(formula = PerformanceTag.1 ~ `currResidenceTenure_bucket_Quartile.(61,123]` + 
                  `currJobTenure_bucket_Quartile.(17,34]` + `currJobTenure_bucket_Quartile.(51,74]` + No_of_times_90_DPD_or_worse_in_last_6_months + 
                  No_of_times_30_DPD_or_worse_in_last_6_months + No_of_times_90_DPD_or_worse_in_last_12_months + 
                  Avgas_CC_Utilization_in_last_12_months + No_of_PL_trades_opened_in_last_12_months + No_of_Inquiries_in_last_12_months__excluding_home___auto_loans_ + 
                  Total_No_of_Trades + `Outstanding_Balance_Quartile.(2.09e+05,7.74e+05]`, 
                family = "binomial", data = train)
summary(model_13)

model_14 <- glm(formula = PerformanceTag.1 ~ `currResidenceTenure_bucket_Quartile.(61,123]` + 
                  `currJobTenure_bucket_Quartile.(17,34]` + No_of_times_90_DPD_or_worse_in_last_6_months + 
                  No_of_times_30_DPD_or_worse_in_last_6_months + No_of_times_90_DPD_or_worse_in_last_12_months + 
                  Avgas_CC_Utilization_in_last_12_months + No_of_PL_trades_opened_in_last_12_months + No_of_Inquiries_in_last_12_months__excluding_home___auto_loans_ + 
                  Total_No_of_Trades + `Outstanding_Balance_Quartile.(2.09e+05,7.74e+05]`, 
                family = "binomial", data = train)
summary(model_14)
vif(model_14)

model_15 <- glm(formula = PerformanceTag.1 ~ `currResidenceTenure_bucket_Quartile.(61,123]` + 
                  `currJobTenure_bucket_Quartile.(17,34]` + No_of_times_90_DPD_or_worse_in_last_6_months + 
                  No_of_times_30_DPD_or_worse_in_last_6_months + No_of_times_90_DPD_or_worse_in_last_12_months + 
                  Avgas_CC_Utilization_in_last_12_months + No_of_PL_trades_opened_in_last_12_months + 
                  Total_No_of_Trades + `Outstanding_Balance_Quartile.(2.09e+05,7.74e+05]`, 
                family = "binomial", data = train)
summary(model_15)

model_16 <- glm(formula = PerformanceTag.1 ~ `currJobTenure_bucket_Quartile.(17,34]` + No_of_times_90_DPD_or_worse_in_last_6_months + 
                  No_of_times_30_DPD_or_worse_in_last_6_months + No_of_times_90_DPD_or_worse_in_last_12_months + 
                  Avgas_CC_Utilization_in_last_12_months + No_of_PL_trades_opened_in_last_12_months + 
                  Total_No_of_Trades + `Outstanding_Balance_Quartile.(2.09e+05,7.74e+05]`, 
                family = "binomial", data = train)
summary(model_16)

model_17 <- glm(formula = PerformanceTag.1 ~ No_of_times_90_DPD_or_worse_in_last_6_months + 
                  No_of_times_30_DPD_or_worse_in_last_6_months + No_of_times_90_DPD_or_worse_in_last_12_months + 
                  Avgas_CC_Utilization_in_last_12_months + No_of_PL_trades_opened_in_last_12_months + 
                  Total_No_of_Trades + `Outstanding_Balance_Quartile.(2.09e+05,7.74e+05]`, 
                family = "binomial", data = train)
summary(model_17)
vif(model_17)

model_18 <- glm(formula = PerformanceTag.1 ~ No_of_times_90_DPD_or_worse_in_last_6_months + 
                  No_of_times_30_DPD_or_worse_in_last_6_months + No_of_times_90_DPD_or_worse_in_last_12_months + 
                  Avgas_CC_Utilization_in_last_12_months + No_of_PL_trades_opened_in_last_12_months + `Outstanding_Balance_Quartile.(2.09e+05,7.74e+05]`, 
                family = "binomial", data = train)
summary(model_18)
vif(model_18)

model_19 <- glm(formula = PerformanceTag.1 ~ No_of_times_30_DPD_or_worse_in_last_6_months + No_of_times_90_DPD_or_worse_in_last_12_months + 
                  Avgas_CC_Utilization_in_last_12_months + No_of_PL_trades_opened_in_last_12_months + `Outstanding_Balance_Quartile.(2.09e+05,7.74e+05]`, 
                family = "binomial", data = train)
summary(model_19)
vif(model_19)

final_model_finalData <- model_19

# predicting the results in test dataset
test$test_predict = predict(final_model_finalData, 
                            newdata = test[,-1])

# Let's see the summary 
summary(test$test_predict)

# Let's use the probability cutoff of 5%.

test_pred <- factor(ifelse(test$test_predict >= 0.05, "1", "0"))
test_actual <- factor(test$PerformanceTag.1)

# Matrix
table(test_pred,test_actual)

library(e1071)
test_conf <- confusionMatrix(test_pred, test_actual, positive = "1")
test_conf

## Finding the best threshold value
perform_fn <- function(cutoff) 
{
  predicted <- factor(ifelse(test$test_predict >= cutoff, "1", "0"))
  conf <- confusionMatrix(predicted, test_actual, positive = "1")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

# Summary of test probability
summary(test$test_predict)
s = seq(.01,.5,length=100)
OUT = matrix(0,100,3)
for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 

plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"lightgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))


cutoff <- s[which.min(abs(OUT[, 1] - OUT[, 2]))]
cutoff 
# Let's choose a cutoff value of 0.04959596 for final model

test_cutoff <- factor(ifelse(test$test_predict >=cutoff, "1", "0"))

conf_final <- confusionMatrix(test_cutoff, test_actual, positive = "1")
acc <- conf_final$overall[1]
sens <- conf_final$byClass[1]
spec <- conf_final$byClass[2]

acc
# Accuracy 
# 0.6493965
sens
# Sensitivity 
# 0.5931264 
spec
# Specificity 
# 0.6519268

#### On the basis of basic Logistic Modelling, we are getting an accuracy of ~65% for both the data sets ###

############################# Model Building for demoData (using xgboost) ############################
## Preprocessing ###
gc(); rm(model_1,model_2,model_3,model_4,model_5,model_6,model_7,model_8,model_9,model_10,model_11,model_12,model_13)
rm(model_14,model_15,model_16,model_17,model_18,model_19,step,p1,p2,p3,p4,p5,p6,p7,p8,p9,p10)
gc()

### Initial Quality Checks ###
sum(is.na(demoData)) ## 0 nulls
sum(sapply(demoData, function(x)
  length(which(x == "")))) # checking for blank "" values; there are none, now

demoData %>% group_by(PerformanceTag.1) %>% summarise(n = n()) ## Positive Cases are 1/33th of negatives, so, data set is unbalanced
## So, we will use different kind of sampling methologies, and compare the performance of each

### Splitting demoData Data into train and test, further ###
set.seed(123)
split_indices <- sample.split(demoData$PerformanceTag.1, SplitRatio = 0.7)
trainModel <- demoData[split_indices,]
testModel <- demoData[!split_indices,]
nrow(trainModel) / nrow(demoData)
nrow(testModel) / nrow(demoData)

### Random Forests, can be used, in here, but takes lot of time, so, we will use Gradient Boosting,
### which produces same results, but faster

#install.packages("unbalanced")
library(unbalanced)
library(gbm)

library(tcltk)  ## For the GUI based progress bar


trainModel <- as.data.frame(trainModel)
trainModel$PerformanceTag.1 <- as.factor(trainModel$PerformanceTag.1)
set.seed(132)
dataSMOTE <-
  ubBalance(
    X = trainModel[, -1],
    Y = trainModel[, 1],
    type = "ubSMOTE",
    percOver = 900,
    percUnder = 600,
    verbose = TRUE,
    positive = "1"
  )
trainBoostData <- cbind(dataSMOTE$X, dataSMOTE$Y)
n <- ncol(trainBoostData)
colnames(trainBoostData)[n] <- "PerformanceTag.1"
summary(trainBoostData$PerformanceTag.1)
table(trainBoostData$PerformanceTag.1)

### Building Boosting tree Model ###
fitcontrol <-
  trainControl(
    method = "repeatedcv",
    number = 10,
    repeats = 1,
    verbose = TRUE
  )

# pb <- tkProgressBar(title = "progress bar", min = 0, max = 10, width = 300)
# 
# for(i in 1:total){
#   Sys.sleep(0.1)
#   setTkProgressBar(pb, i, label=paste( round(i/10*100, 0),
#                                        "% done"))
# }
# close(pb)

gbmfit <-
  caret::train(PerformanceTag.1 ~ .,
               data = trainBoostData,
               method = "gbm",
               verbose = FALSE)


### predicted probabilities of Churn 1 for test data

test_pred = predict(gbmfit, type = "prob",
                    newdata = testModel[, -1])[, -1]
# Let's see the summary
summary(test_pred)

# Let's use the probability cutoff of 5%.
test_pred <- factor(ifelse(test_pred >= 0.05, "1", "0"))
test_actual <- factor(testModel$PerformanceTag.1)
testModel$PerformanceTag.1 <- test_actual
test_actual <- factor(test_actual)
testModel$PerformanceTag.1 <- factor(testModel$PerformanceTag.1)

test_conf <- confusionMatrix(test_pred, test_actual, positive = "1")
test_conf

# Finding out the optimal probalility cutoff

perform_fn <- function(cutoff)
{
  predicted <- factor(ifelse(test_pred >= cutoff, "1", "0"))
  conf <- confusionMatrix(predicted, test_actual, positive = "1")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc)))
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

# Summary of test probability
test_pred = predict(gbmfit, type = "prob",
                    newdata = testModel[, -1])[, -1]
summary(test_pred)
s = seq(.01, .14, length = 100)
OUT = matrix(0, 100, 3)
for (i in 1:100)
{
  OUT[i, ] = perform_fn(s[i])
}

plot(
  s,
  OUT[, 1],
  xlab = "Cutoff",
  ylab = "Value",
  cex.lab = 1.5,
  cex.axis = 1.5,
  ylim = c(0, 1),
  type = "l",
  lwd = 2,
  axes = FALSE,
  col = 2
)
axis(1, seq(0, 1, length = 5), seq(0, 1, length = 5), cex.lab = 1.5)
axis(2, seq(0, 1, length = 5), seq(0, 1, length = 5), cex.lab = 1.5)
lines(s, OUT[, 2], col = "darkgreen", lwd = 2)
lines(s, OUT[, 3], col = 4, lwd = 2)
box()
legend(
  0,
  .50,
  col = c(2, "darkgreen", 4, "darkred"),
  lwd = c(2, 2, 2, 2),
  c("Sensitivity", "Specificity", "Accuracy")
)


cutoff <- s[which.min(abs(OUT[, 1] - OUT[, 2]))]
cutoff ## 0.0559596

# Let's choose a cutoff value of 0.0559596 for final model
test_pred = predict(gbmfit, type = "prob",
                    newdata = testModel[, -1])[, -1]
test_cutoff <- factor(ifelse(test_pred > cutoff, "1", "0"))

conf_final <-
  confusionMatrix(test_cutoff, test_actual, positive = "1")
acc <- conf_final$overall[1]
sens <- conf_final$byClass[1]
spec <- conf_final$byClass[2]

acc
# Accuracy
# 0.5508229
sens
# Sensitivity
# 0.5395928
spec
# Specificity
# 0.5513071

################## Model Building for the final Data (using xgboost) #################
### Initial Quality Checks ###
sum(is.na(finalData)) ## 0 nulls
sum(sapply(finalData, function(x)
  length(which(x == "")))) # checking for blank "" values; there are none, now

finalData %>% group_by(PerformanceTag.1) %>% summarise(n = n()) ## Positive Cases are 1/33th of negatives, so, data set is unbalanced
## So, we will use different kind of sampling methologies, and compare the performance of each

### Splitting finalData Data into train and test, further ###
set.seed(123)
split_indices <- sample.split(finalData$PerformanceTag.1, SplitRatio = 0.7)
trainModel <- finalData[split_indices,]
testModel <- finalData[!split_indices,]
nrow(trainModel) / nrow(finalData)
nrow(testModel) / nrow(finalData)

### Random Forests, can be used, in here, but takes lot of time, so, we will use Gradient Boosting,
### which produces same results, but faster

trainModel <- as.data.frame(trainModel)
trainModel$PerformanceTag.1 <- as.factor(trainModel$PerformanceTag.1)
set.seed(132)
dataSMOTE <-
  ubBalance(
    X = trainModel[, -1],
    Y = trainModel[, 1],
    type = "ubSMOTE",
    percOver = 900,
    percUnder = 600,
    verbose = TRUE,
    positive = "1"
  )
trainBoostData <- cbind(dataSMOTE$X, dataSMOTE$Y)
n <- ncol(trainBoostData)
colnames(trainBoostData)[n] <- "PerformanceTag.1"
summary(trainBoostData$PerformanceTag.1)
table(trainBoostData$PerformanceTag.1)

### Building Boosting tree Model ###
fitcontrol <-
  trainControl(
    method = "repeatedcv",
    number = 10,
    repeats = 1,
    verbose = TRUE
  )
gbmfit <-
  caret::train(PerformanceTag.1 ~ .,
               data = trainBoostData,
               method = "gbm",
               verbose = FALSE)


### predicted probabilities for test data

test_pred = predict(gbmfit, type = "prob",
                    newdata = testModel[, -1])[, -1]
# Let's see the summary
summary(test_pred)

# Let's use the probability cutoff of 5%.
test_pred <- factor(ifelse(test_pred >= 0.05, "1", "0"))
test_actual <- factor(testModel$PerformanceTag.1)
testModel$PerformanceTag.1 <- test_actual
test_actual <- factor(test_actual)
testModel$PerformanceTag.1 <- factor(testModel$PerformanceTag.1)

test_conf <- confusionMatrix(test_pred, test_actual, positive = "1")
test_conf

# Finding out the optimal probalility cutoff

perform_fn <- function(cutoff)
{
  predicted <- factor(ifelse(test_pred >= cutoff, "1", "0"))
  conf <- confusionMatrix(predicted, test_actual, positive = "1")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc)))
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

# Summary of test probability
test_pred = predict(gbmfit, type = "prob",
                    newdata = testModel[, -1])[, -1]
summary(test_pred)
s = seq(.01, .14, length = 100)
OUT = matrix(0, 100, 3)
for (i in 1:100)
{
  OUT[i, ] = perform_fn(s[i])
}

plot(
  s,
  OUT[, 1],
  xlab = "Cutoff",
  ylab = "Value",
  cex.lab = 1.5,
  cex.axis = 1.5,
  ylim = c(0, 1),
  type = "l",
  lwd = 2,
  axes = TRUE,
  axes = FALSE,

  col = 2
)
axis(1, seq(0, 1, length = 5), seq(0, 1, length = 5), cex.lab = 1.5)
axis(2, seq(0, 1, length = 5), seq(0, 1, length = 5), cex.lab = 1.5)
lines(s, OUT[, 2], col = "darkgreen", lwd = 2)
lines(s, OUT[, 3], col = 4, lwd = 2)
box()
legend(
  0,
  .50,
  col = c(2, "darkgreen", 4, "darkred"),
  lwd = c(2, 2, 2, 2),
  c("Sensitivity", "Specificity", "Accuracy")
)


cutoff <- s[which.min(abs(OUT[, 1] - OUT[, 2]))]
cutoff ## 0.04676768

# Let's choose a cutoff value of 0.04676768 for final model
test_pred = predict(gbmfit, type = "prob",
                    newdata = testModel[, -1])[, -1]
test_cutoff <- factor(ifelse(test_pred > cutoff, "1", "0"))

conf_final <-
  confusionMatrix(test_cutoff, test_actual, positive = "1")
acc <- conf_final$overall[1]
sens <- conf_final$byClass[1]
spec <- conf_final$byClass[2]

acc
# Accuracy
# 0.6054797
sens
# Sensitivity
# 0.5780543
spec
# Specificity
# 0.6066621


#### Important Variables ####
"currResidenceTenure
No of times 90DPDorworse in last 12months
Avgas CCUtilization in last 12 months 
No of trades opened in last 12months
`currResidenceTenure_bucket_Quartile.(10,61]`
`currResidenceTenure_bucket_Quartile.(61,123]`
No_of_times_90_DPD_or_worse_in_last_6_months
No_of_times_30_DPD_or_worse_in_last_6_months
"
############################

# predicting the results in test dataset
test$test_predict = predict(model_17, 
                            newdata = test[,-1])

# Let's see the summary 
summary(test$test_predict)

# Let's use the probability cutoff of 5%.

test_pred <- factor(ifelse(test$test_predict >= 0.05, "1", "0"))
test_actual <- factor(test$PerformanceTag)

# Matrix
table(test_pred,test_actual)

library(e1071)
test_conf <- confusionMatrix(test_pred, test_actual, positive = "1")
test_conf

## Finding the best threshold value
perform_fn <- function(cutoff) 
{
  predicted <- factor(ifelse(test$test_predict >= cutoff, "1", "0"))
  conf <- confusionMatrix(predicted, test_actual, positive = "1")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

# Summary of test probability
summary(test$test_predict)
s = seq(.01,.5,length=100)
OUT = matrix(0,100,3)
for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 

plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"lightgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))


cutoff <- s[which.min(abs(OUT[, 1] - OUT[, 2]))]
cutoff 
# Let's choose a cutoff value of 0.05 for final model

test_cutoff <- factor(ifelse(test$test_predict >= 0.05, "1", "0"))

conf_final <- confusionMatrix(test_cutoff, test_actual, positive = "1")
acc <- conf_final$overall[1]
sens <- conf_final$byClass[1]
spec <- conf_final$byClass[2]

acc
# Accuracy 
# 0.6596222
sens
# Sensitivity 
# 0.6255656 
spec
# Specificity 
# 0.6610905 

##### Model Building(Logistic regression) using woe values and Building a score Card ######
library(data.table)
library(scorecard)

## woe binning of the Demographic Data ##
dt <- setDT(demoData_woe[,-1])
dt_s = var_filter(dt, y="PerformanceTag")
bins = woebin(dt, y="PerformanceTag", print_step = 1)
dt_list = split_df(dt_s, y="PerformanceTag", ratio = 0.7, seed = 30)
train = dt_list$train; test = dt_list$test;
train_woe = woebin_ply(train, bins, print_step=0)
test_woe = woebin_ply(test, bins, print_step=0)
train_woe$PerformanceTag <- as.numeric(train_woe$PerformanceTag)
test_woe$PerformanceTag <- as.numeric(test_woe$PerformanceTag)
m1Demo = glm(PerformanceTag ~ ., data = train_woe)
m1stepDemo <- stepAIC(m1Demo, direction="both")
summary(m1stepDemo)
vif(m1stepDemo)

### Performance Metrics ####
# predicting the results in test dataset
test$test_predict = predict(m1stepDemo, 
                            newdata = test_woe[,-1])

# Let's see the summary 
summary(test$test_predict)

## Taking the cutoff of 0.04464646
test_cutoff <- factor(ifelse(test$test_predict >=0.04464646, "1", "0"))

conf_final <- confusionMatrix(test_cutoff, test_actual, positive = "1")
acc <- conf_final$overall[1]
sens <- conf_final$byClass[1]
spec <- conf_final$byClass[2]

acc
# Accuracy 
# 0.6023004
sens
# Sensitivity 
# 0.5622172 
spec
# Specificity 
# 0.6040285 


## woe binning of the final Data set (Demographic + Credit) ##
final_woe <- cbind(creditBureauData_woe[,-c(1,19)], demoData_woe[,-1])
dt = setDT(final_woe)
dt_s = var_filter(dt, y="PerformanceTag")
bins = woebin(dt, y="PerformanceTag", print_step = 1)

dt_list = split_df(dt_s, y="PerformanceTag", ratio = 0.7, seed = 30)
train = dt_list$train; test = dt_list$test;
train_woe = woebin_ply(train, bins, print_step=0)
test_woe = woebin_ply(test, bins, print_step=0)
train_woe$PerformanceTag <- as.numeric(train_woe$PerformanceTag)
test_woe$PerformanceTag <- as.numeric(test_woe$PerformanceTag)

m1 = glm(PerformanceTag ~ ., family = "binomial", data = train_woe)
m1step <- stepAIC(m1, direction="both")
summary(m1step)
vif(m1step)
### Performance Metrics ####
# predicting the results in test dataset
test$test_predict = predict(m1step, 
                            newdata = test_woe[,-1], type = "response")

# Let's see the summary 
summary(test$test_predict)

## Taking the cutoff of 0.04464646
test_cutoff <- factor(ifelse(test$test_predict >= 0.054,"1", "0"))
test_actual <- factor(test_woe$PerformanceTag)

conf_final <- confusionMatrix(test_cutoff, test_actual, positive = "1")
acc <- conf_final$overall[1]
sens <- conf_final$byClass[1]
spec <- conf_final$byClass[2]

acc
# Accuracy 
# 0.6965588
sens
# Sensitivity 
# 0.5622172 
spec
# Specificity 
# 0.7023508 

### Score Card ###
m2 = eval(m1step$call)

# score
card = scorecard(bins, m2, points0 = 400, odds0 = 1/10, pdo = 20)
# credit score
scoreCard = scorecard_ply(dt_s, card, print_step=0)
scoreCard <- cbind(demoData_woe[,1], scoreCard)

final_woe_data = woebin_ply(final_woe, bins, print_step=0)
scoreCard$PerformanceTag <- predict(m1step, newdata = final_woe_data[,-1], type = "response")
#test_cutoff <- scoreCard$PerformanceTag
scoreCard$PerformanceTag <- factor(ifelse(as.numeric(scoreCard$PerformanceTag) >= 0.054,"1", "0"))
test_cutoff <- ifelse(test_cutoff>= 0.054,1,0)

View(scoreCard)
table(scoreCard$score, scoreCard$PerformanceTag)
write.csv(scoreCard, "scoreCard.csv", row.names = FALSE)
summary(scoreCard[scoreCard$PerformanceTag==1,]$score)
summary(scoreCard[scoreCard$PerformanceTag==0,]$score)


### Building a xgboost model using woe imputed datasets ###
library(unbalanced)
train_woe <- as.data.frame(train_woe)
train_woe$PerformanceTag <- as.factor(train_woe$PerformanceTag)
set.seed(132)
dataSMOTE <-
  ubBalance(
    X = train_woe[, -1],
    Y = train_woe[, 1],
    type = "ubSMOTE",
    percOver = 900,
    percUnder = 600,
    verbose = TRUE,
    positive = "1"
  )
trainBoostData <- cbind(dataSMOTE$X, dataSMOTE$Y)
n <- ncol(trainBoostData)
colnames(trainBoostData)[n] <- "PerformanceTag"
summary(trainBoostData$PerformanceTag)
table(trainBoostData$PerformanceTag)

### Building Boosting tree Model ###
gbmfit <-
  caret::train(PerformanceTag ~ .,
               data = train_woe,
               method = "xgbTree",
               trControl = trainControl(
                 method = "repeatedcv",
                 number = 4,
                 repeats = 4,
                 verbose = FALSE
               ),
               verbose = FALSE)

### predicted probabilities for test data

test_pred = predict(gbmfit, type = "prob",
                    newdata = test_woe[, -1])[,2]
# Let's see the summary
summary(test_pred)

test_cutoff <- factor(ifelse(test_pred > 0.052, "1", "0"))
test_actual <- factor(test_woe$PerformanceTag)
conf_final <-
  confusionMatrix(test_cutoff, test_actual, positive = "1")
acc <- conf_final$overall[1]
sens <- conf_final$byClass[1]
spec <- conf_final$byClass[2]

acc
# Accuracy
# 0.6721994
sens
# Sensitivity
# 0.5995475
spec
# Specificity
# 0.6753316

#### Since, the use of xgboost hasn`t provided us any significant advantage over logistic regression,
#### Hence, we have choosen Logistic Regression based model as the final one

#test_actual <- ifelse(demoData$PerformanceTag == "1", 1, 0)
test$test_predict = predict(m1step, 
                            newdata = test_woe[,-1], type = "response")

# Let's see the summary 
summary(test$test_predict)

## Taking the cutoff of 0.054
test_cutoff <- factor(ifelse(test$test_predict >= 0.054,"1", "0"))
test_actual <- factor(test_woe$PerformanceTag)
test_cutoff <- ifelse(test_cutoff == "1", 1, 0)
test_actual <- ifelse(test_actual == "1", 1, 0)


# on testing  data
pred_object_test <- prediction(test_cutoff, test_actual)
performance_measures_test <-
  ROCR::performance(pred_object_test, "tpr", "fpr")
ks_table_test <- attr(performance_measures_test, "y.values")[[1]] -
  (attr(performance_measures_test, "x.values")[[1]])
plot(performance_measures_test,
     main = "ROC curve",
     colorize = T)

max(ks_table_test) # 0.264568

# Lift & Gain Chart

lift <- function(labels , predicted_prob, groups = 10) {
  if (is.factor(labels))
    labels  <- as.integer(as.character(labels))
  if (is.factor(predicted_prob))
    predicted_prob <- as.integer(as.character(predicted_prob))
  helper = data.frame(cbind(labels , predicted_prob))
  helper[, "bucket"] = ntile(-helper[, "predicted_prob"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(labels), funs(total = n(),
                                    totalresp = sum(., na.rm = TRUE))) %>%
    
    mutate(
      Cumresp = cumsum(totalresp),
      Gain = Cumresp / sum(totalresp) * 100,
      Cumlift = Gain / (bucket * (100 / groups))
    )
  return(gaintable)
}

Performance_decile = lift(test_actual, test_pred, groups = 10)
Performance_decile

Gain <- c(0, Performance_decile$Gain)
Deciles <- c(0, Performance_decile$bucket)
plot(
  y = Gain,
  x = Deciles,
  type = "l",
  lwd = 2,
  xlab = "Bucket",
  ylab = "Gain",
  main = "Gain Chart"
)

Random_Gain <- seq(from = 0, to = 100, by = 10)
lines(
  y = Random_Gain,
  x = Deciles,
  type = "l",
  lwd = 2,
  col = "red"
)

Perfect_Gain <- vector(mode = "numeric", length = 11)
for (i in 2:11) {
  Perfect_Gain[i] <- 100 * min(1, 129 * (i - 1) / 209)
}
lines(
  y = Perfect_Gain,
  x = Deciles,
  type = "l",
  lwd = 2,
  col = "darkgreen"
)

legend(
  "bottomright",
  col = c("darkgreen", "black", "red"),
  lwd = c(2, 2, 2, 2),
  c("Perfect Model", "Actual Model", "Random Model"),
  cex = 0.7
)

# plotting the lift chart
Lift <- Gain / Random_Gain
Random_Lift <- Random_Gain / Random_Gain

plot(
  y = Lift,
  x = Deciles,
  type = "l",
  ylim = c(0, 3.5),
  lwd = 2,
  xlab = "Bucket",
  ylab = "Lift",
  main = "Lift Chart",
  ylim <- c()
)
lines(
  y = Random_Lift,
  x = Deciles,
  type = "l",
  lwd = 2,
  col = "red"
)

legend(
  "topright",
  col = c("black", "red"),
  lwd = c(2, 2, 2),
  c("Actual Model", "Random Model"),
  cex = 0.7
)

### Financial Benefit Assessment ####
## The following assumptions have been made while assessing the financial model ##
" The cost of acquisition for every customer has been considered same
  The expected business/revenue/profit for every customer has been considered same
  The figures (acquisition cost/reveue per person) are taken for representational purpose. Actual
  values may vary.
"

## Following Observations have been made wrt to the model ##
" We can able to capture ~70% targeted customers by 5th decile
  Assuming that acquisition cost for each customer is ₹30 and revenue per customer is ₹1000.
  Here are the stats:
  
    Without Model:
  Upto 5th decile, cost of acquisition of Customers = 2139*5*30 = ₹320850
  With this, 50% targeted group were approved. Hence, total revenue = 884*0.5*1000 = ₹442000
  Total Profit = ₹121150

    With the model:
  70% of targeted group were approved. Hence, total revenue = 884*0.7*1000 = ₹618800
  Total Profit = ₹297950
  
  Hence, improvement in Revenue = ((297950-121150)/121150)*100 = 146%
  
"