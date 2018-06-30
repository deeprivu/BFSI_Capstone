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
There are 1425 istances, where Performance Tag is not available, so, shall we remove them.
Missing Value Imputation Strategy (While not using woe values)

Correlation Matrix and Plots need to be changed, its clumpsy now 
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
demoData <- demoData[is.na(demoData$PerformanceTag)==F,]
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
#### Imputing with woe values ####

## Changing the data type of the variables, and small necessary amendments in the dataset
str(woe_data)
woe_data <- woe_data[,-1] ## Application_ID will not be required
woe_data$Gender <- as.factor(woe_data$Gender)
woe_data$MaritalStatus <- as.factor(woe_data$MaritalStatus)
woe_data$Education <- as.factor(woe_data$Education)
woe_data$Profession <- as.factor(woe_data$Profession)
woe_data$PerformanceTag <- as.factor(woe_data$PerformanceTag)
woe_data$TypeofResidence <- as.factor(woe_data$TypeofResidence)

woe_data$Age <- as.integer(woe_data$Age)
woe_data$Dependents <- as.integer(woe_data$Dependents)
woe_data$Income <- as.integer(woe_data$Income)
woe_data$currResidenceTenure <- as.integer(woe_data$currResidenceTenure)
woe_data$currJobTenure <- as.integer(woe_data$currJobTenure)

 woe_data <- as.data.frame(woe_data)
 row.names(woe_data) <- 1:nrow(woe_data)
 outiv <- iv.mult(woe_data,"PerformanceTag")
 woe_test <- iv.replace.woe(woe_data, outiv) ## Imputing with woe values

 iv.mult(woe_data,"PerformanceTag")
 colSums(is.na(woe_test))
 colSums(is.na(demoData))

### After imputation, we have 2 datasets: demoData <- Imputation with normal methodologies
### woe_data <- Imputation with woe_values


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
corrplot(cor_matrix, method = "pie", type = "lower", tl.cex = 0.9)

##########################3####3 Data Understanding (For Credit Data) ###################################3
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
## The predictor Variable PerformanceTag has nulls, removing the records (~2%)
creditBureauData <- creditBureauData[is.na(creditBureauData$Performance.Tag)==F,]
sum(sapply(creditBureauData, function(x)
  length(which(x == "")))) # checking for blank "" values; there are none
sum(duplicated(creditBureauData$Application.ID)) ## 3 duplicates
creditBureauData <- creditBureauData[is.na(creditBureauData$Performance.Tag)==F,] ## Removing the recprds where target Variable is absent

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
corrplot(cor_matrix, method = "pie", type = "lower", tl.cex = 0.9)
## From the plot, we can see that, there exists a strong correlation amongst the variables

############################# Preparing for the final Data ##########################
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
corrplot(cor_matrix, method = "pie", type = "lower", tl.cex = 0.9) ## Correlation in the Final Data Set


############################### Model Building Phase for Demo Data (using Logistic Regression) ########################
# separate training and testing data
set.seed(100)
trainindices= sample(1:nrow(demoData), 0.7*nrow(demoData))
train = demoData[trainindices,]
test = demoData[-trainindices,]

## Logistic Regression ##
## Initial Model using all the variables of Demo_Data ##
model_1 <-glm(PerformanceTag.1~.,data=train)
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
               data = train)
summary(model_2)
vif(model_2)

model_3 <- glm(formula = PerformanceTag.1 ~ Income + currResidenceTenure + 
                 currJobTenure + iscurrResidenceTenurelessthan8 + iscurrJobTenurelessthan4 + 
                 Profession.SE + `Age_bucket_Quartile.(37,45]` + `Income_bucket_Quartile.(40,61]` + 
                 `currResidenceTenure_bucket_Quartile.(10,61]` + `currJobTenure_bucket_Quartile.(17,34]` + `currJobTenure_bucket_Response.(60,74]`, 
               data = train)
summary(model_3)
vif(model_3)

model_4 <- glm(formula = PerformanceTag.1 ~ Income + currResidenceTenure + 
                 currJobTenure + iscurrResidenceTenurelessthan8 + iscurrJobTenurelessthan4 + 
                 Profession.SE + `Age_bucket_Quartile.(37,45]` +  
                 `currResidenceTenure_bucket_Quartile.(10,61]` + `currJobTenure_bucket_Quartile.(17,34]` + `currJobTenure_bucket_Response.(60,74]`, 
               data = train)
summary(model_4)
vif(model_4)

model_5 <- glm(formula = PerformanceTag.1 ~ Income + currResidenceTenure + 
                 currJobTenure + iscurrResidenceTenurelessthan8 + iscurrJobTenurelessthan4 + 
                 Profession.SE + `Age_bucket_Quartile.(37,45]` + `currJobTenure_bucket_Quartile.(17,34]` + `currJobTenure_bucket_Response.(60,74]`, 
               data = train)
summary(model_5)
vif(model_5)

model_6 <- glm(formula = PerformanceTag.1 ~ Income + currResidenceTenure + 
                 currJobTenure + iscurrResidenceTenurelessthan8 + iscurrJobTenurelessthan4 + 
                 Profession.SE + `currJobTenure_bucket_Quartile.(17,34]` + `currJobTenure_bucket_Response.(60,74]`, 
               data = train)
summary(model_6)

model_7 <- glm(formula = PerformanceTag.1 ~ Income + currResidenceTenure + 
                 currJobTenure + iscurrResidenceTenurelessthan8 + iscurrJobTenurelessthan4 + 
                 `currJobTenure_bucket_Quartile.(17,34]` + `currJobTenure_bucket_Response.(60,74]`, 
               data = train)
summary(model_7)

model_8 <- glm(formula = PerformanceTag.1 ~ Income + currResidenceTenure + 
                 currJobTenure + iscurrResidenceTenurelessthan8 + iscurrJobTenurelessthan4 + 
                 `currJobTenure_bucket_Response.(60,74]`, 
               data = train)
summary(model_8)

model_9 <- glm(formula = PerformanceTag.1 ~ Income + currResidenceTenure + 
                 currJobTenure + iscurrResidenceTenurelessthan8 + `currJobTenure_bucket_Response.(60,74]`, 
               data = train)
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

test_cutoff <- factor(ifelse(test$test_predict >=0.04464646, "1", "0"))

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
model_1 <-glm(PerformanceTag.1~.,data=train)
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
               data = train)
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
               data = train)
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
               data = train)
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
               data = train)
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
               data = train)
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
               data = train)
summary(model_7)

model_8 <- glm(formula = PerformanceTag.1 ~ `Age_bucket_Quartile.(45,53]` + `Age_bucket_Quartile.(53,66]` + 
                 `currResidenceTenure_bucket_Quartile.(10,61]` + `currResidenceTenure_bucket_Quartile.(61,123]` + 
                 `currJobTenure_bucket_Quartile.(17,34]` + `currJobTenure_bucket_Quartile.(34,51]` + 
                 `currJobTenure_bucket_Quartile.(51,74]` + `Age_bucket_Response.(36,56]` + 
                 No_of_times_90_DPD_or_worse_in_last_6_months + 
                 No_of_times_30_DPD_or_worse_in_last_6_months + No_of_times_90_DPD_or_worse_in_last_12_months + 
                 Avgas_CC_Utilization_in_last_12_months + No_of_PL_trades_opened_in_last_12_months + No_of_Inquiries_in_last_12_months__excluding_home___auto_loans_ + 
                 Total_No_of_Trades + `Outstanding_Balance_Quartile.(2.09e+05,7.74e+05]`, 
               data = train)
summary(model_8)

model_9 <- glm(formula = PerformanceTag.1 ~ `Age_bucket_Quartile.(53,66]` + 
                 `currResidenceTenure_bucket_Quartile.(10,61]` + `currResidenceTenure_bucket_Quartile.(61,123]` + 
                 `currJobTenure_bucket_Quartile.(17,34]` + `currJobTenure_bucket_Quartile.(34,51]` + 
                 `currJobTenure_bucket_Quartile.(51,74]` + `Age_bucket_Response.(36,56]` + 
                 No_of_times_90_DPD_or_worse_in_last_6_months + 
                 No_of_times_30_DPD_or_worse_in_last_6_months + No_of_times_90_DPD_or_worse_in_last_12_months + 
                 Avgas_CC_Utilization_in_last_12_months + No_of_PL_trades_opened_in_last_12_months + No_of_Inquiries_in_last_12_months__excluding_home___auto_loans_ + 
                 Total_No_of_Trades + `Outstanding_Balance_Quartile.(2.09e+05,7.74e+05]`, 
               data = train)
summary(model_9)

model_10 <- glm(formula = PerformanceTag.1 ~ `currResidenceTenure_bucket_Quartile.(10,61]` + `currResidenceTenure_bucket_Quartile.(61,123]` + 
                 `currJobTenure_bucket_Quartile.(17,34]` + `currJobTenure_bucket_Quartile.(34,51]` + 
                 `currJobTenure_bucket_Quartile.(51,74]` + `Age_bucket_Response.(36,56]` + 
                 No_of_times_90_DPD_or_worse_in_last_6_months + 
                 No_of_times_30_DPD_or_worse_in_last_6_months + No_of_times_90_DPD_or_worse_in_last_12_months + 
                 Avgas_CC_Utilization_in_last_12_months + No_of_PL_trades_opened_in_last_12_months + No_of_Inquiries_in_last_12_months__excluding_home___auto_loans_ + 
                 Total_No_of_Trades + `Outstanding_Balance_Quartile.(2.09e+05,7.74e+05]`, 
               data = train)
summary(model_10)

model_11 <- glm(formula = PerformanceTag.1 ~ `currResidenceTenure_bucket_Quartile.(61,123]` + 
                  `currJobTenure_bucket_Quartile.(17,34]` + `currJobTenure_bucket_Quartile.(34,51]` + 
                  `currJobTenure_bucket_Quartile.(51,74]` + `Age_bucket_Response.(36,56]` + 
                  No_of_times_90_DPD_or_worse_in_last_6_months + 
                  No_of_times_30_DPD_or_worse_in_last_6_months + No_of_times_90_DPD_or_worse_in_last_12_months + 
                  Avgas_CC_Utilization_in_last_12_months + No_of_PL_trades_opened_in_last_12_months + No_of_Inquiries_in_last_12_months__excluding_home___auto_loans_ + 
                  Total_No_of_Trades + `Outstanding_Balance_Quartile.(2.09e+05,7.74e+05]`, 
                data = train)
summary(model_11)

model_12 <- glm(formula = PerformanceTag.1 ~ `currResidenceTenure_bucket_Quartile.(61,123]` + 
                  `currJobTenure_bucket_Quartile.(17,34]` + `currJobTenure_bucket_Quartile.(34,51]` + 
                  `currJobTenure_bucket_Quartile.(51,74]` + No_of_times_90_DPD_or_worse_in_last_6_months + 
                  No_of_times_30_DPD_or_worse_in_last_6_months + No_of_times_90_DPD_or_worse_in_last_12_months + 
                  Avgas_CC_Utilization_in_last_12_months + No_of_PL_trades_opened_in_last_12_months + No_of_Inquiries_in_last_12_months__excluding_home___auto_loans_ + 
                  Total_No_of_Trades + `Outstanding_Balance_Quartile.(2.09e+05,7.74e+05]`, 
                data = train)
summary(model_12)
vif(model_12)

model_13 <- glm(formula = PerformanceTag.1 ~ `currResidenceTenure_bucket_Quartile.(61,123]` + 
                  `currJobTenure_bucket_Quartile.(17,34]` + `currJobTenure_bucket_Quartile.(51,74]` + No_of_times_90_DPD_or_worse_in_last_6_months + 
                  No_of_times_30_DPD_or_worse_in_last_6_months + No_of_times_90_DPD_or_worse_in_last_12_months + 
                  Avgas_CC_Utilization_in_last_12_months + No_of_PL_trades_opened_in_last_12_months + No_of_Inquiries_in_last_12_months__excluding_home___auto_loans_ + 
                  Total_No_of_Trades + `Outstanding_Balance_Quartile.(2.09e+05,7.74e+05]`, 
                data = train)
summary(model_13)

model_14 <- glm(formula = PerformanceTag.1 ~ `currResidenceTenure_bucket_Quartile.(61,123]` + 
                  `currJobTenure_bucket_Quartile.(17,34]` + No_of_times_90_DPD_or_worse_in_last_6_months + 
                  No_of_times_30_DPD_or_worse_in_last_6_months + No_of_times_90_DPD_or_worse_in_last_12_months + 
                  Avgas_CC_Utilization_in_last_12_months + No_of_PL_trades_opened_in_last_12_months + No_of_Inquiries_in_last_12_months__excluding_home___auto_loans_ + 
                  Total_No_of_Trades + `Outstanding_Balance_Quartile.(2.09e+05,7.74e+05]`, 
                data = train)
summary(model_14)
vif(model_14)

model_15 <- glm(formula = PerformanceTag.1 ~ `currResidenceTenure_bucket_Quartile.(61,123]` + 
                  `currJobTenure_bucket_Quartile.(17,34]` + No_of_times_90_DPD_or_worse_in_last_6_months + 
                  No_of_times_30_DPD_or_worse_in_last_6_months + No_of_times_90_DPD_or_worse_in_last_12_months + 
                  Avgas_CC_Utilization_in_last_12_months + No_of_PL_trades_opened_in_last_12_months + 
                  Total_No_of_Trades + `Outstanding_Balance_Quartile.(2.09e+05,7.74e+05]`, 
                data = train)
summary(model_15)

model_16 <- glm(formula = PerformanceTag.1 ~ `currJobTenure_bucket_Quartile.(17,34]` + No_of_times_90_DPD_or_worse_in_last_6_months + 
                  No_of_times_30_DPD_or_worse_in_last_6_months + No_of_times_90_DPD_or_worse_in_last_12_months + 
                  Avgas_CC_Utilization_in_last_12_months + No_of_PL_trades_opened_in_last_12_months + 
                  Total_No_of_Trades + `Outstanding_Balance_Quartile.(2.09e+05,7.74e+05]`, 
                data = train)
summary(model_16)

model_17 <- glm(formula = PerformanceTag.1 ~ No_of_times_90_DPD_or_worse_in_last_6_months + 
                  No_of_times_30_DPD_or_worse_in_last_6_months + No_of_times_90_DPD_or_worse_in_last_12_months + 
                  Avgas_CC_Utilization_in_last_12_months + No_of_PL_trades_opened_in_last_12_months + 
                  Total_No_of_Trades + `Outstanding_Balance_Quartile.(2.09e+05,7.74e+05]`, 
                data = train)
summary(model_17)
vif(model_17)

model_18 <- glm(formula = PerformanceTag.1 ~ No_of_times_90_DPD_or_worse_in_last_6_months + 
                  No_of_times_30_DPD_or_worse_in_last_6_months + No_of_times_90_DPD_or_worse_in_last_12_months + 
                  Avgas_CC_Utilization_in_last_12_months + No_of_PL_trades_opened_in_last_12_months + `Outstanding_Balance_Quartile.(2.09e+05,7.74e+05]`, 
                data = train)
summary(model_18)
vif(model_18)

model_19 <- glm(formula = PerformanceTag.1 ~ No_of_times_30_DPD_or_worse_in_last_6_months + No_of_times_90_DPD_or_worse_in_last_12_months + 
                  Avgas_CC_Utilization_in_last_12_months + No_of_PL_trades_opened_in_last_12_months + `Outstanding_Balance_Quartile.(2.09e+05,7.74e+05]`, 
                data = train)
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

pb <- tkProgressBar(title = "progress bar", min = 0, max = 10, width = 300)

for(i in 1:total){
  Sys.sleep(0.1)
  setTkProgressBar(pb, i, label=paste( round(i/10*100, 0),
                                       "% done"))
}
close(pb)

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
cutoff ## 0.05727273

# Let's choose a cutoff value of 0.05727273 for final model
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
# 0.5678641
sens
# Sensitivity
# 0.5373303
spec
# Specificity
# 0.5692085

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
# 0.6121368
sens
# Sensitivity
# 0.60181
spec
# Specificity
# 0.6125915

######################## Modelling using woe imputed Data (Final Data)  ###################
### Preparing woe final dataset
nrow(demoData_woe); nrow(creditBureauData_woe)
final_woe <- cbind(demoData_woe, creditBureauData_woe)
final_woe <- final_woe[,-c("ApplicationID","Application.ID","PerformanceTag")]
colnames(final_woe) <- gsub("\\.","",names(final_woe))
final_woe <- as.data.frame(final_woe)
str(final_woe)
final_woe$Gender <- as.factor(final_woe$Gender)
final_woe$MaritalStatus <- as.factor(final_woe$MaritalStatus)
final_woe$Dependents <- as.numeric(final_woe$Dependents)
final_woe$Education <- as.factor(final_woe$Education)
final_woe$Profession <- as.factor(final_woe$Profession)
final_woe$TypeofResidence <- as.factor(final_woe$TypeofResidence)


outiv <- iv.mult(final_woe,"PerformanceTag")

## Relacing the woe final dataset with respective woe values
final_woe <- final_woe[,!(names(final_woe) %in% c("Age","Gender","MaritalStatus","Dependents","Education","Profession",
                                                  "TypeofResidence","currJobTenure","Presenceofopenhomeloan","Presenceofopenautoloan"))]
outiv <- iv.mult(final_woe,"PerformanceTag")
woe_test <- iv.replace.woe(final_woe,iv=outiv)
final_woe <- woe_test[,18:ncol(woe_test)]
#final_woe$PerformanceTag <- as.numeric(final_woe$PerformanceTag)
final_woe$PerformanceTag <- as.numeric(ifelse(final_woe$PerformanceTag==1,1,0))
final_woeForest <- final_woe
final_woe <- cbind(final_woe,finalData)
final_woe <- final_woe[,-19]

### Model Building for final_woe(using Logistic Regression) ###
set.seed(100)
trainindices= sample(1:nrow(final_woe), 0.7*nrow(final_woe))
train = final_woe[trainindices,]
test = final_woe[-trainindices,]

## Logistic Regression ##
## Initial Model using all the variables of Demo_Data ##
model_1 <-glm(PerformanceTag~.,data=train)
summary(model_1)
vif(model_1)

## Running stepAIC
step <- stepAIC(model_1, direction="both")
summary(step)
vif(step)

## Further Model Building Processess ###
model_2 <- glm(formula = PerformanceTag ~ Income_woe + currResidenceTenure_woe + 
                 Nooftimes90DPDorworseinlast12months_woe + AvgasCCUtilizationinlast12months_woe + 
                 Nooftradesopenedinlast12months_woe + NoofPLtradesopenedinlast6months_woe + 
                 NoofInquiriesinlast12monthsexcludinghomeautoloans_woe + OutstandingBalance_woe + 
                 Age + Gender.M + Profession.SE + `Age_bucket_Quartile.(37,45]` + 
                 `Age_bucket_Quartile.(45,53]` + `Age_bucket_Quartile.(53,66]` + 
                 `Income_bucket_Quartile.(27,40]` + `Income_bucket_Quartile.(40,61]` + 
                 `currResidenceTenure_bucket_Quartile.(10,61]` + `currResidenceTenure_bucket_Quartile.(61,123]` + 
                 `currJobTenure_bucket_Quartile.(17,34]` + `currJobTenure_bucket_Quartile.(34,51]` + 
                 `currJobTenure_bucket_Quartile.(51,74]` + `Age_bucket_Response.(36,56]` + 
                 No_of_times_90_DPD_or_worse_in_last_6_months + No_of_times_30_DPD_or_worse_in_last_6_months + No_of_times_90_DPD_or_worse_in_last_12_months + 
                 No_of_trades_opened_in_last_12_months + No_of_PL_trades_opened_in_last_6_months + 
                 No_of_PL_trades_opened_in_last_12_months, data = train)
summary(model_2)
vif(model_2)

model_3 <- glm(formula = PerformanceTag ~ Income_woe + currResidenceTenure_woe + 
                 Nooftimes90DPDorworseinlast12months_woe + AvgasCCUtilizationinlast12months_woe + 
                 Nooftradesopenedinlast12months_woe + NoofPLtradesopenedinlast6months_woe + 
                 NoofInquiriesinlast12monthsexcludinghomeautoloans_woe + OutstandingBalance_woe + Gender.M + Profession.SE + Age +
                 `Income_bucket_Quartile.(27,40]` + `Income_bucket_Quartile.(40,61]` + 
                 `currResidenceTenure_bucket_Quartile.(10,61]` + `currResidenceTenure_bucket_Quartile.(61,123]` + 
                 `currJobTenure_bucket_Quartile.(17,34]` + `currJobTenure_bucket_Quartile.(34,51]` + 
                 `currJobTenure_bucket_Quartile.(51,74]` +  
                 No_of_times_90_DPD_or_worse_in_last_6_months + No_of_times_30_DPD_or_worse_in_last_6_months + No_of_times_90_DPD_or_worse_in_last_12_months + 
                 No_of_trades_opened_in_last_12_months + No_of_PL_trades_opened_in_last_6_months + 
                 No_of_PL_trades_opened_in_last_12_months, data = train)
summary(model_3)
vif(model_3)

model_4 <- glm(formula = PerformanceTag ~ Income_woe + currResidenceTenure_woe + 
                 Nooftimes90DPDorworseinlast12months_woe + AvgasCCUtilizationinlast12months_woe + 
                 Nooftradesopenedinlast12months_woe + NoofPLtradesopenedinlast6months_woe + 
                 NoofInquiriesinlast12monthsexcludinghomeautoloans_woe + OutstandingBalance_woe + Gender.M + Profession.SE + Age +
                 `Income_bucket_Quartile.(27,40]` + `Income_bucket_Quartile.(40,61]` + 
                 `currResidenceTenure_bucket_Quartile.(10,61]` + `currResidenceTenure_bucket_Quartile.(61,123]` + 
                 `currJobTenure_bucket_Quartile.(17,34]` + `currJobTenure_bucket_Quartile.(34,51]` + 
                 `currJobTenure_bucket_Quartile.(51,74]` +  
                 No_of_times_90_DPD_or_worse_in_last_6_months + No_of_times_30_DPD_or_worse_in_last_6_months + No_of_times_90_DPD_or_worse_in_last_12_months + 
                 No_of_trades_opened_in_last_12_months + No_of_PL_trades_opened_in_last_6_months, data = train)
summary(model_4)
vif(model_4)

model_5 <- glm(formula = PerformanceTag ~ Income_woe + currResidenceTenure_woe + 
                 Nooftimes90DPDorworseinlast12months_woe + AvgasCCUtilizationinlast12months_woe + 
                 Nooftradesopenedinlast12months_woe + NoofPLtradesopenedinlast6months_woe + 
                 NoofInquiriesinlast12monthsexcludinghomeautoloans_woe + OutstandingBalance_woe + Gender.M + Profession.SE + Age +
                 `Income_bucket_Quartile.(27,40]` + `Income_bucket_Quartile.(40,61]` + 
                 `currResidenceTenure_bucket_Quartile.(10,61]` + `currResidenceTenure_bucket_Quartile.(61,123]` + 
                 `currJobTenure_bucket_Quartile.(17,34]` + `currJobTenure_bucket_Quartile.(34,51]` + 
                 `currJobTenure_bucket_Quartile.(51,74]` +  
                 No_of_times_90_DPD_or_worse_in_last_6_months + No_of_times_30_DPD_or_worse_in_last_6_months + No_of_times_90_DPD_or_worse_in_last_12_months + No_of_PL_trades_opened_in_last_6_months, data = train)
summary(model_5)
vif(model_5)

model_6 <- glm(formula = PerformanceTag ~ Income_woe + currResidenceTenure_woe + 
                 Nooftimes90DPDorworseinlast12months_woe + AvgasCCUtilizationinlast12months_woe + 
                 Nooftradesopenedinlast12months_woe + NoofPLtradesopenedinlast6months_woe + 
                 NoofInquiriesinlast12monthsexcludinghomeautoloans_woe + OutstandingBalance_woe + Profession.SE + Age +
                 `Income_bucket_Quartile.(27,40]` + `Income_bucket_Quartile.(40,61]` + 
                 `currResidenceTenure_bucket_Quartile.(10,61]` + `currResidenceTenure_bucket_Quartile.(61,123]` + 
                 `currJobTenure_bucket_Quartile.(17,34]` + `currJobTenure_bucket_Quartile.(34,51]` + 
                 `currJobTenure_bucket_Quartile.(51,74]` +  
                 No_of_times_90_DPD_or_worse_in_last_6_months + No_of_times_30_DPD_or_worse_in_last_6_months + No_of_times_90_DPD_or_worse_in_last_12_months + No_of_PL_trades_opened_in_last_6_months, data = train)
summary(model_6)
vif(model_6)

model_7 <- glm(formula = PerformanceTag ~ Income_woe + currResidenceTenure_woe + 
                 Nooftimes90DPDorworseinlast12months_woe + AvgasCCUtilizationinlast12months_woe + 
                 Nooftradesopenedinlast12months_woe + NoofPLtradesopenedinlast6months_woe + 
                 NoofInquiriesinlast12monthsexcludinghomeautoloans_woe + OutstandingBalance_woe + Profession.SE +
                 `Income_bucket_Quartile.(27,40]` + `Income_bucket_Quartile.(40,61]` + 
                 `currResidenceTenure_bucket_Quartile.(10,61]` + `currResidenceTenure_bucket_Quartile.(61,123]` + 
                 `currJobTenure_bucket_Quartile.(17,34]` + `currJobTenure_bucket_Quartile.(34,51]` + 
                 `currJobTenure_bucket_Quartile.(51,74]` +  
                 No_of_times_90_DPD_or_worse_in_last_6_months + No_of_times_30_DPD_or_worse_in_last_6_months + No_of_times_90_DPD_or_worse_in_last_12_months + No_of_PL_trades_opened_in_last_6_months, data = train)
summary(model_7)
vif(model_7)

model_8 <- glm(formula = PerformanceTag ~ Income_woe + currResidenceTenure_woe + 
                 Nooftimes90DPDorworseinlast12months_woe + AvgasCCUtilizationinlast12months_woe + 
                 Nooftradesopenedinlast12months_woe + NoofPLtradesopenedinlast6months_woe + 
                 NoofInquiriesinlast12monthsexcludinghomeautoloans_woe + OutstandingBalance_woe +
                 `Income_bucket_Quartile.(27,40]` + `Income_bucket_Quartile.(40,61]` + 
                 `currResidenceTenure_bucket_Quartile.(10,61]` + `currResidenceTenure_bucket_Quartile.(61,123]` + 
                 `currJobTenure_bucket_Quartile.(17,34]` + `currJobTenure_bucket_Quartile.(34,51]` + 
                 `currJobTenure_bucket_Quartile.(51,74]` +  
                 No_of_times_90_DPD_or_worse_in_last_6_months + No_of_times_30_DPD_or_worse_in_last_6_months + No_of_times_90_DPD_or_worse_in_last_12_months + No_of_PL_trades_opened_in_last_6_months, data = train)
summary(model_8)
vif(model_8)

model_9 <- glm(formula = PerformanceTag ~ Income_woe + currResidenceTenure_woe + 
                 Nooftimes90DPDorworseinlast12months_woe + AvgasCCUtilizationinlast12months_woe + 
                 Nooftradesopenedinlast12months_woe + NoofPLtradesopenedinlast6months_woe + 
                 NoofInquiriesinlast12monthsexcludinghomeautoloans_woe + OutstandingBalance_woe +
                 `Income_bucket_Quartile.(27,40]` + `Income_bucket_Quartile.(40,61]` + 
                 `currResidenceTenure_bucket_Quartile.(10,61]` + `currResidenceTenure_bucket_Quartile.(61,123]` + 
                 `currJobTenure_bucket_Quartile.(17,34]` + 
                 `currJobTenure_bucket_Quartile.(51,74]` +  
                 No_of_times_90_DPD_or_worse_in_last_6_months + No_of_times_30_DPD_or_worse_in_last_6_months + No_of_times_90_DPD_or_worse_in_last_12_months + No_of_PL_trades_opened_in_last_6_months, data = train)
summary(model_9)
vif(model_9)

model_10 <- glm(formula = PerformanceTag ~ Income_woe + currResidenceTenure_woe + 
                 Nooftimes90DPDorworseinlast12months_woe + AvgasCCUtilizationinlast12months_woe + 
                 Nooftradesopenedinlast12months_woe + NoofPLtradesopenedinlast6months_woe + OutstandingBalance_woe +
                 `Income_bucket_Quartile.(27,40]` + `Income_bucket_Quartile.(40,61]` + 
                 `currResidenceTenure_bucket_Quartile.(10,61]` + `currResidenceTenure_bucket_Quartile.(61,123]` + 
                 `currJobTenure_bucket_Quartile.(17,34]` + 
                 `currJobTenure_bucket_Quartile.(51,74]` +  
                 No_of_times_90_DPD_or_worse_in_last_6_months + No_of_times_30_DPD_or_worse_in_last_6_months + No_of_times_90_DPD_or_worse_in_last_12_months + No_of_PL_trades_opened_in_last_6_months, data = train)
summary(model_10)
vif(model_10)

model_11 <- glm(formula = PerformanceTag ~ Income_woe + currResidenceTenure_woe + 
                  Nooftimes90DPDorworseinlast12months_woe + AvgasCCUtilizationinlast12months_woe + 
                  Nooftradesopenedinlast12months_woe + NoofPLtradesopenedinlast6months_woe + OutstandingBalance_woe +
                  `Income_bucket_Quartile.(27,40]` + `Income_bucket_Quartile.(40,61]` + 
                  `currResidenceTenure_bucket_Quartile.(10,61]` + `currResidenceTenure_bucket_Quartile.(61,123]` + 
                  `currJobTenure_bucket_Quartile.(17,34]` + 
                  `currJobTenure_bucket_Quartile.(51,74]` +  
                  No_of_times_90_DPD_or_worse_in_last_6_months + No_of_times_30_DPD_or_worse_in_last_6_months + No_of_PL_trades_opened_in_last_6_months, data = train)
summary(model_11)
vif(model_11)

model_12 <- glm(formula = PerformanceTag ~ Income_woe + currResidenceTenure_woe + 
                  Nooftimes90DPDorworseinlast12months_woe + AvgasCCUtilizationinlast12months_woe + 
                  Nooftradesopenedinlast12months_woe + NoofPLtradesopenedinlast6months_woe + OutstandingBalance_woe +
                  `Income_bucket_Quartile.(27,40]` + `Income_bucket_Quartile.(40,61]` + 
                  `currResidenceTenure_bucket_Quartile.(10,61]` + `currResidenceTenure_bucket_Quartile.(61,123]` + 
                  `currJobTenure_bucket_Quartile.(17,34]` + No_of_times_90_DPD_or_worse_in_last_6_months + No_of_times_30_DPD_or_worse_in_last_6_months + No_of_PL_trades_opened_in_last_6_months, data = train)
summary(model_12)
vif(model_12)

model_13 <- glm(formula = PerformanceTag ~ Income_woe + currResidenceTenure_woe + 
                  Nooftimes90DPDorworseinlast12months_woe + AvgasCCUtilizationinlast12months_woe + 
                  Nooftradesopenedinlast12months_woe + OutstandingBalance_woe +
                  `Income_bucket_Quartile.(27,40]` + `Income_bucket_Quartile.(40,61]` + 
                  `currResidenceTenure_bucket_Quartile.(10,61]` + `currResidenceTenure_bucket_Quartile.(61,123]` + 
                  `currJobTenure_bucket_Quartile.(17,34]` + No_of_times_90_DPD_or_worse_in_last_6_months + No_of_times_30_DPD_or_worse_in_last_6_months + No_of_PL_trades_opened_in_last_6_months, data = train)
summary(model_13)
vif(model_13)

model_14 <- glm(formula = PerformanceTag ~ Income_woe + currResidenceTenure_woe + 
                  Nooftimes90DPDorworseinlast12months_woe + AvgasCCUtilizationinlast12months_woe + 
                  Nooftradesopenedinlast12months_woe + `Income_bucket_Quartile.(27,40]` + `Income_bucket_Quartile.(40,61]` + 
                  `currResidenceTenure_bucket_Quartile.(10,61]` + `currResidenceTenure_bucket_Quartile.(61,123]` + 
                  `currJobTenure_bucket_Quartile.(17,34]` + No_of_times_90_DPD_or_worse_in_last_6_months + No_of_times_30_DPD_or_worse_in_last_6_months + No_of_PL_trades_opened_in_last_6_months, data = train)
summary(model_14)
vif(model_14)

model_15 <- glm(formula = PerformanceTag ~ Income_woe + currResidenceTenure_woe + 
                  Nooftimes90DPDorworseinlast12months_woe + AvgasCCUtilizationinlast12months_woe + 
                  Nooftradesopenedinlast12months_woe + `Income_bucket_Quartile.(27,40]` + `Income_bucket_Quartile.(40,61]` + 
                  `currResidenceTenure_bucket_Quartile.(10,61]` + `currResidenceTenure_bucket_Quartile.(61,123]` + 
                  `currJobTenure_bucket_Quartile.(17,34]` + No_of_times_90_DPD_or_worse_in_last_6_months + No_of_times_30_DPD_or_worse_in_last_6_months, data = train)
summary(model_15)
vif(model_15)

model_16 <- glm(formula = PerformanceTag ~ currResidenceTenure_woe + 
                  Nooftimes90DPDorworseinlast12months_woe + AvgasCCUtilizationinlast12months_woe + 
                  Nooftradesopenedinlast12months_woe +  
                  `currResidenceTenure_bucket_Quartile.(10,61]` + `currResidenceTenure_bucket_Quartile.(61,123]` + 
                  `currJobTenure_bucket_Quartile.(17,34]` + No_of_times_90_DPD_or_worse_in_last_6_months + No_of_times_30_DPD_or_worse_in_last_6_months, data = train)
summary(model_16)
vif(model_16)

model_17 <- glm(formula = PerformanceTag ~ currResidenceTenure_woe + 
                  Nooftimes90DPDorworseinlast12months_woe + AvgasCCUtilizationinlast12months_woe + 
                  Nooftradesopenedinlast12months_woe +  
                  `currResidenceTenure_bucket_Quartile.(10,61]` + `currResidenceTenure_bucket_Quartile.(61,123]` + No_of_times_90_DPD_or_worse_in_last_6_months + No_of_times_30_DPD_or_worse_in_last_6_months, data = train)
summary(model_17)
vif(model_17)

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
# Let's choose a cutoff value of 0.04959596 for final model

test_cutoff <- factor(ifelse(test$test_predict >= 0.05, "1", "0"))

conf_final <- confusionMatrix(test_cutoff, test_actual, positive = "1")
acc <- conf_final$overall[1]
sens <- conf_final$byClass[1]
spec <- conf_final$byClass[2]

acc
# Accuracy 
# 0.6542627
sens
# Sensitivity 
# 0.5984163 
spec
# Specificity 
# 0.6567216 

######### Modelling using xgboost #########
set.seed(123)
split_indices <- sample.split(final_woe$PerformanceTag, SplitRatio = 0.7)
trainModel <- final_woe[split_indices,]
testModel <- final_woe[!split_indices,]

### Random Forests, can be used, in here, but takes lot of time, so, we will use Gradient Boosting,
### which produces same results, but faster
library(unbalanced)
trainModel <- as.data.frame(trainModel)
trainModel$PerformanceTag <- as.factor(trainModel$PerformanceTag)
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
colnames(trainBoostData)[n] <- "PerformanceTag"
summary(trainBoostData$PerformanceTag)
table(trainBoostData$PerformanceTag)

### Building Boosting tree Model ###
gbmfit <-
  caret::train(PerformanceTag ~ .,
               data = trainBoostData,
               method = "gbm",
               trControl = trainControl(
                 method = "repeatedcv",
                 number = 4,
                 repeats = 4,
                 verbose = FALSE
               ),
               verbose = FALSE)

### predicted probabilities for test data

test_pred = predict(gbmfit, type = "prob",
                    newdata = testModel[, -1])[, -1]
# Let's see the summary
summary(test_pred)

# Let's use the probability cutoff of 5%.
test_pred <- factor(ifelse(test_pred >= 0.1, "1", "0"))
test_actual <- factor(testModel$PerformanceTag)
testModel$PerformanceTag <- test_actual
test_actual <- factor(test_actual)
testModel$PerformanceTag <- factor(testModel$PerformanceTag)

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
cutoff ## 0.04414141

# Let's choose a cutoff value of 0.04414141 for final model
test_pred = predict(gbmfit, type = "prob",
                    newdata = testModel[, -1])[, -1]
test_cutoff <- factor(ifelse(test_pred > 0.049, "1", "0"))

conf_final <-
  confusionMatrix(test_cutoff, test_actual, positive = "1")
acc <- conf_final$overall[1]
sens <- conf_final$byClass[1]
spec <- conf_final$byClass[2]

acc
# Accuracy
# 0.6942894
sens
# Sensitivity
# 0.5033937
spec
# Specificity
# 0.7026946

##### Building a score Card ######
library(data.table)
library(scorecard)

# woe binning
dt_s = var_filter(final_woe, y="PerformanceTag")
bins = woebin(dt_s, y="PerformanceTag", print_step = 1)

# score
card = scorecard(bins, model_17, points0 = 400, odds0 = 1/10, pdo = 20)
# credit score
scoreCard = scorecard_ply(final_woe, card, print_step=0)
scoreCard <- cbind(ApplicationIDcredit, scoreCard)
scoreCard$PerformanceTag <- final_woe$PerformanceTag

View(scoreCard)
table(scoreCard$score, scoreCard$PerformanceTag)

