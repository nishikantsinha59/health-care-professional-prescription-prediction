getwd()
setwd('E://Data Science//indegene_assignment')
# setwd('C://nishikant//data science//indegene')

# Read the input file using read.csv
?View
ta <- read.csv("data.csv", na.strings=c("","NA"))

# View few records for confirmation
View(targetList)
# Great our input file has been read successfully

# Now lets begin the journey of analysis
# Check structure of loaded data
str(targetList)
# This simple function tells us a lot of things about our dataset like
# Loaded data set has total 31 features/columns/fields
# The total number of records in this dataset is 77247
# It also tell us about data type of each variable

# There are few variables which should be categorical but they have been read as continuous
# Convert those to factor

# If look carefully you will notice that there are columns havig keyword ignore 
# And in the instruction it has been mentioned not to cosider those columns
# Therefore Delete the those unwanted columns
#targetList <- targetList[,-c(26:31)]
targetList <- targetList[,-grep(pattern="Ignore*",colnames(targetList))]

# Here we have deleted columns containg ignore keyword 
# Alternatively we can delete based on column indices
# targetList <- targetList[,-c(26:31)]

# There are too many blank cells in PRESUMED_DEAD_FLAG column
# Check the count of such occurences
table(targetList$PRESUMED_DEAD_FLAG)
    # D 
    #77245     2 

# Bingo it's almost in every record except two
# Let's confirm this, first check the levels if this factor variable
levels(targetList$PRESUMED_DEAD_FLAG)
    #[1] ""  "D"

# So we have two levels and one is "", thus we need to replace this
levels(targetList$PRESUMED_DEAD_FLAG)[1] <- "Not specified"
levels(targetList$PRESUMED_DEAD_FLAG)
table(targetList$PRESUMED_DEAD_FLAG)
    # Not specified             D 
    #         77245             2

# Great we got to know that there are only two records which have values for PRESUMED_DEAD_FLAG
# View those 2 records
View(targetList[which(targetList$PRESUMED_DEAD_FLAG == 'D'),])


# If compare customer_id and id2 values both looks same, let's find out
tmp <- ifelse(targetList$customer_id == targetList$ID2,'Matched','Not_matched')
table(tmp)
  #Matched Not matched 
  #77244           3 

# Check the records whose value not matched
which(tmp == 'Not_matched')
  #[1] 22771 38453 59466

targetList$customer_id[22771]
# [1] 854317
targetList$ID2[22771]
# [1] 0

targetList$ID2[38453]
# [1] 0
targetList$customer_id[38453]
#[1] 1920276

targetList$customer_id[59466]
# [1] 3571677
targetList$ID2[59466]
# [1] 0
 
# As we can see here there is no difference between these two columns which makes it redundant
# Remove ID2 column
targetList$ID2 <- NULL



# Now check for the missing values in each column
# Apply sum(is.na()) to each variable to check missing values
print(sapply(targetList, function(x) sum(is.na(x))))

# There is similarity between different columns in terms of missing values
# Most of the important columns have same number of missing values
# So we can remove the records in which all the important features are missing

targetList <- targetList[-which(rowMeans(is.na(targetList[,c(1:13,20,24)])) == 1),]



############ Outliers Treatment ##########
contVar <- c(9,10,15,16,17,18,19,20,21,22,24)
catVar <- c(2:8,11:13,23)

boxplot.with.outlier.label(y, lab_y, ylim = c(-5,5))
boxplot(targetList$Age)
# Here few record is there for which the age is above 100
# On checking we got to know that the actual value of these 67 records is 116 which is possible case in reality

# plot for other variables
boxplot(targetList[,contVar])

# Check again for missing values
print(sapply(targetList, function(x) sum(is.na(x))))
# Still there is missing value in columns state, region, division and gender_code
# Which needs to be imputed



############ Missing value imputation #################
install.packages('mice')
library(mice)

# There are few variables which should be categorical but they have been read as continuous
# Convert those to factor
factor(x = character(), levels, labels = levels,
       exclude = NA, ordered = is.ordered(x), nmax = NA)

targetList$State <- factor(targetList$State)
targetList$Region <- factor(targetList$Region)
targetList$Division <- factor(targetList$Division)
targetList$Group_Name <- factor(targetList$Group_Name)
targetList$GENDER_CODE <- factor(targetList$GENDER_CODE)
targetList$MSA_Population_Size <- factor(targetList$MSA_Population_Size, ordered = F)

#Correlation values of attributes of data
install.packages('corrplot')
install.packages('caret')
library(corrplot)
library(caret)
?corrplot

# Plot
m<-cor(targetList[,contVar])
corrplot(m,method = 'circle')

# Find the variables having high correlation
highCorr <- findCorrelation(m, cutoff = .75)
colnames(m)[highCorr] 
# This suggest that we can remove YrsPractice variable


# For ML apllication all numeric variables must be brought into the same scale
#targetList[,contVar] <- scale(targetList[,contVar])

# Split the dataset in train and test
trainProd1 <-targetList[which(targetList$MyProd1_Rx != 0),]
testProd1 <-targetList[which(targetList$MyProd1_Rx == 0),] 

trainProd2 <-targetList[which(targetList$MyProd2_Rx != 0),] 
testProd2 <-targetList[which(targetList$MyProd2_Rx == 0),] 


########## Model Implementation ###########
# Stepwise Regression
library(MASS)
fit <- lm(y~x1+x2+x3,data=mydata)
step <- stepAIC(fit, direction="both")
step$anova # display results






##### Impute Region#####
#Check which records have missing value for region 
View(targetList[which(is.na(targetList$State)==TRUE),])

# Perform mice imputation, excluding certain less-than-useful variables:
mice_mod <- mice(full[, !names(full) %in% c('PassengerId','Name','Ticket','Cabin','Survived')],
                 method='rf') 



# Save the complete output 
mice_output <- complete(mice_mod)


######## Distribution ############
hist(targetList$CompProd1_Rx, breaks = 80, xlim = c(0,100))
hist(targetList$MyProd2_Rx, breaks = 100, xlim = c(0,2))

