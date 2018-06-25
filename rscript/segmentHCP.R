# Read the input file using read.csv
# Replace blank with NA
targetList <- read.csv("data.csv", na.strings=c("","NA"))

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


# If look carefully you will notice that there are columns havig keyword ignore 
# And in the instruction it has been mentioned not to cosider those columns
# Therefore Delete the those unwanted columns
#targetList <- targetList[,-c(26:31)]
targetList <- targetList[,-grep(pattern="Ignore*",colnames(targetList))]
# Here we have deleted columns containg ignore keyword 


# Remove the unwanted column which can produce noise and overfitting
# There are too many blank cells in PRESUMED_DEAD_FLAG column
# Only 2 records are having values for PRESUMED_DEAD_FLAG
# So we can safely remove this
targetList$PRESUMED_DEAD_FLAG <- NULL

# Also the variables like customer_id and ID 2 which has unique values for every record
# Can introduce overfitting so better to get rid of these variables
targetList$customer_id <- NULL
targetList$ID2 <- NULL

# Check other variables
# Here TOP, PE seems to have value 0 for most of the record
table(targetList$PE)
  #       0    11    30 
  #   77243     1     3 

table(targetList$TOP)
#        0    20    71 
#   77243     3     1

# So our doubt was right only 4 record have non zero values for PE and TOP
# Hence these two should be ignored
targetList$TOP <- NULL
targetList$PE <- NULL


# If you look closely there is also something fishy with SPECIALITY variable
# Let's dig deeper to find out the truth
table(targetList$SPECIAlITY)
  #   FM    IM Other 
  #   2     1 77244 
# Bingo this time also our intution was right as almost all its records contains
# value 'Other' except 3 observation, thus it won't be fare to consider this for
# model implementation
targetList$SPECIAlITY <- NULL


# Now check for the missing values in each column
# Apply sum(is.na()) to each variable to check missing values
print(sapply(targetList, function(x) sum(is.na(x))))

# Now if you notice there is similarity between different columns in terms of missing values
# Most of the important columns have same number of missing values
# So we can remove the records in which most of the features are missing
targetList <- targetList[-which(rowMeans(is.na(targetList[,c(1:11,18,19)])) == 1),]
# targetList <- targetList[-which((rowMeans(is.na(targetList[,c(1:11,18,19)])) == 1) 
#                                 & targetList$MyProd1_Rx == 0
#                                 & targetList$MyProd2_Rx == 0), ]

# Check again for missing values
print(sapply(targetList, function(x) sum(is.na(x))))
# Still there is missing value in columns state, region, division and gender_code
# Which needs to be imputed


############ Outliers Treatment ##########
# Separate continuos and categorical variable
contVar <- c(8,9,13:19)
catVar <- c(2:7,10:12)

par(mfrow = c(1,1))
boxplot(targetList[,contVar])
boxplot(targetList[,c(18,19)])
# Here few record is there for which the age is above 100
# On checking we got to know that the actual value of these 67 records is 116 which is possible case in reality
# There is no outliers in these two variables
# For other variables there is a lot of uncertainity in values so we will leave them for now


############ Missing value imputation #################
# Install and load the required packages
install.packages('mice')
install.packages('randomForest')
library(mice)
library(randomForest)

# Now before starting with imputation once check the structure of our cleaned dataset
str(targetList)
# It has few variables which should be factor but it is not
# We have to covert them into factor then we can proceed for imputation
targetList$State <- factor(targetList$State)
targetList$Region <- factor(targetList$Region)
targetList$Division <- factor(targetList$Division)
targetList$Group_Name <- factor(targetList$Group_Name)
targetList$GENDER_CODE <- factor(targetList$GENDER_CODE)
targetList$MSA_Population_Size <- factor(targetList$MSA_Population_Size)


# Imputing RX_Restriction_Indicator
# This variable has max number of missing values
# Moreover only one level of factor is given, then we can add a new level and replace NA with string 'Not Specified'
levels(targetList$RX_Restriction_Indicator) <- c(levels(targetList$RX_Restriction_Indicator),"Not Specified")
targetList$RX_Restriction_Indicator[is.na(targetList$RX_Restriction_Indicator)] <- "Not Specified"

# Perform mice imputation, excluding certain less-than-useful variables:
mice_mod <- mice(targetList[, !names(targetList) %in% c('ID','RX_Restriction_Indicator','MyProd1_Rx','Myprod2_Rx')], method='rf') 

# Save the complete output 
mice_output <- complete(mice_mod)

# Evaluate the imputed values by comparing there distribution
# For state variable
par(mfrow=c(1,2))
barplot(table(targetList$State))
barplot(table(mice_output$State))

# For region
barplot(table(targetList$Region))
barplot(table(mice_output$Region))

# For division
barplot(table(targetList$Division))
barplot(table(mice_output$Division))

# For Gender_code
barplot(table(targetList$GENDER_CODE))
barplot(table(mice_output$GENDER_CODE))

# Here these distrbution comparision that imputed values are good to go
# Thus we can replace the missing values with imputed one.
targetList$State[which(is.na(targetList$State)==TRUE)] <- mice_output$State[which(is.na(targetList$State)==TRUE)]
targetList$Region[which(is.na(targetList$Region)==TRUE)] <- mice_output$Region[which(is.na(targetList$Region)==TRUE)]
targetList$Division[which(is.na(targetList$Division)==TRUE)] <- mice_output$Division[which(is.na(targetList$Division)==TRUE)]
targetList$GENDER_CODE[which(is.na(targetList$GENDER_CODE)==TRUE)] <- mice_output$GENDER_CODE[which(is.na(targetList$GENDER_CODE)==TRUE)]

# Check again for missing values
print(sapply(targetList, function(x) sum(is.na(x))))
# Great now we can proceed to next section


#########Correlation values of attributes of data ##########
install.packages('corrplot')
install.packages('caret')
library(corrplot)
library(caret)

# Plot
m<-cor(targetList[,contVar])
corrplot(m,method = 'circle')

# Find the variables having high correlation
highCorr <- findCorrelation(m, cutoff = .75)
colnames(m)[highCorr]
      # [1] "YrsPractice"
# This suggest that there is multicollinearity 
# so we shouldn't consider YrsPractice variable for regression analysis

####### Scaling #######
# For ML apllication all numeric variables must be brought into the same scale
#targetList[,contVar] <- scale(targetList[,contVar])
targetListScaled <- targetList
targetListScaled[,c(8,9,15:19)] <- scale(targetListScaled[,c(8,9,15:19)])


########## Model Implementation ###########
# Split the dataset in train and test
trainProd1 <-targetListScaled[which(targetListScaled$MyProd1_Rx != 0),]
testProd1 <-targetListScaled[which(targetListScaled$MyProd1_Rx == 0),] 

trainProd2 <-targetListScaled[which(targetListScaled$MyProd2_Rx != 0),] 
testProd2 <-targetListScaled[which(targetListScaled$MyProd2_Rx == 0),] 

# Multiple linear regression model
lm_model <- lm(MyProd1_Rx ~ ., data=trainProd1[,-c(1,2,10,11,12,19)])
summary(lm_model)
anova(lm_model)

# Function that returns Root Mean Squared Error
rmse <- function(error)
{
  return(sqrt(mean(error^2)))
}

fit_rmse <- rmse(lm_model$residuals)
# [1] 0.1622034

# Predict the test dataset values for MyProd_Rx1 using the model
lm_predTest <- predict(lm_model,testProd1[,-c(1,2,10,11,12,19)])

# Do the same steps for MyProd_Rx2
lm_model2 <- lm(MyProd2_Rx ~ ., data=trainProd2[,-c(1,2,10,11,12,19)])
summary(lm_model2)
anova(lm_model2)

fit_rmse <- rmse(lm_model2$residuals)
# [1] 0.1671408

# Predict the test dataset for MyProd_Rx2 values using the model
lm_predTest2 <- predict(lm_model2,testProd2[,-c(1,2,10,11,12,19)])



# Decision Trees model
# Set a random seed
set.seed(129)

# Build rpart model 
library(rpart)
dt_model <- rpart(MyProd1_Rx ~ .,data = trainProd1[,-c(1)], method = "anova") 

# Use the rpart model to predict the missing age values
dt_predTest <- predict(dt_model, testProd1[,-c(1)])

# Repeat the same steps for MyProd_Rx2
dt_model2 <- rpart(MyProd2_Rx ~ .,data = trainProd2[,-c(1)], method = "anova") 

# Use the rpart model to predict the missing age values
dt_predTest2 <- predict(dt_model2, testProd2[,-c(1)])



############# Predicting values of MyProd1_Rx and MyProd2_Rx Using MICE #############
# First value 0 with NAs
targetList$MyProd1_Rx[which(targetList$MyProd1_Rx == 0)] <- NA
targetList$MyProd2_Rx[which(targetList$MyProd2_Rx == 0)] <- NA

# Perform mice imputation, excluding certain less-than-useful variables:
mice_final <- mice(targetList[, !names(targetList) %in% c('ID','RX_Restriction_Indicator')], method='rf', maxit = 3) 

# Save the complete output 
mice_output_final <- complete(mice_final)


# Comparing results of Linear Regression ,Decesion Trees and MICE
par(mfrow = c(1,4))

# For MyProd1_Rx
hist(targetList$MyProd1_Rx,breaks = 100, xlab = "Initial", main = "")
hist(mice_output_final$MyProd1_Rx,breaks = 100, xlab = "Mice",main = "")
hist(rbind(trainProd1$MyProd1_Rx,lm_predTest),breaks = 100, xlab = "Linear Regression", main = "")
hist(rbind(trainProd1$MyProd1_Rx,dt_predTest),breaks = 100, xlab = "Decision Trees",main = "")
# On these three plots it is clear that MICE gives more similar distribution for MyProd_Rx1 values then Linear Regression and Decision Trees

# For MyProd2_Rx
hist(targetList$MyProd2_Rx,breaks = 100, xlab = "Initial", main = "")
hist(mice_output_final$MyProd2_Rx,breaks = 100, xlab = "Mice",main = "")
hist(rbind(trainProd1$MyProd2_Rx,lm_predTest2),breaks = 100, xlab = "Linear Regression", main = "")
hist(rbind(trainProd1$MyProd2_Rx,dt_predTest2),breaks = 100, xlab = "Decision Trees",main = "")
# Interesting there is a significant difference between initial value distribution
# and the values predicted by Linear regression and Decision Trees
# but MICE values distribution is pretty much similar to initial one
# Thus for both MyProd1_Rx and MyProd2_Rx, we replace NA with MICE predicted values


# Replace the missing values with predicted values
targetList$MyProd1_Rx[which(is.na(targetList$MyProd1_Rx)==TRUE)] <- mice_output_final$MyProd1_Rx[which(is.na(targetList$MyProd1_Rx)==TRUE)]
targetList$MyProd2_Rx[which(is.na(targetList$MyProd2_Rx)==TRUE)] <- mice_output_final$MyProd2_Rx[which(is.na(targetList$MyProd2_Rx)==TRUE)]


# Now the dataset is complete we just need segment it into four categories 
# i.e Super High, High, Medium, Low
# There are two approach which we can follow
# 1) Categories based on quartile of MyProd1_Rx and MyProd2_Rx, 
    #so each category will have equal number of people
# 2) Categories solely on the value of MyProd1_Rx and MyProd2_Rx, 
    #for example people with value higher than 1.7 will categorize as Super High value

# Here for this scenario the second approach is best suited, 
# since our client is having limited budget for marketing

# Start categorizing based on the value of both the products
# First create the new column
targetList$value <- NA

# Replace data based on the values in more than one field
targetList$value[targetList$MyProd1_Rx >= 1.734173 
                       | targetList$MyProd2_Rx >= 1.8465
                       & is.na(targetList$value)] <- "Super High"

targetList$value[((targetList$MyProd1_Rx >= 1.156116 & targetList$MyProd1_Rx < 1.734173) 
                 | (targetList$MyProd2_Rx >= 1.231 & targetList$MyProd2_Rx < 1.8465))
                 & is.na(targetList$value)] <- "High"

targetList$value[((targetList$MyProd1_Rx >= 0.5780578 & targetList$MyProd1_Rx < 1.156116) 
                 | (targetList$MyProd2_Rx >= 0.6155 & targetList$MyProd2_Rx < 1.231))
                 & is.na(targetList$value)] <- "Medium"

targetList$value[(targetList$MyProd1_Rx < 0.5780578 
                       | targetList$MyProd2_Rx < 0.6155)
                       & is.na(targetList$value)] <- "Low"

# Convert this variable to factor
targetList$value <- factor(targetList$value)

# Check the number of health care professional(HCP) in each category
table(targetList$value)
    #     High        Low     Medium Super High 
    #     120      33750       1057         50
# Here the number of HCP is very less in Super High, High and medium as compared to low
# This can be changed according to the capacity of marketer to engage HCP

# Creating Separate data frame for each category
superHighValue_HCP <- targetList[targetList$value == "Super High",]
HighValue_HCP <- targetList[targetList$value == "High",]
mediumValue_HCP <- targetList[targetList$value == "Medium",]
lowValue_HCP <- targetList[targetList$value == "Low",]

# Write the final dataset as csv file
write.csv(targetList, file = "completeData.csv",row.names=T)

