# Health-Care-Professional-Prescription-Prediction

Nishikant Sinha - June 2018

This assignnment is aimed to predict the likelihood that a Health Care Professional will prescribe the product of a particular Life Science Marketer based on the past prescription data.

### Problem Definition
#### Background
Lisa, who is a Life sciences marketer conducts Marketing initiatives to engage Health Care Professionals (HCPs) mainly Doctors, to influence them for writing Rx (prescription) of her Pharma Products (MyProd1 and MyProd2). Lisa has received the attached file (data.csv) as her Target List (TL). She has only limited marketing budget,  to help her maximise her returns she has approached you to help her find the high value doctors. It would greatly help her if she is able to segment the TL into 4 segments – Super High, High, Medium and Low value, on the basis of their liklihood of prescribing MyProd1 or MyProd2 in future.
Our Job is to Segment the TL into the 4 segments.

#### Data Set:
Attached data set (data.csv) has records for various HCP’s. The file has demographic data (age, gender, US state etc) and historical data of past Rx (Prescription that the doctor has written for MyProd1 , MyProd2 or competetitor products), ignore any variable with header ‘ignore’.

### EDA Summary
#### Objective : 
The objective of this EDA is to help a Life sciences marketer to get maximum return by segmenting his/her Target List(TL) which has information about the Health Care Professional (HCPs), based on the likelihood of prescribing their product MyProd1 or MyProd2 in future.

#### Approach :
The first and foremost step in any type of data science is to understand the problem definition and know given dataset thoroughly.  After understanding the problem definition completely, I started exploring the dataset which gave me many insights about the it’s features (columns).

With all the gathered information in the prior step I moved further for data cleansing and imputation. These all are the generic data pre-processing technique which I have done.
The main approach to solve this problem statement is to predict the values of MyProd1_Rx and MyProd2_Rx and then segment the TL into 4 segments referring to these two variables.

For predicting these 2 variables I have used Multiple 3 different techniques - 
1.	Linear Regression Model 
2.	Decision Trees Model
3.	Multivariate Imputation via Chained Equations (MICE) 
Among all these MICE gave the best result.

For segmenting there are 2 approach that can be used -
1.	Segment using equal proportion of people divided based on the quartile values of MyProd1_Rx and MyProd2_Rx.
2.	Segment solely on the values of MyProd1_Rx and MyProd2_Rx without focusing on equal distribution in each category.

The 2nd approach best fits in this scenario as it will allow the marketer to have more control on the segmentations, such that they can alter it according to their capacity to engage HCP and their budget for marketing.


