# Decision-Tree
a decision tree for predicting Breast-cancer risk
# use the library
library(readr)
# Import dataset
breast_cancer <- read_csv("C:/Users/tnnha/Documents/Swinburne/Year 3/Sem 1/Business Analytics/ASM 3 breast cancer/new-breast-cancer.csv")

# View the dataset
View(breast_cancer)
# Shows that dataset has 286 records and 10 columns
dim(breast_cancer) 

# view the class for all columns
class(breast_cancer$Class)
class(breast_cancer$Age)
class(breast_cancer$Menopause)
class(breast_cancer$`Tumor-size`)
class(breast_cancer$`Inv-nodes`)
class(breast_cancer$`Node-caps`)
class(breast_cancer$`Deg-malig`)
class(breast_cancer$Breast)
class(breast_cancer$`Breast-quad`)
class(breast_cancer$Irradiat)

# convert dataset to data frame
 as.data.frame(breast_cancer)

# view the dataset summary to see if there's any missing values
summary(breast_cancer)

# result: there's no NAs





# MISSING VALUES 
#==============================================
# it's mentioned that missing values is denoted with "?"
# it is also mentioned that these missing values only exist in column 6 and 9

# find record with '?' in the 6th column
which(breast_cancer[,6] == "?")
# result shows missing values is located at rows 146, 164, 165, 184, 185, 234, 264, 265 for column 6 (node-caps)

# find records with '?' in the 9th column
which(breast_cancer[,9] == "?")
# result shows that there is only one missing values and it is located at row 207 for clumn 9 (Breast-quad)


# DUPLICATE VALUES
#=============================================
# check if there's any duplication
duplicated(breast_cancer)

# to know which row is duplicated
which(duplicated(breast_cancer))
# it is shown that row 22, 79, 92, 98, 112, 113, 114, 121, 143, 172, 208, 238, 269 and 281 are duplicates

# For now, no action will be done on the missing values and duplicate values yet


# View and see OUTLIERS
#=======================
# Boxplot on the numeric column to see if there is any outliers
# In this dataset, there is only one numeric column which is 'deg-malig'
boxplot(breast_cancer$`Deg-malig`,
        main = "Breast Cancer's Degree of Malignancy",
        xlab = "Degree of Malignancy Value",
        ylab = "Degree of Malignancy",
        col = "#d494bb",
        border = "#33444a",
        horizontal = TRUE,
        notch = FALSE
)
# the boxplot shows that there is no outliers



# DATA VISUALISATION
#=====================
# Data visualisation is created on the columns to see patterns/value distribution and significant relationship 
# that may exist between columns

recurrance_column <- table(breast_cancer$Class)
barplot(recurrance_column, main = "Cancer Recurrance Cases for breast cancer patients", xlab = "Cancer recurrance", col="#c7b5c1")
#'no-recurrance-events' is higher than recurrence-events

age_column <- table(breast_cancer$Age)
barplot(age_column, main = "Group of ages for breast cancer patients", xlab = "Age Group", col="#c7b5c1")
#barplot shows most sufferers are from age group (50-59), followrd by (40-49) and the least is from (20-29)

menopause_column <- table(breast_cancer$Menopause)
barplot(menopause_column, main = "Menopause condition for breast cancer patients", xlab = "Menopause condition", col="#c7b5c1")
#barplot shows 'premeno' is the highest, followed closely by 'ge40' and a little portion is from 'lt40'

tumorsize_column <- table(breast_cancer$`Tumor-size`)
barplot(tumorsize_column, main = "Tumor Size for breast cancer patients", xlab = "Tumor Size", col="#c7b5c1")

invnode_column <- table(breast_cancer$`Inv-nodes`)
barplot(invnode_column, main = "Inv-nodes Group for breast cancer patients", xlab = "Inv-node Group", col="#c7b5c1")
#'group 0-2' is the highest.The least is group 24-26

nodecaps_column <- table(breast_cancer$`Node-caps`)
barplot(nodecaps_column, main = "Node-caps Group for breast cancer patients", xlab = "Node-caps Group", col="#c7b5c1")
#'No' has the highest amount oif sufferer, followed by 'yes' and there are issing values

breast_column <- table(breast_cancer$Breast)
barplot(breast_column, main = "Breast psotition for breast cancer patients", xlab = "Breast position", col="#c7b5c1")
#'left breast' is higher than 'right breast'

breastquad_column <- table(breast_cancer$`Breast-quad`)
barplot(breastquad_column, main = "Breast-quad posotition for breast cancer patients", xlab = "Breast-quad position", col="#c7b5c1")
#'left low' is the highest, followed by 'left low'. There is one missing value

irradiat_column <- table(breast_cancer$Irradiat)
barplot(irradiat_column, main = "Irradiation for breast cancer patients", xlab = "Irradiation", col="#c7b5c1")
#'no' is highest than 'yes'


# Cross Tab for recurrence and age
#======================================
recurrence_age <- table(breast_cancer$Class, breast_cancer$Age)
recurrence_age

margin.table(recurrence_age, 1) #to know frequency for recurrence events
margin.table(recurrence_age, 2) #to know frequency for age

chisq.test(recurrence_age)
#result: p-value is 0.5497 which is >0.05. This means they are not related. Not significant



# Cross Tab for recurrence and tumor size
#==============================================
recurrence_tumorsize <- table(breast_cancer$Class, breast_cancer$`Tumor-size`)
recurrence_tumorsize

margin.table(inv_tumorsize, 2) #to know frequency for each in tumor-size

chisq.test(recurrence_tumorsize)
#result shows:
#p-value is 0.0564, which is >0.05 . Not significant


# Cross tab for recurrence and node-caps
#==================================================
recurrence_nodecaps <- table(breast_cancer$Class, breast_cancer$`Node-caps`)
recurrence_nodecaps

margin.table(recurrence_nodecaps, 2) #to know frequency for each node-caps

chisq.test(recurrence_nodecaps)
#result shows:
#p-value is 0.00000118 it is <.05 . This is significant



# Cross tab for recurrence and menopause
#======================================================

recurrence_menopause <- table(breast_cancer$Class, breast_cancer$Menopause)
recurrence_menopause

margin.table(recurrence_menopause, 2) #to know frequency for each in inv-nodes

chisq.test(recurrence_menopause)
#result: p-value is 0.6732 which is >.05. Thus, the relationship is not significant



# Cross tab for recurrence and deg-malig 
#=======================================================

recurrence_degmalig <- table(breast_cancer$Class, breast_cancer$`Deg-malig`)
recurrence_degmalig

margin.table(recurrence_degmalig, 2) #to know frequency for each in inv-nodes

chisq.test(recurrence_degmalig)
#result: p-value is 0.0000001311 which is <.05. Thus, the relationship is significant



# Cross tab for recurrence and inv-nodes 
#======================================================

recurrence_invnode <- table(breast_cancer$Class, breast_cancer$`Inv-nodes`)
recurrence_invnode

margin.table(recurrence_invnode, 2) #to know frequency for each in inv-nodes

chisq.test(recurrence_invnode)
#result: p-value is 0.00006638 which is <.05. Thus, the relationship is significant



# Cross tab for recurrence and breast
#===================================================

recurrence_breast <- table(breast_cancer$Class, breast_cancer$Breast)
recurrence_breast

margin.table(recurrence_invnode, 2) #to know frequency for each in inv-nodes

chisq.test(recurrence_breast)
#result: p-value is 0.3886 which is >.05. Thus, the relationship is not significant



# Cross tab for recurrence and breast-quad 
#===========================================================

recurrence_breastquad <- table(breast_cancer$Class, breast_cancer$`Breast-quad`)
recurrence_breastquad

margin.table(recurrence_breastquad, 2) #to know frequency for each in inv-nodes

chisq.test(recurrence_breastquad)
#result: p-value is 0.319 which is >.05. Thus, the relationship is not significant


# Cross tab for recurrence and irradiat 
#=============================================================

recurrence_irradiat <- table(breast_cancer$Class, breast_cancer$Irradiat)
recurrence_irradiat

margin.table(recurrence_irradiat, 2) #to know frequency for each in inv-nodes

chisq.test(recurrence_irradiat)
#result: p-value is 0.001764 which is <.05. Thus, the relationship is significant









#=================================================================================================================
#=================================================================================================================
#     2)Data Preparation
#=================================================================================================================
#=================================================================================================================


# HANDLING MISSING VALUES
#========================
# Earlier, missing values have been found in column:
# Node-caps = 8 missing values
# Breast-quad = 1 missing value

# for Node-caps, the value will be imputed based on the tumor-size (because the bigger the tumor size- higher chance for node-caps to happen)
# for records with tumor size 0-4 until 25-29, the node-caps will be imputed with 'no'
# for records tumor size 30-34 until 50-54, the node-caps will be imputed with 'yes'
# missing values at 146 164 165 184 185 234 264 265 have been imputed below:

breast_cancer[146,]$`Node-caps`<- "no"
breast_cancer[164,]$`Node-caps`<- "no"
breast_cancer[165,]$`Node-caps`<- "no"
breast_cancer[184,]$`Node-caps`<- "yes"
breast_cancer[185,]$`Node-caps`<- "yes"
breast_cancer[234,]$`Node-caps`<- "no"
breast_cancer[264,]$`Node-caps`<- "no"
breast_cancer[265,]$`Node-caps`<- "no"

# check again if all the missing values have been replaced
which(breast_cancer[,6] == "?")

# For Breast-quad, the row that contain the missing value will just be deleted because
# it is only one row so it will not affect the dataset (also it is still below 5% of the overall dataset)

# remove the row at 207 because it's the one that has missing value for Breast quad column
breast_cancer <- breast_cancer[-c(207),] 

# check again if all the missing values have been replaced
which(breast_cancer[,9] == "?")


# HANDLING DUPLICATE VALUES
#===========================
#DUPLICATE VALUES will not be removed.
#because it is possible for different people to have the same attribiutes as the unique identification has not been provided.


# HANDLING OUTLIERS
#===================
#There is no outliers detected in the dataset


#HANDLING ERRONEOUS DATA
#=========================
#Delete 'Breast-quad column' because it is found that most of the values imputed is wrong
breast_cancer <- breast_cancer[,-9]

#duplicate the data if in case something goes wrong
breast_cancer2 <- breast_cancer


#CONVERT CATEGORICAL VARIABLE TO NUMERIC (For the columns that highly influence the Class column)
#==================================================================================================


#-------------------
# i) Class column
#-------------------

#if no recurrence, will be imputed with 1
for (i in 1:nrow(breast_cancer))
{
  if(breast_cancer$Class[i] == "no-recurrence-events")
  {
    breast_cancer$Class[i] = 1;
  }
  
}

#if there is recurence events, will be imputed with 2
for (i in 1:nrow(breast_cancer))
{
  if(breast_cancer$Class[i] == "recurrence-events")
  {
    breast_cancer$Class[i] = 2;
  }
  
}



#-----------------
# ii) Node caps
#-----------------

#if no node caps, will be imputed with 0
for (i in 1:nrow(breast_cancer))
{
  if(breast_cancer$`Node-caps`[i] == "no")
  {
    breast_cancer$`Node-caps`[i] = 0;
  }
  
}

#if there is node-caps, will be imputed with 1
for (i in 1:nrow(breast_cancer))
{
  if(breast_cancer$`Node-caps`[i] == "yes")
  {
    breast_cancer$`Node-caps`[i] = 1;
  }
  
}



#----------------
#iii) Inv-node
#----------------

#if inv-node = 0-2, will be imputed with 1
for (i in 1:nrow(breast_cancer))
{
  if(breast_cancer$`Inv-nodes`[i] == "0-2")
  {
    breast_cancer$`Inv-nodes`[i] = 1;
  }
}

#if inv-node = 3-5, will be imputed with 2
for (i in 1:nrow(breast_cancer))
{
  if(breast_cancer$`Inv-nodes`[i] == "3-5")
  {
    breast_cancer$`Inv-nodes`[i] = 2;
  }
}

#if inv-node = 6-8, will be imputed with 3
for (i in 1:nrow(breast_cancer))
{
  if(breast_cancer$`Inv-nodes`[i] == "6-8")
  {
    breast_cancer$`Inv-nodes`[i] = 3;
  }
}

#if inv-node = 9-11, will be imputed with 4
for (i in 1:nrow(breast_cancer))
{
  if(breast_cancer$`Inv-nodes`[i] == "9-11")
  {
    breast_cancer$`Inv-nodes`[i] = 4;
  }
}

#if inv-node = 12-14, will be imputed with 5
for (i in 1:nrow(breast_cancer))
{
  if(breast_cancer$`Inv-nodes`[i] == "12-14")
  {
    breast_cancer$`Inv-nodes`[i] = 5;
  }
}

#if inv-node = 15-17, will be imputed with 6
for (i in 1:nrow(breast_cancer))
{
  if(breast_cancer$`Inv-nodes`[i] == "15-17")
  {
    breast_cancer$`Inv-nodes`[i] = 6;
  }
}

#if inv-node = 24-26, will be imputed with 7
for (i in 1:nrow(breast_cancer))
{
  if(breast_cancer$`Inv-nodes`[i] == "24-26")
  {
    breast_cancer$`Inv-nodes`[i] = 7;
  }
}


#-------------------
# iiii) Irradiat
#-------------------

#if no irradiat, will be imputed with 0
for (i in 1:nrow(breast_cancer))
{
  if(breast_cancer$Irradiat[i] == "no")
  {
    breast_cancer$Irradiat[i] = 0;
  }
  
}

#if there is node-caps, will be imputed with 1
for (i in 1:nrow(breast_cancer))
{
  if(breast_cancer$Irradiat[i] == "yes")
  {
    breast_cancer$Irradiat[i] = 1;
  }
  
}




#CREATE NEW DATASET WITH RELEVANT VARIABLES ONLY
#==================================================


# Convert the relevant columns to numeric
breast_cancer$Class <- as.factor(breast_cancer$Class)
breast_cancer$`Inv-nodes`<- as.numeric(breast_cancer$`Inv-nodes`)
breast_cancer$`Node-caps`<- as.numeric(breast_cancer$`Node-caps`)
breast_cancer$Irradiat <- as.numeric(breast_cancer$Irradiat)


# create new dataset with relevant column only
library(dplyr)
breast_cancer_modelData <- select(breast_cancer, -c(Age, Menopause, `Tumor-size`, Breast))

#view the summary and dimension of the new dataset
summary(breast_cancer_modelData)
dim(breast_cancer_modelData)
# dimension shows that there are 285 rows and 5 columns

#just duplicate the dataset in case something goes wrong
breast_cancer_modelData2 <-breast_cancer_modelData






#============================================================================================================
#============================================================================================================
# 3)Data Sampling
#============================================================================================================
#=============================================================================================================

# randomly choose 70% of the data set as training data 
# 30% as a test dataset (70/30 ratio)

set.seed(777)
#Why do you need to set seed?
#set.seed(n) - is used to generate the same random sample everytime.
#can put any numbers as 'n'


#70% of our rows will be assigned to the new dataset
#here, we WILL assign it to the train index first. index here is like a count (count of rows)
train.index <- sample(1:nrow(breast_cancer_modelData), 0.7*nrow(breast_cancer_modelData))

#assign the Breast Cancer index to the new train dataset (breast_cancer_modelData.train)
breast_cancer_modelData.train <- breast_cancer_modelData[train.index,]

#now we can see that the dimension of this new dataset only has 199 rows compared to
#the previous dataset that has 285 rows. (70% of previous)
dim(breast_cancer_modelData.train)
summary(breast_cancer_modelData.train)

## select the remaining 30% as the testing data
breast_cancer_modelData.test <- breast_cancer_modelData[-train.index,] 
#assign the above code to new dataset for testing
dim(breast_cancer_modelData.test) #dimension shows it only has 86 rows(30% from the original dataset)
summary(breast_cancer_modelData.test)


# Create normalize dataset for KNN ***THIS IS ONLY FOR KNN- CAN IGNORE THIS
#=======================================
# for kNN, we need to normalize the data first. This is the function to normalize the data
normalize_data <- function(x){
  return ((x - min(x)) / (max(x) - min(x)))
}

# we will just normalize the columns that we will use to predict the target column
# that's why we will not use the first column
breastcancer_normal <- as.data.frame(lapply(breast_cancer_modelData[,2:5], normalize_data)) 

# view the normalize data dimension and summary
dim(breastcancer_normal)
summary(breastcancer_normal)

# extract to training dataset
breastcancer.knn.train <- breastcancer_normal[train.index,]
# view the dimension
dim(breastcancer.knn.train)

# extract to testing dataset
breastcancer.knn.test <- breastcancer_normal[-train.index,]
# view the dimension
dim(breastcancer.knn.test)











#======================================================================================================================
#======================================================================================================================
# 4) Building the Model
#=======================================================================================================================
#=======================================================================================================================

# Load package for evaluations
library(caret)
library(ggplot2)
library(lattice)


#------------------------------------------------
#   i) SVM
#------------------------------------------------




#------------------------------------------------------
#   ii) K-nearest Neighbours Model
#------------------------------------------------------

# create seperate dataframe for 'Class' which is our target result column. (**In R: dataset[row,column])
# this will be used as 'cl' argument in knn function
train.label_breast <- breast_cancer_modelData[train.index,1]
test.label_breast <- breast_cancer_modelData[-train.index,1]

#dim(train.label_breast)
#summary(train.label_breast)
#dim(breastcancer.knn.train)
#summary(breastcancer.knn.train)

# install package for kNN
install.packages("class")

# use the package
library(class)

# usually the most ideal value for 'k' is the square root of the total rows (285)
# square root of 285 = 16.88. So we will run model with k=16 and k=17
# Here, run kNN function using the normalized train dataset
knn.16 <- knn(breastcancer.knn.train, breastcancer.knn.test, cl=train.label_breast, k=16) 
knn.17 <- knn(breastcancer.knn.train, breastcancer.knn.test, cl=train.label_breast, k=17) 


# evaluate the accuracy for k=16 and k=17
# evaluate the accuracy with test data
# to check prediction against test value (table form)

accuracytable.16 <- 100 * sum(test.label_breast == knn.16)/86
accuracytable.16
#accuracy : 68.60%

accuracytable.17 <- 100 * sum(test.label_breast == knn.17)/86
accuracytable.17
#accuracy : 68.60%

#since the accuracy for both k=16 and k=17 are the same. k=16 will be picked.

# ACCURACY
#=====================
#another way to evaluate the accuracy
#using ocnfusion matrix from 'caret' package

confusionMatrix(table(knn.16, test.label_breast))
#accuracy: 68.60%

## Extract the test data species to build the confusion matrix
knn.confusion <- table(knn.16, breast_cancer_modelData.test$Class)
print(knn.confusion)


# PRECISION
#=======================
# Precision per class
knn.precision.A <- knn.confusion[1,1] / sum(knn.confusion[,1])
print(knn.precision.A)

knn.precision.B <- knn.confusion[2,2] / sum(knn.confusion[,2])
print(knn.precision.B)


# Overall precision
knn.overall.precision<-(knn.precision.A+knn.precision.B/2)
print(knn.overall.precision)

# Result : Precision is 1


# RECALL 
#=======================
# Recall per class

confusionMatrix(table(knn.16, test.label_breast))
# from this confusionmatrix, it is seen that the recall or sensitivity is 1

# F1
#========================
knn.f1 <- 2 * overall.precision * 1 / (overall.precision + 1)
print(knn.f1)

# Result: F1 is 0.9792




#-----------------------------------------------------------
#   iii) Decision Tree
#-----------------------------------------------------------

# Load the library for Decision Tree
 library(rpart)

# Duplicate data model that will be used for the Decision Tree
  tree_model <- breast_cancer_modelData
  tree_train <- breast_cancer_modelData.train
  tree_test <-  breast_cancer_modelData.test
  
  #dim(tree_test)

# Default decision tree model
  # Builds a decision tree from the breast_cancer dataset to predict Class given all other columns as predictors
  tree_model.tree <- rpart(Class~.,data = tree_train)
# Reports the model
  print(tree_model.tree)
## VISUALIZE THE MODEL
  ## plot the tree structure
  plot(tree_model.tree, margin=c(.1))
  title(main = "Decision Tree Model of Breast Cancer Data")
  text(tree_model.tree, use.n = TRUE)
  ## print the tree structure
  summary(tree_model.tree)

  ## MODEL EVALUATION
  ## make prediction using decision model
  tree_model.predictions <- predict(tree_model.tree, tree_test, type = "class")
  head(tree_model.predictions)
  
  ## Comparison table
  tree_model.comparison <- tree_test
  tree_model.comparison$Predictions <- tree_model.predictions
  tree_model.comparison[ , c("Class", "Predictions")]
  
  ## View misclassified rows
  disagreement.index <- tree_model.comparison$Class != tree_model.comparison$Predictions
  tree_model.comparison[disagreement.index,]
  
  ## If instead you wanted probabilities.
  #tree_model.predictions <- predict(tree_model.tree, tree_test, type = "prob")
  
  ## Extract the test data specie to build the confusion matrix
  tree_model.confusion <- table(tree_model.predictions, tree_test$Class)
  print(tree_model.confusion)
  
  ## calculate accuracy, precision, recall, F1
  
  #Accuracy
  tree_model.accuracy <- sum(diag(tree_model.confusion)) / sum(tree_model.confusion)
  print(tree_model.accuracy)
  
  #Precision per class
  tree_model.precision.A <- tree_model.confusion[1,1] / sum(tree_model.confusion[,1])
  print(tree_model.precision.A)
  
  tree_model.precision.B <- tree_model.confusion[2,2] / sum(tree_model.confusion[,2])
  print(tree_model.precision.B)
  
  #Overall precision
  tree_overall.precision<-(tree_model.precision.A+tree_model.precision.B)/2
  print(tree_overall.precision)
  
  #Recall per class
  tree_model.recall.A <- tree_model.confusion[1,1] / sum(tree_model.confusion[1,])
  print(tree_model.recall.A)
  
  tree_model.recall.B <- tree_model.confusion[2,2] / sum(tree_model.confusion[2,])
  print(tree_model.recall.B)

  # Overall recall
  tree_overall.recall<-(tree_model.recall.A+tree_model.recall.B)/2
  print(tree_overall.recall)
  
  # F1
  tree_model.f1 <- 2 * ((tree_overall.precision * tree_overall.recall) / (tree_overall.precision + tree_overall.recall))
  print(tree_model.f1)

#------------------------------------------------------------
#iv) Random Forest 
#------------------------------------------------------------




#-------------------------------------------------------------
#v) Artificial Neural Network (aNN)
#-------------------------------------------------------------

#install packages
install.packages("quantmod")
install.packages("neuralnet")

#Load the R packages
library(quantmod)
library(neuralnet)

#duplicate the model data to new variable that will be used for aNN
ann_modeldata <- breast_cancer_modelData
ann_traindata <- breast_cancer_modelData.train
ann_testdata <- breast_cancer_modelData.test

# act.fct = "logistic" : is used to smoothen the result
# hidden=3 : represents single layer with 3 neurons respectively
# linear.output=FALSE : if apply act.fct, usually it set FALSE otherwise TRUE
# fit neural network to the dataset
ann_breastcancer <- neuralnet(ann_traindata$Class~ann_traindata$`Inv-nodes`+ann_traindata$`Node-caps`+ann_traindata$`Deg-malig`+ann_traindata$Irradiat, data=ann_traindata, hidden=6, act.fct="logistic", linear.output = FALSE)

# plot the neural net model
plot(ann_breastcancer)

# Evaluate the model
confusionMatrix(table(knn.16, test.label_breast))


####################################################################################################
# Prediction using neural network
# will predict the probability score of the test data
predict_ann <- compute(breastcancer_nn,ann_testdata)

# Convert the probabilities into binary classes setting threshold level 0.5
prob <- predict_ann$net.result
predict_ann <- ifelse(prob>0.5, 1, 0)
####################################################################################################
predict_ann



Predict$net.result
